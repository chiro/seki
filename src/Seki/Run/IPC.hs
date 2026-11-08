{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.IPC (withIPC, withIPCAt, ipcPath, CommandResult (..), submitCommand) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM
import Control.DeepSeq (force)
import Control.Exception (IOException, bracket, bracketOnError, evaluate, finally, mask, onException, try)
import Control.Monad (forever, unless, when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Foreign.C.Error (Errno (..), eCONNREFUSED)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Exception (IOException (..))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Internal.Core.Action qualified as Action
import Seki.Internal.Core.Layout qualified as Layout
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Run.External
import Seki.Run.IPCSchema
import Seki.Run.Policy (ExternalInput (..), Policy, observePolicy, policyBuildStatus, policyCanBuild, policyIngress, policyScratchpadNames, policyTagsOf)
import Seki.Run.Snapshot (projectSnapshot)
import System.Directory (removeFile)
import System.Environment (getEnv)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Posix.Directory qualified as Posix
import System.Posix.Files
import System.Posix.IO (FdOption (CloseOnExec), setFdOption)
import System.Posix.Process (getProcessID)
import System.Posix.Types (Fd (..))
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)

ipcPath :: IO FilePath
ipcPath = do
  root <- getEnv "XDG_RUNTIME_DIR"
  unless (isAbsolute root) (fail "XDG_RUNTIME_DIR must be absolute")
  pure (root </> "seki" </> "ipc.sock")

{- | The Linux abstract guard serializes pathname inspection, stale replacement,
and cleanup, including two servers in the same process.
-}
withEndpoint :: FilePath -> (Socket -> IO a) -> IO a
withEndpoint path body = do
  let dir = takeDirectory path
  uid <- getEffectiveUserID
  made <- try @IOException (Posix.createDirectory dir 0o700)
  case made of
    Left e | not (isAlreadyExistsError e) -> ioError e
    _ -> pure ()
  status <- getSymbolicLinkStatus dir
  unless (isDirectory status && fileOwner status == uid) (fail "unsafe IPC directory")
  unless (fileMode status `intersectFileModes` 0o077 == 0) (fail "IPC directory must have mode 0700")
  bracket (ownedSocket) close $ \guard -> do
    bind guard (SockAddrUnix ('\0' : path))
    existing <- try @IOException (getSymbolicLinkStatus path)
    case existing of
      Left e | isDoesNotExistError e -> pure ()
      Left e -> ioError e
      Right old -> do
        unless (isSocket old && fileOwner old == uid) (fail "unsafe IPC endpoint")
        alive <- bracket (ownedSocket) close $ \probe ->
          timeout 1000000 (try @IOException (connect probe (SockAddrUnix path)))
        case alive of
          Just (Left e) | isDoesNotExistError e -> pure ()
          Just (Left e) | ioeRefused e -> removeFile path
          _ -> fail "IPC endpoint is active or cannot be checked"
    bracket (ownedSocket) close $ \listener -> do
      bind listener (SockAddrUnix path)
      owned <- getSymbolicLinkStatus path
      let cleanup = do
            now <- try @IOException (getSymbolicLinkStatus path)
            case now of
              Right s | fileID s == fileID owned && deviceID s == deviceID owned -> removeFile path
              _ -> pure ()
      (setFileMode path 0o600 >> listen listener 32 >> body listener) `finally` cleanup
 where
  ioeRefused e = fmap Errno (ioe_errno e) == Just eCONNREFUSED

withIPC :: (Layout.LayoutClass l) => (String -> IO ()) -> Policy String l -> IO a -> IO a
withIPC logLine policy body = do
  entered <- newIORef False
  attempt <- try @IOException (ipcPath >>= \path -> withIPCAt path policy (writeIORef entered True >> body))
  case attempt of
    Right value -> pure value
    Left setupError -> do
      started <- readIORef entered
      if started
        then ioError setupError
        else logLine ("IPC unavailable: " <> show setupError <> "; continuing without IPC") >> body

withIPCAt :: (Layout.LayoutClass l) => FilePath -> Policy String l -> IO a -> IO a
withIPCAt path policy body =
  withEndpoint path $ \listener -> mask $ \restore -> do
    pid <- getProcessID
    tick <- getMonotonicTimeNSec
    let session = Text.pack (show pid <> "-" <> show tick)
    latest <- newTVarIO Nothing
    clients <- newMVar Map.empty
    observePolicy policy $ \revision st runtime -> do
      build <- policyBuildStatus policy
      let base = projectSnapshot session Text.pack (policyTagsOf policy) revision st runtime
          combined = case base of Object fields -> Object (KeyMap.insert "build" (toJSON build) fields); other -> other
      snapshot <- evaluate (force combined)
      atomically (writeTVar latest (Just (revision, snapshot)))
    let acceptLoop = forever $ mask $ \unmask ->
          bracketOnError (accept listener) (close . fst) $ \(client, _) -> do
            closeOnExec client
            modifyMVar_ clients $ \active ->
              if Map.size active >= 32
                then close client >> pure active
                else do
                  gate <- newEmptyTMVarIO
                  tid <- forkFinally (atomically (takeTMVar gate) >> unmask (serve client latest session)) $ \_ -> do
                    close client
                    modifyMVar_ clients (pure . Map.filter (/= client))
                  atomically (putTMVar gate ())
                  pure (Map.insert tid client active)
    accepter <- forkFinally acceptLoop (const (pure ()))
    let cleanup = do
          killThread accepter
          active <- modifyMVar clients (\m -> pure (Map.empty, Map.toList m))
          mapM_ (\(tid, client) -> close client >> killThread tid) active
          observePolicy policy (\_ _ _ -> pure ())
    restore body `finally` cleanup
 where
  serve client latest session = readRequests BS.empty
   where
    readRequests pending = case BS.elemIndex 10 pending of
      Just n -> do
        let (line, rest) = BS.splitAt n pending
        case decodeRequest line of
          Left err -> do
            sendValue (failure (errorRequestId err) "invalid_request" (errorMessage err))
            unless (errorClosesConnection err) (readRequests (BS.drop 1 rest))
          Right (Request ident op) -> case op of
            Snapshot -> readSnapshot >>= sendSnapshot ident >> readRequests (BS.drop 1 rest)
            Subscribe -> bracket watchDisconnect stopWatcher $ \(_, ended) -> do
              (revision, snapshot) <- atomically (disconnected ended `orElse` (readTVar latest >>= maybe retry pure))
              sendSnapshot ident (revision, snapshot)
              stream ended ident revision
            _ -> do
              canBuild <- policyCanBuild policy
              case if op == Recompile && not canBuild then Left "recompile unavailable: no installed build context" else command op of
                Left message -> sendValue (failure ident "invalid_command" message)
                Right action -> do
                  answer <- submitCommand (policyIngress policy) action
                  case answer of
                    CommandOverloaded -> sendValue (failure ident "overloaded" "command queue is full")
                    CommandClosed -> sendValue (failure ident "unavailable" "session ingress is closed")
                    CommandTimedOut -> sendValue (failure ident "unavailable" "command receipt timed out; application is unknown")
                    CommandApplied revision -> sendValue (reply ident (object ["session" .= session, "revision" .= revision]))
              readRequests (BS.drop 1 rest)
      Nothing
        | BS.length pending > maxLineBytes -> pure ()
        | otherwise -> do
            received <- timeout 60000000 (recv client (min 4096 (maxLineBytes + 1 - BS.length pending)))
            chunk <- maybe (fail "IPC client read timeout") pure received
            unless (BS.null chunk) (readRequests (pending <> chunk))
    readSnapshot = atomically (readTVar latest >>= maybe retry pure)
    watchDisconnect = do
      ended <- newEmptyTMVarIO
      let awaitEOF = recv client 4096 >>= \bytes -> unless (BS.null bytes) awaitEOF
      watcher <- forkFinally awaitEOF (\_ -> atomically (putTMVar ended ()))
      pure (watcher, ended)
    stopWatcher (watcher, ended) = killThread watcher >> atomically (readTMVar ended)
    disconnected ended = readTMVar ended >> throwSTM (userError "IPC subscriber disconnected")
    stream ended ident lastRevision = do
      next@(revision, _) <-
        atomically $
          disconnected ended `orElse` do
            value@(revision, _) <- readTVar latest >>= maybe retry pure
            check (revision /= lastRevision)
            pure value
      sendSnapshot ident next
      stream ended ident revision
    sendSnapshot ident (_, snapshot) = case encodeLine (reply ident (object ["snapshot" .= snapshot])) of
      Left _ -> sendValue (failure ident "snapshot_too_large" "snapshot exceeds line limit")
      Right line -> sendLine line
    sendValue value = case encodeLine value of
      Left _ -> fail "IPC response exceeds line limit"
      Right line -> sendLine line
    sendLine line = do
      sent <- timeout 5000000 (sendAll client line)
      when (sent == Nothing) (fail "IPC client write timeout")
  command = \case
    Recompile -> Right Action.restart
    Scratchpad name | Text.unpack name `elem` policyScratchpadNames policy -> Right (Action.toggleScratchpad (Text.unpack name))
    Scratchpad _ -> Left "unknown scratchpad"
    ScratchpadReset name | Text.unpack name `elem` policyScratchpadNames policy -> Right (Action.resetScratchpad (Text.unpack name))
    ScratchpadReset _ -> Left "unknown scratchpad"
    Workspace tag | Text.unpack tag `elem` policyTagsOf policy -> Right (Action.windows (StackSet.view (Text.unpack tag)))
    Spawn text -> Right (Action.spawn (Text.unpack text))
    Layout op -> Right $ case op of
      Next -> Action.sendMessage Layout.NextLayout
      Shrink -> Action.sendMessage Layout.Shrink
      Expand -> Action.sendMessage Layout.Expand
      IncMaster -> Action.sendMessage (Layout.IncMasterN 1)
      DecMaster -> Action.sendMessage (Layout.IncMasterN (-1))
    _ -> Left "unknown workspace"

-- | Admission and application are separate outcomes for one command handler.
data CommandResult = CommandOverloaded | CommandClosed | CommandTimedOut | CommandApplied Integer
  deriving (Eq, Show)

submitCommand :: ExternalIngress (ExternalInput i l) -> Action.Action i l () -> IO CommandResult
submitCommand ingress action = do
  receipt <- newEmptyTMVarIO
  submitExternal ingress (ExternalInput action (Just receipt)) >>= \case
    Full -> pure CommandOverloaded
    IngressClosed -> pure CommandClosed
    Accepted -> maybe CommandTimedOut CommandApplied <$> timeout 5000000 (atomically (takeTMVar receipt))

reply :: Text.Text -> Value -> Value
reply ident result = object ["version" .= (1 :: Int), "id" .= ident, "result" .= result]

failure :: Text.Text -> Text.Text -> String -> Value
failure ident code message = object ["version" .= (1 :: Int), "id" .= ident, "error" .= object ["code" .= code, "message" .= Text.take 1024 (Text.pack message)]]

ownedSocket :: IO Socket
ownedSocket = do
  sock <- socket AF_UNIX Stream defaultProtocol
  closeOnExec sock `onException` close sock
  pure sock

closeOnExec :: Socket -> IO ()
closeOnExec sock = withFdSocket sock (\fd -> setFdOption (Fd fd) CloseOnExec True)
