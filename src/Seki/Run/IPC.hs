{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.IPC (withIPC, withIPCAt, ipcPath) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM
import Control.DeepSeq (force)
import Control.Exception (IOException, bracket, evaluate, finally, mask, try)
import Control.Monad (forever, unless, when)
import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
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
import Seki.Run.Policy (ExternalInput (..), Policy, closePolicyIngress, observePolicy, policyExternalProducer, policyTagsOf)
import Seki.Run.Snapshot (projectSnapshot)
import System.Directory (removeFile)
import System.Environment (getEnv)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Posix.Directory qualified as Posix
import System.Posix.Files
import System.Posix.Process (getProcessID)
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
  bracket (socket AF_UNIX Stream defaultProtocol) close $ \guard -> do
    bind guard (SockAddrUnix ('\0' : path))
    existing <- try @IOException (getSymbolicLinkStatus path)
    case existing of
      Left e | isDoesNotExistError e -> pure ()
      Left e -> ioError e
      Right old -> do
        unless (isSocket old && fileOwner old == uid) (fail "unsafe IPC endpoint")
        alive <- bracket (socket AF_UNIX Stream defaultProtocol) close $ \probe ->
          timeout 1000000 (try @IOException (connect probe (SockAddrUnix path)))
        case alive of
          Just (Left e) | isDoesNotExistError e -> pure ()
          Just (Left e) | ioeRefused e -> removeFile path
          _ -> fail "IPC endpoint is active or cannot be checked"
    bracket (socket AF_UNIX Stream defaultProtocol) close $ \listener -> do
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

withIPC :: (Layout.LayoutClass l) => Policy String l -> IO a -> IO a
withIPC policy body = ipcPath >>= \path -> withIPCAt path policy body

withIPCAt :: (Layout.LayoutClass l) => FilePath -> Policy String l -> IO a -> IO a
withIPCAt path policy body =
  withEndpoint path $ \listener -> mask $ \restore -> do
    pid <- getProcessID
    tick <- getMonotonicTimeNSec
    let session = Text.pack (show pid <> "-" <> show tick)
    latest <- newTVarIO Nothing
    jobs <- newTBQueueIO 256
    clients <- newMVar Map.empty
    healthy <- newTVarIO True
    observePolicy policy $ \revision st runtime -> do
      snapshot <- evaluate (force (projectSnapshot session Text.pack (policyTagsOf policy) revision st runtime))
      atomically (writeTVar latest (Just (revision, snapshot)))
    broker <-
      forkFinally
        ( forever $ do
            (action, result) <- atomically (readTBQueue jobs)
            receipt <- newEmptyTMVarIO
            admitted <- submitExternal (policyExternalProducer policy) (ExternalInput action (Just receipt))
            answer <- case admitted of
              Accepted -> Right <$> atomically (takeTMVar receipt)
              Full -> pure (Left "command queue is full")
              IngressClosed -> pure (Left "session ingress is closed")
            atomically (putTMVar result answer)
        )
        (\_ -> atomically (writeTVar healthy False))
    let acceptLoop = forever $ mask $ \unmask -> do
          (client, _) <- unmask (accept listener)
          modifyMVar_ clients $ \active ->
            if Map.size active >= 32
              then close client >> pure active
              else do
                gate <- newEmptyTMVarIO
                tid <- forkFinally (atomically (takeTMVar gate) >> unmask (serve client latest jobs healthy session)) $ \_ -> do
                  close client
                  modifyMVar_ clients (pure . Map.filter (/= client))
                atomically (putTMVar gate ())
                pure (Map.insert tid client active)
    accepter <- forkFinally acceptLoop (const (pure ()))
    let cleanup = do
          closePolicyIngress policy
          killThread accepter
          active <- modifyMVar clients (\m -> pure (Map.empty, Map.toList m))
          mapM_ (\(tid, client) -> close client >> killThread tid) active
          killThread broker
          observePolicy policy (\_ _ _ -> pure ())
    restore body `finally` cleanup
 where
  serve client latest jobs healthy session = readRequests BS.empty
   where
    readRequests pending = case BS.elemIndex 10 pending of
      Just n -> do
        let (line, rest) = BS.splitAt n pending
        case decodeRequest line of
          Left message -> sendValue (failure "" "invalid_request" message)
          Right (Request ident op) -> case op of
            Snapshot -> readSnapshot >>= sendSnapshot ident >> readRequests (BS.drop 1 rest)
            Subscribe -> do
              (revision, snapshot) <- readSnapshot
              sendSnapshot ident (revision, snapshot)
              stream ident revision
            _ -> do
              case command op of
                Left message -> sendValue (failure ident "invalid_command" message)
                Right action -> do
                  result <- newEmptyTMVarIO
                  queued <- atomically $ do
                    full <- isFullTBQueue jobs
                    alive <- readTVar healthy
                    if full || not alive then pure False else writeTBQueue jobs (action, result) >> pure True
                  if not queued
                    then sendValue (failure ident "overloaded" "command queue is full")
                    else do
                      answer <- atomically (takeTMVar result `orElse` (readTVar healthy >>= check . not >> pure (Left "session command broker stopped")))
                      case answer of
                        Left message -> sendValue (failure ident "unavailable" message)
                        Right revision -> sendValue (reply ident (object ["session" .= session, "revision" .= revision]))
              readRequests (BS.drop 1 rest)
      Nothing
        | BS.length pending > maxLineBytes -> pure ()
        | otherwise -> do
            received <- timeout 60000000 (recv client (min 4096 (maxLineBytes + 1 - BS.length pending)))
            chunk <- maybe (fail "IPC client read timeout") pure received
            unless (BS.null chunk) (readRequests (pending <> chunk))
    readSnapshot = atomically (readTVar latest >>= maybe retry pure)
    stream ident lastRevision = do
      next@(revision, _) <- atomically $ do
        value@(revision, _) <- readTVar latest >>= maybe retry pure
        check (revision /= lastRevision)
        pure value
      sendSnapshot ident next
      stream ident revision
    sendSnapshot ident (_, snapshot) = sendValue (reply ident (object ["snapshot" .= snapshot]))
    sendValue value = case encodeLine value of
      Left _ -> sendValue (failure "" "snapshot_too_large" "snapshot exceeds line limit")
      Right line -> do
        sent <- timeout 5000000 (sendAll client line)
        when (sent == Nothing) (fail "IPC client write timeout")
  command = \case
    Workspace tag | Text.unpack tag `elem` policyTagsOf policy -> Right (Action.windows (StackSet.view (Text.unpack tag)))
    Spawn text -> Right (Action.spawn (Text.unpack text))
    Layout op -> Right $ case op of
      Next -> Action.sendMessage Layout.NextLayout
      Shrink -> Action.sendMessage Layout.Shrink
      Expand -> Action.sendMessage Layout.Expand
      IncMaster -> Action.sendMessage (Layout.IncMasterN 1)
      DecMaster -> Action.sendMessage (Layout.IncMasterN (-1))
    _ -> Left "unknown workspace"

reply :: Text.Text -> Value -> Value
reply ident result = object ["version" .= (1 :: Int), "id" .= ident, "result" .= result]

failure :: Text.Text -> Text.Text -> String -> Value
failure ident code message = object ["version" .= (1 :: Int), "id" .= ident, "error" .= object ["code" .= code, "message" .= message]]
