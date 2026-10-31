{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.IPCPerf (ipcPerfScenarios) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, displayException, finally)
import Control.Monad (forM_, replicateM)
import Data.Aeson (eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Integration.Fixture
import Seki.Integration.Harness
import Seki.Internal.Config (def)
import Seki.Internal.Core.StackSet qualified as SS
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Run.Config (policyConfigOf)
import Seki.Run.External
import Seki.Run.IPC (withIPCAt)
import Seki.Run.Policy
import Seki.Run.Runtime
import System.FilePath ((</>))
import System.Timeout (timeout)

ipcPerfScenarios :: [(String, IO ())]
ipcPerfScenarios = [("m3-ipc-perf", ipcPerf)]

ipcPerf :: IO ()
ipcPerf = do
  starts <- newIORef Map.empty
  serial <- newIORef (0 :: Integer)
  let decorate handlers =
        handlers
          { prepareRiverManagePlan = \snapshot -> do
              start <- getMonotonicTimeNSec
              revision <- atomicModifyIORef' serial (\n -> (n + 1, n + 1))
              atomicModifyIORef' starts (\values -> (Map.insert revision start values, ()))
              prepareRiverManagePlan handlers snapshot
          }
  withFixtureUsing decorate (policyConfigOf def) "m3-ipc-perf" 1 $ \fixture _ -> do
    root <- maybe (failIntegration "missing runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
    let path = root </> "seki" </> "ipc.sock"
        open = do
          client <- socket AF_UNIX Stream defaultProtocol
          connect client (SockAddrUnix path)
          pure client
        subscribe = do
          client <- open
          sendAll client "{\"version\":1,\"id\":\"sub\",\"op\":\"subscribe\"}\n"
          readLine <- bufferedLine client
          _ <- readLine
          pure (client, readLine)
    withIPCAt path (mPolicy fixture) $ do
      submitExternal (policyExternalProducer (mPolicy fixture)) (ExternalInput (pure ()) Nothing) >>= assertEqual "publish initial" Accepted
      forM_ [1, 2, 32 :: Int] $ \clients -> bracket open close $ \commander -> do
        readReply <- bufferedLine commander
        let unread = max 0 (clients - 2)
        bracket (replicateM unread subscribe) (mapM_ (close . fst)) $ \_ ->
          bracket (if clients == 1 then pure Nothing else Just <$> subscribe) (mapM_ (close . fst)) $ \subscriber -> do
            observed <- newChan
            done <- newEmptyMVar
            reader <- case subscriber of
              Nothing -> pure Nothing
              Just (_, readSnapshot) ->
                Just
                  <$> forkFinally
                    ( let loop = do
                            bytes <- readSnapshot
                            ended <- getMonotonicTimeNSec
                            revision <- snapshotRevision bytes
                            writeChan observed (Right (revision, ended))
                            loop
                       in loop
                    )
                    ( \result -> do
                        case result of Left e -> writeChan observed (Left (displayException e)); Right () -> pure ()
                        putMVar done ()
                    )
            let stop = mapM_ (\thread -> killThread thread >> takeMVar done) reader
                matching revision = do
                  value <- readChan observed >>= requireRight "subscriber reader"
                  if fst value >= revision then pure (snd value) else matching revision
            ( forM_ [1 .. 1000 :: Int] $ \n -> do
                let tag = if odd n then "2" else "1"
                started <- getMonotonicTimeNSec
                sendAll commander ("{\"version\":1,\"id\":\"perf\",\"op\":\"workspace\",\"tag\":\"" <> tag <> "\"}\n")
                response <- readReply
                ended <- getMonotonicTimeNSec
                revision <- appliedRevision response
                state <- readPolicyState (mPolicy fixture)
                assertEqual "observed requested workspace outside timed interval" (Just (if odd n then "2" else "1")) (case coreWorkspaces state of AttachedState ss _ -> Just (SS.currentTag ss); _ -> Nothing)
                begin <- Map.lookup revision <$> readIORef starts >>= maybe (failIntegration "missing matching owner application") pure
                change <- case subscriber of Nothing -> pure Nothing; Just _ -> Just . subtract begin <$> matching revision
                assertEqual "command below preselected 50ms ceiling" True (ended - started < 50000000)
                forM_ change $ \duration -> assertEqual "change to subscriber below 50ms ceiling" True (duration < 50000000)
                announce ("M3_IPC_PERF " <> show clients <> " " <> show n <> " " <> show (ended - started) <> " " <> maybe "null" show change)
              )
              `finally` stop
      saveCapture fixture "ipc-perf"

bufferedLine :: Socket -> IO (IO BS.ByteString)
bufferedLine client = do
  pending <- newIORef BS.empty
  let readOne = do
        bytes <- readIORef pending
        case BS.elemIndex 10 bytes of
          Just n -> do
            writeIORef pending (BS.drop (n + 1) bytes)
            pure (BS.take n bytes)
          Nothing | BS.length bytes > 65536 -> failIntegration "oversized response"
          Nothing -> do
            chunk <- recv client 4096
            if BS.null chunk then failIntegration "unexpected IPC disconnect" else writeIORef pending (bytes <> chunk) >> readOne
  pure (timeout assertionTimeoutMicroseconds readOne >>= maybe (failIntegration "IPC perf timeout") pure)

appliedRevision :: BS.ByteString -> IO Integer
appliedRevision bytes = do
  value <- requireRight "command JSON" (eitherDecodeStrict' bytes)
  requireRight "application revision" (parseEither (withObject "envelope" (\o -> o .: "result" >>= withObject "result" (.: "revision"))) value)

snapshotRevision :: BS.ByteString -> IO Integer
snapshotRevision bytes = do
  value <- requireRight "snapshot JSON" (eitherDecodeStrict' bytes)
  requireRight "snapshot revision" (parseEither (withObject "envelope" (\o -> o .: "result" >>= withObject "result" (\r -> r .: "snapshot" >>= withObject "snapshot" (.: "revision")))) value)
