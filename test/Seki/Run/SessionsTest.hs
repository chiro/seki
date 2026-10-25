{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.SessionsTest (tests) where

import Control.Exception (bracket, onException, throwIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Seki.Internal.Config (Config (..), def)
import Seki.Internal.Core.Action (restart)
import Seki.Internal.Core.ManageHook (doShift, idHook)
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Run.Config (SekiArgs (..), parseSekiArgs, runConfig, runSessions)
import Seki.Run.MockRiver (FrameReader, encodedPayload, event, expectMockSuccess, handshake, newFrameReader, openRuntimePair, readUntil, receiveFrame, receiveLayerShellOutputRequest, receiveNewIdRequest, requests, requireObjectId, runMock, sendFrames)
import Seki.Run.Policy (PolicyConfig (..))
import Seki.Run.Restart (Launch (..), RestartRefusal (..))
import Seki.Wire.Arguments (putInt, putNewId, putString, putUint)
import Seki.Wire.Transport (ConnectionTarget (..), ReceivedChunk (..), Transport, closeTransport, receiveChunk)
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..))
import System.Directory (listDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "restart sessions"
    [ testCase "no arguments start fresh" $ parseSekiArgs [] @?= Right (SekiArgs Nothing)
    , testCase "--restore names the state file" $ parseSekiArgs ["--restore", "/run/seki/x.state"] @?= Right (SekiArgs (Just "/run/seki/x.state"))
    , testCase "any other arguments are a usage error" $ mapM_ (either (const (pure ())) (assertFailure . show) . parseSekiArgs) [["--restore"], ["-v"], ["--restore", "a", "b"]]
    , testCase "a failed exec reconnects on the socket path and restores in the same process" testFallback
    , testCase "unavailable at startup retries on the socket path" testUnavailableRetry
    , testCase "a state file of an unknown version starts fresh and is deleted" testUnknownVersion
    ]

-- | A window with this identifier on an output named 56, then manage_start; returns the requests of that manage sequence.
enumerate :: FrameReader -> Transport -> (ObjectId, ObjectId, ObjectId) -> Word -> Text -> IO [RawFrame]
enumerate reader peer (manager, _, layerShell) raw identifier = do
  window <- requireObjectId (fromIntegral raw)
  output <- requireObjectId (fromIntegral raw + 1)
  sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output)]
  _ <- receiveNewIdRequest reader peer window (Opcode 2)
  ls <- receiveLayerShellOutputRequest reader peer layerShell output
  sendFrames peer [event window 17 (putString identifier), event output 1 (putUint 56), event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100), event manager 2 (pure ())]
  readUntil reader peer manager 2

expectClosed :: Transport -> Assertion
expectClosed peer =
  receiveChunk peer 8 >>= \case
    Right Nothing -> pure ()
    other -> assertFailure ("expected the runtime to close the connection, got " <> show (fmap (fmap receivedBytes) other))

{- | Workspace 1 under Full. A fresh session restarts at once and keeps new
windows on workspace 1. A restored session refuses to restart and sends new
windows to workspace 2, so only a restored window is proposed a tile there.
-}
sessionConfig :: IORef [String] -> Maybe a -> PolicyConfig String Full
sessionConfig logged restored =
  PolicyConfig Full ["1", "2"] [] defaultSceneStyle (\_ -> pure ()) hook restart [] Nothing check (\line -> modifyIORef' logged (line :))
 where
  hook = maybe idHook (const (doShift "2")) restored
  check = pure (maybe (Right ()) (const (Left TargetUnrecorded)) restored)

testFallback :: Assertion
testFallback = withSystemTempDirectory "seki-sessions" $ \dir -> do
  (first, firstPeer) <- openRuntimePair
  (second, secondPeer) <- openRuntimePair
  logged <- newIORef []
  execs <- newIORef []
  firstDone <- runMock $ flip onException (closeTransport firstPeer) $ do
    reader <- newFrameReader
    globals@(manager, _, _) <- handshake reader firstPeer
    _ <- enumerate reader firstPeer globals 0xff000010 "w1"
    stop <- receiveFrame reader firstPeer
    (rawFrameSender stop, rawFrameOpcode stop) @?= (manager, Opcode 0)
    window <- requireObjectId 0xff000010
    -- River closes every window when it processes stop, before finished.
    sendFrames firstPeer [event window 0 (pure ()), event manager 1 (pure ())]
    _ <- readUntil reader firstPeer manager 1
    expectClosed firstPeer
  secondDone <- runMock $ flip onException (closeTransport secondPeer) $ do
    reader <- newFrameReader
    globals <- handshake reader secondPeer
    frames <- enumerate reader secondPeer globals 0xff000020 "w1"
    window <- requireObjectId 0xff000020
    tile <- encodedPayload (putInt 196 >> putInt 96)
    requests window 3 frames @?= [tile]
    closeTransport secondPeer
  let launch =
        Launch
          { launchFirstTarget = Right first
          , launchReconnectTarget = Right second
          , launchTarget = Right "/opt/seki"
          , launchStateDir = Right (dir </> "seki")
          , launchExec = \path args _ -> modifyIORef' execs ((path, args) :) >> throwIO (userError "exec refused in tests")
          }
  result <- runSessions launch (sessionConfig logged) Nothing
  expectMockSuccess firstDone
  expectMockSuccess secondDone
  result @?= Right ()
  calls <- readIORef execs
  case calls of
    [("/opt/seki", ["--restore", file])] -> assertBool "the state file was in the state directory" (take (length dir) file == dir)
    other -> assertFailure ("expected one exec, got " <> show other)
  listDirectory (dir </> "seki") >>= (@?= [])
  lines' <- reverse <$> readIORef logged
  assertBool ("the fallback is logged: " <> show lines') ("restart: exec of /opt/seki failed (user error (exec refused in tests)); restoring in this process" `elem` lines')
  assertBool ("the restored session refuses its own restart: " <> show lines') ("restart refused: no executable path was recorded at startup" `elem` lines')

testUnavailableRetry :: Assertion
testUnavailableRetry = do
  (first, firstPeer) <- openRuntimePair
  (second, secondPeer) <- openRuntimePair
  logged <- newIORef []
  firstDone <- runMock $ flip onException (closeTransport firstPeer) $ do
    reader <- newFrameReader
    (manager, _, _) <- handshake reader firstPeer
    sendFrames firstPeer [event manager 0 (pure ())]
    expectClosed firstPeer
  secondDone <- runMock $ flip onException (closeTransport secondPeer) $ do
    reader <- newFrameReader
    globals <- handshake reader secondPeer
    _ <- enumerate reader secondPeer globals 0xff000030 "w1"
    closeTransport secondPeer
  let launch = Launch (Right first) (Right second) (Right "/opt/seki") (Left NoStateDirectory) (\_ _ _ -> throwIO (userError "unused"))
  result <- runSessions launch (const (sessionConfig logged (Just ())){policyStartupHook = pure ()}) Nothing
  expectMockSuccess firstDone
  expectMockSuccess secondDone
  result @?= Right ()
  readIORef logged >>= \lines' -> assertBool ("the retry is logged: " <> show lines') ("window management unavailable, retrying" `elem` lines')

testUnknownVersion :: Assertion
testUnknownVersion = withSystemTempDirectory "seki-sessions" $ \dir -> do
  (target, peer) <- openRuntimePair
  let file = dir </> "restore.state"
      fd = case target of
        InheritedSocket (Fd n) -> show n
        UnixSocketPath _ -> error "expected an inherited socket"
  writeFile file "seki-restore 99\nRestoreState {}"
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    globals <- handshake reader peer
    frames <- enumerate reader peer globals 0xff000040 "w1"
    window <- requireObjectId 0xff000040
    requests window 3 frames @?= []
    closeTransport peer
  result <- withEnv "WAYLAND_SOCKET" fd (runConfig ["--restore", file] def{workspaces = ["1", "2"], manageHook = doShift "2"})
  expectMockSuccess done
  either (assertFailure . show) pure result
  listDirectory dir >>= (@?= [])
 where
  withEnv name value action = bracket (lookupEnv name) (maybe (unsetEnv name) (setEnv name)) (\_ -> setEnv name value >> action)
