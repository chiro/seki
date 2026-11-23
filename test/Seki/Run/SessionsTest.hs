{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.SessionsTest (tests) where

import Control.Exception (AsyncException (UserInterrupt), ErrorCall, onException, throw, throwIO, try)
import Control.Monad (when, (>=>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Seki.Internal.Config (Config (..), def)
import Seki.Internal.Core.Action (restart)
import Seki.Internal.Core.ManageHook (doShift, idHook)
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Run.BuildSession (StartupBuild (..))
import Seki.Run.Config (Entry (..), SekiArgs (..), parseSekiArgs, runConfig, runSessions)
import Seki.Run.ConfigBuild (Trial (..), Unproven (..), markUnproven, readUnproven, startupArguments)
import Seki.Run.IPC (withIPCAt)
import Seki.Run.MockRiver (FrameReader, encodedPayload, event, expectMockSuccess, handshake, newFrameReader, openRuntimePair, readUntil, receiveFrame, receiveLayerShellOutputRequest, receiveNewIdRequest, requests, requireObjectId, runMock, sendFrames, withSandbox, withVariables)
import Seki.Run.Policy (PolicyConfig (..), newPolicy)
import Seki.Run.Restart (Launch (..), RestartRefusal (..))
import Seki.Wire.Arguments (putInt, putNewId, putObject, putString, putUint)
import Seki.Wire.Transport (ConnectionDiscoveryError (..), ConnectionTarget (..), ReceivedChunk (..), Transport, closeTransport, receiveChunk)
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getEnv, getExecutablePath, lookupEnv, setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "restart sessions"
    [ testCase "no arguments start fresh and owe no build" $ parseSekiArgs [] @?= Right (Run NoStartupBuild)
    , testCase "the launcher's arguments start fresh and owe one build" $ parseSekiArgs startupArguments @?= Right (Run StartupBuild)
    , testCase "--restore names the state file" $ parseSekiArgs ["--restore", "/run/seki/x.state"] @?= Right (Restore "/run/seki/x.state")
    , testCase "--check selects candidate validation" $ parseSekiArgs ["--check"] @?= Right Check
    , testCase "any other arguments are a usage error" $ mapM_ (either (const (pure ())) (assertFailure . show) . parseSekiArgs) [["--restore"], ["--restore", "--check"], ["-v"], ["--restore", "a", "b"], ["--check", "--restore", "a"], ["--restore", "a", "--check"], ["--check", "--check"], ["--check", "extra"], startupArguments <> ["--check"], "--check" : startupArguments, startupArguments <> ["--restore", "a"], "--restore" : startupArguments, startupArguments <> startupArguments]
    , testCase "a failed exec reconnects on the socket path and restores in the same process" testFallback
    , testCase "failed exec reconnects when diagnostic logging fails" (fallbackSession True)
    , testCase "unavailable at startup retries on the socket path" testUnavailableRetry
    , testCase "a state file of an unknown version starts fresh and is deleted" testUnknownVersion
    , testCase "an image that cannot reach river is no longer on trial" testNeverManaged
    , testCase "an image whose session ends cleanly before it proved healthy is no longer on trial" testCleanEndOnTrial
    , testCase "an image that dies in its startup hook records that it crashed" testDiesOnTrial
    , testCase "an image that river stops for a protocol error records that it crashed" testProtocolErrorOnTrial
    , testCase "an interrupt records nothing, so the image is still on its first trial" testInterruptedOnTrial
    , testCase "a sandbox owns the home, the XDG directories, and the seki variables, and gives each its earlier value back" testSandbox
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
  PolicyConfig [] [] Full ["1", "2"] [] defaultSceneStyle (\_ -> pure ()) hook restart [] Nothing check (\line -> modifyIORef' logged (line :))
 where
  hook = maybe idHook (const (doShift "2")) restored
  check = const (pure (maybe (Right ()) (const (Left TargetUnrecorded)) restored))

testFallback :: Assertion
testFallback = fallbackSession False

fallbackSession :: Bool -> Assertion
fallbackSession brokenLog = withSandbox [] $ \dir -> do
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
  let configFor restored =
        (sessionConfig logged restored)
          { policyLog = \line -> do
              modifyIORef' logged (line :)
              when (brokenLog && "restart: exec of " `isPrefixOf` line) (throwIO (userError "diagnostic logging unavailable"))
          }
  result <- runSessions launch configFor (Fresh NoStartupBuild)
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
testUnavailableRetry = withSandbox [] $ \_ -> do
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
  owner <- newPolicy (sessionConfig logged (Just ())) >>= either (assertFailure . show) pure
  runtime <- getEnv "XDG_RUNTIME_DIR"
  result <-
    withIPCAt (runtime </> "seki" </> "ipc.sock") owner $
      runSessions launch (const (sessionConfig logged (Just ())){policyStartupHook = pure ()}) (Fresh NoStartupBuild)
  expectMockSuccess firstDone
  expectMockSuccess secondDone
  result @?= Right ()
  readIORef logged >>= \lines' -> do
    assertBool ("the retry is logged: " <> show lines') ("window management unavailable, retrying" `elem` lines')
    assertBool "occupied IPC endpoint is diagnosed" (any ((== "IPC unavailable") . take 15) lines')

testUnknownVersion :: Assertion
testUnknownVersion = withSandbox [] $ \_ -> withSystemTempDirectory "seki-sessions" $ \dir -> do
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
  setEnv "WAYLAND_SOCKET" fd
  result <- runConfig ["--restore", file] def{workspaces = ["1", "2"], manageHook = doShift "2"}
  expectMockSuccess done
  either (assertFailure . show) pure result
  listDirectory dir >>= (@?= [])

{- | An installed session whose state directory holds a marker that names
this test executable, as if the launcher had just exec'd it.
-}
onTrial :: (FilePath -> IO a) -> IO a
onTrial body = withSandbox [("SEKI_CONFIG_" <> name, "/store" </> name) | name <- ["ARCHIVE", "GHC", "CABAL", "ENV", "TEMPLATES"]] $ \_ -> do
  state <- (</> "seki") <$> getEnv "XDG_STATE_HOME"
  createDirectoryIfMissing True state
  getExecutablePath >>= markUnproven state . (`Unproven` FirstTrial)
  body state

testNeverManaged :: Assertion
testNeverManaged = onTrial $ \state -> do
  logged <- newIORef []
  let launch = Launch (Left XdgRuntimeDirMissing) (Left XdgRuntimeDirMissing) (Right "/opt/seki") (Left NoStateDirectory) (\_ _ _ -> throwIO (userError "unused"))
  result <- runSessions launch (const (sessionConfig logged (Just ()))) (Fresh StartupBuild)
  either (const (pure ())) (const (assertFailure "expected the connection to fail")) result
  readUnproven state >>= (@?= Nothing)

testCleanEndOnTrial :: Assertion
testCleanEndOnTrial = onTrial $ \state -> do
  (target, peer) <- openRuntimePair
  logged <- newIORef []
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    globals <- handshake reader peer
    _ <- enumerate reader peer globals 0xff000060 "w1"
    closeTransport peer
  result <- runSessions (singleSession target) (const (sessionConfig logged (Just ())){policyStartupHook = pure ()}) (Fresh StartupBuild)
  expectMockSuccess done
  result @?= Right ()
  readUnproven state >>= (@?= Nothing)

-- | One window and one output, then a manage sequence that runs the startup hook.
hookSequence :: Transport -> IO ()
hookSequence peer = do
  reader <- newFrameReader
  (manager, _, _) <- handshake reader peer
  window <- requireObjectId 0xff000050
  output <- requireObjectId 0xff000051
  sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 2 (pure ())]
  expectClosedAfterRequests peer

singleSession :: ConnectionTarget -> Launch
singleSession target = Launch (Right target) (Left XdgRuntimeDirMissing) (Right "/opt/seki") (Left NoStateDirectory) (\_ _ _ -> throwIO (userError "unused"))

testDiesOnTrial :: Assertion
testDiesOnTrial = onTrial $ \state -> do
  (target, peer) <- openRuntimePair
  logged <- newIORef []
  done <- runMock (hookSequence peer `onException` closeTransport peer)
  image <- getExecutablePath
  outcome <- try @ErrorCall (runSessions (singleSession target) (const (sessionConfig logged (Just ())){policyStartupHook = error "the startup hook throws"}) (Fresh StartupBuild))
  either (const (pure ())) (assertFailure . ("expected the hook to end the process, got " <>) . show) outcome
  expectMockSuccess done
  readUnproven state >>= (@?= Just (Unproven image Crashed))

testInterruptedOnTrial :: Assertion
testInterruptedOnTrial = onTrial $ \state -> do
  (target, peer) <- openRuntimePair
  logged <- newIORef []
  done <- runMock (hookSequence peer `onException` closeTransport peer)
  image <- getExecutablePath
  outcome <- try @AsyncException (runSessions (singleSession target) (const (sessionConfig logged (Just ())){policyStartupHook = throw UserInterrupt}) (Fresh StartupBuild))
  outcome @?= Left UserInterrupt
  expectMockSuccess done
  readUnproven state >>= (@?= Just (Unproven image FirstTrial))

testProtocolErrorOnTrial :: Assertion
testProtocolErrorOnTrial = onTrial $ \state -> do
  (target, peer) <- openRuntimePair
  logged <- newIORef []
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    globals@(manager, _, _) <- handshake reader peer
    _ <- enumerate reader peer globals 0xff000070 "w1"
    display <- requireObjectId 1
    sendFrames peer [event display 0 (putObject manager >> putUint 0 >> putString "sequence order violated")]
    expectClosedAfterRequests peer
  image <- getExecutablePath
  result <- runSessions (singleSession target) (const (sessionConfig logged (Just ())){policyStartupHook = pure ()}) (Fresh StartupBuild)
  expectMockSuccess done
  either (const (pure ())) (const (assertFailure "expected the protocol error to end the session")) result
  readUnproven state >>= (@?= Just (Unproven image Crashed))

testSandbox :: Assertion
testSandbox = do
  let owned = ["HOME", "XDG_CONFIG_HOME", "XDG_STATE_HOME", "XDG_CACHE_HOME", "XDG_DATA_HOME", "XDG_RUNTIME_DIR"]
      ambient = zip owned [Just "/ambient/home", Nothing, Just "/ambient/state", Nothing, Nothing, Just "/ambient/run"] <> [("SEKI_CONFIG_ARCHIVE", Just "/ambient/archive"), ("SEKI_CONFIG_LAUNCHER", Just "/ambient/bin/seki"), ("SEKI_CONFIG_GHC", Nothing), ("WAYLAND_DISPLAY", Just "wayland-1"), ("WAYLAND_SOCKET", Nothing)]
  withVariables ambient $ do
    withSandbox [("SEKI_CONFIG_GHC", "/store/ghc")] $ \root -> do
      inside <- mapM lookupEnv owned
      assertBool ("every owned directory is under the sandbox: " <> show inside) (all (maybe False (isPrefixOf root)) inside)
      mapM_ (maybe (assertFailure "an owned directory is unset") (doesDirectoryExist >=> assertBool "an owned directory exists")) inside
      runtime <- getEnv "XDG_RUNTIME_DIR"
      getFileStatus runtime >>= (@?= 0o700) . (`intersectFileModes` 0o777) . fileMode
      mapM lookupEnv ["SEKI_CONFIG_ARCHIVE", "SEKI_CONFIG_LAUNCHER", "SEKI_CONFIG_GHC", "WAYLAND_DISPLAY"] >>= (@?= [Nothing, Nothing, Just "/store/ghc", Nothing])
      setEnv "WAYLAND_SOCKET" "7"
    mapM (lookupEnv . fst) ambient >>= (@?= map snd ambient)

-- | Read whatever the runtime still sends until it closes the connection.
expectClosedAfterRequests :: Transport -> Assertion
expectClosedAfterRequests peer =
  receiveChunk peer 4096 >>= \case
    Right Nothing -> pure ()
    Right (Just _) -> expectClosedAfterRequests peer
    Left e -> assertFailure ("expected the runtime to close the connection, got " <> show e)
