{-# LANGUAGE LambdaCase #-}

module Seki.Run.RestartTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Bits ((.&.))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Seki.Internal.Core.Restore (RestoreError (..), RestoreState (..), SavedFloat (..), SavedWorkspace (..), WindowKey (..))
import Seki.Internal.Core.StackSet (Stack (..))
import Seki.Run.Restart
import Seki.Wire.Transport (ConnectionDiscoveryError (..), ConnectionTarget (..))
import System.Directory (createDirectory, doesFileExist, listDirectory)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus, setFileMode)
import System.Process (createProcess, getPid, proc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "restart supervisor"
    [ testGroup
        "the restart check"
        [ testCase "refuses a missing target" $ checked $ \dir -> pure (dir </> "absent", Left (TargetMissing (dir </> "absent")))
        , testCase "refuses a directory" $ checked $ \dir -> createDirectory (dir </> "d") >> pure (dir </> "d", Left (TargetNotRegularFile (dir </> "d")))
        , testCase "refuses a file without the execute bit" $ checked $ \dir -> file dir "plain" 0o644 >>= \p -> pure (p, Left (TargetNotExecutable p))
        , testCase "accepts an executable file" $ checked $ \dir -> file dir "seki" 0o755 >>= \p -> pure (p, Right ())
        , testCase "accepts a symlink to an executable file" $ checked $ \dir -> do
            target <- file dir "seki-new" 0o755
            createSymbolicLink target (dir </> "current")
            pure (dir </> "current", Right ())
        , testCase "refuses a dangling symlink as missing" $ checked $ \dir -> do
            createSymbolicLink (dir </> "gone") (dir </> "current")
            pure (dir </> "current", Left (TargetMissing (dir </> "current")))
        , testCase "refuses when no socket path can be reconnected to" $ withSystemTempDirectory "seki-restart" $ \dir -> do
            p <- file dir "seki" 0o755
            checkRestart (launchAt dir p){launchReconnectTarget = Left XdgRuntimeDirMissing} >>= (@?= Left (NoReconnectTarget XdgRuntimeDirMissing))
        , testCase "refuses when there is no state directory" $ withSystemTempDirectory "seki-restart" $ \dir -> do
            p <- file dir "seki" 0o755
            checkRestart (launchAt dir p){launchStateDir = Left NoStateDirectory} >>= (@?= Left NoStateDirectory)
        , testCase "refuses when no target was recorded" $ withSystemTempDirectory "seki-restart" $ \dir ->
            checkRestart (launchAt dir ""){launchTarget = Left TargetUnrecorded} >>= (@?= Left TargetUnrecorded)
        ]
    , testGroup
        "the restart target"
        [ testCase "an absolute argv[0] is kept as given" $ targetFromArgv0 "/home/u" noPath "/nix/profile/bin/seki" >>= (@?= Just "/nix/profile/bin/seki")
        , testCase "a relative argv[0] is made absolute against the startup directory" $ targetFromArgv0 "/home/u" noPath "./bin/seki" >>= (@?= Just "/home/u/bin/seki")
        , testCase "a bare argv[0] is looked up on PATH" $ targetFromArgv0 "/home/u" (\n -> pure (if n == "seki" then Just "/usr/bin/seki" else Nothing)) "seki" >>= (@?= Just "/usr/bin/seki")
        , testCase "a bare argv[0] found through a relative PATH entry is made absolute" $ targetFromArgv0 "/home/u" (\_ -> pure (Just "bin/seki")) "seki" >>= (@?= Just "/home/u/bin/seki")
        , testCase "a bare argv[0] missing from PATH gives nothing" $ targetFromArgv0 "/home/u" noPath "seki" >>= (@?= Nothing)
        , testCase "an empty argv[0] gives nothing" $ targetFromArgv0 "/home/u" noPath "" >>= (@?= Nothing)
        ]
    , testGroup
        "the state file"
        [ testCase "is written privately, read back, and deleted by the reader" testStateFileRoundTrip
        , testCase "of an unknown version is deleted and refused by version" testUnknownVersionFile
        , testCase "that cannot be read is refused" $ withSystemTempDirectory "seki-restart" $ \dir ->
            loadRestoreFile (dir </> "absent") >>= \case
              Left (RestoreFileUnreadable _) -> pure ()
              other -> assertFailure ("expected an unreadable file, got " <> show other)
        ]
    , testCase "exec passes --restore and the environment without WAYLAND_SOCKET, and returns its failure" testExecRestart
    , testCase "reaping collects an exited child" testReap
    ]
 where
  noPath _ = pure Nothing

checked :: (FilePath -> IO (FilePath, Either RestartRefusal ())) -> Assertion
checked prepare = withSystemTempDirectory "seki-restart" $ \dir -> do
  (path, expected) <- prepare dir
  checkRestart (launchAt dir path) >>= (@?= expected)

file :: FilePath -> FilePath -> Int -> IO FilePath
file dir name mode = do
  let path = dir </> name
  writeFile path "#!/bin/sh\n"
  setFileMode path (fromIntegral mode)
  pure path

launchAt :: FilePath -> FilePath -> Launch
launchAt dir target =
  Launch
    { launchFirstTarget = Right (UnixSocketPath (dir </> "wayland-1"))
    , launchReconnectTarget = Right (UnixSocketPath (dir </> "wayland-1"))
    , launchTarget = Right target
    , launchStateDir = Right (dir </> "seki")
    , launchExec = \_ _ _ -> throwIO (userError "exec disabled in tests")
    }

sample :: RestoreState String
sample =
  RestoreState
    [SavedWorkspace "1" Nothing "Full" (Just (Stack (WindowKey (Text.pack "c68415ce")) [] [WindowKey (Text.pack "\12354")]))]
    [(WindowKey (Text.pack "c68415ce"), SavedFloatAt 1 2 3 4)]
    Set.empty
    (Set.fromList ["foot"])

testStateFileRoundTrip :: Assertion
testStateFileRoundTrip = withSystemTempDirectory "seki-restart" $ \dir -> do
  let stateDir = dir </> "seki"
  path <- writeRestoreFile stateDir sample >>= either (assertFailure . show) pure
  dirMode <- fileMode <$> getFileStatus stateDir
  fileMode' <- fileMode <$> getFileStatus path
  (dirMode .&. 0o777, fileMode' .&. 0o777) @?= (0o700, 0o600)
  listDirectory stateDir >>= (@?= [drop (length stateDir + 1) path])
  loadRestoreFile path >>= (@?= Right sample)
  doesFileExist path >>= assertBool "the reader deletes the file" . not

testUnknownVersionFile :: Assertion
testUnknownVersionFile = withSystemTempDirectory "seki-restart" $ \dir -> do
  let path = dir </> "restore.state"
  writeFile path "seki-restore 99\nRestoreState {}"
  result <- loadRestoreFile path
  case result of
    Left e -> do
      e @?= RestoreDecodeError (RestoreUnknownVersion "99")
      describeLoadError e @?= "unknown state version 99, starting fresh"
    Right _ -> assertFailure "expected an unknown version"
  doesFileExist path >>= assertBool "the reader deletes the file" . not

testExecRestart :: Assertion
testExecRestart = withSystemTempDirectory "seki-restart" $ \dir -> do
  calls <- newIORef Nothing
  setEnv "WAYLAND_SOCKET" "7"
  let launch = (launchAt dir "/opt/seki"){launchExec = \p a e -> writeIORef calls (Just (p, a, lookup "WAYLAND_SOCKET" e)) >> throwIO (userError "no such file")}
  failure <- execRestart launch (dir </> "restore.state")
  unsetEnv "WAYLAND_SOCKET"
  show failure @?= show (userError "no such file")
  readIORef calls >>= (@?= Just ("/opt/seki", ["--restore", dir </> "restore.state"], Nothing))

testReap :: Assertion
testReap = do
  (_, _, _, handle) <- createProcess (proc "true" [])
  pid <- getPid handle >>= maybe (assertFailure "the child has no pid") pure
  threadDelay 200000
  reapInheritedChildren
  doesFileExist ("/proc/" <> show pid <> "/stat") >>= assertBool "the exited child is reaped" . not
