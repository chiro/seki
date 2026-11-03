{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.BuildSessionTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally, throwIO)
import Control.Monad (void)
import Data.IORef
import Seki.Run.BuildSession
import Seki.Run.ConfigBuild
import Seki.Run.External
import Seki.Run.Restart
import Seki.Wire.Transport (ConnectionDiscoveryError (..), ConnectionTarget (..))
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, readSymbolicLink, setFileMode)
import System.Posix.IO (OpenMode (WriteOnly), closeFd, defaultFileFlags, dup, dupTo, openFd)
import System.Posix.Types (Fd (..))
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "session config builds"
    [ testCase "activation seal waits for accepted inputs and rejects later admission" $ do
        ingress <- newExternalIngress
        bindExternalWake (externalLifecycle ingress) (pure True) >>= (@?= True)
        submitExternal (externalProducer ingress) (1 :: Int) >>= (@?= Accepted)
        sealExternal ingress >>= (@?= False)
        drainExternal ingress >>= (\batch -> drainedInputs batch @?= [1])
        sealExternal ingress >>= (@?= True)
        submitExternal (externalProducer ingress) 2 >>= (@?= IngressClosed)
    , testCase "duplicate requests bound queue and invalidate older failure" $ do
        let (running, _) = transitionBuild (BuildIdle 7) BuildRequested
            queued = foldl (\state _ -> fst (transitionBuild state BuildRequested)) running [1 .. 1000 :: Int]
        queued @?= BuildRunning 7 1 (Just 2) Nothing
        transitionBuild queued (BuildCompleted 7 1 (Left ("obsolete", Nothing))) @?= (BuildRunning 7 2 Nothing Nothing, StartBuild 7 2)
    , testCase "old epoch and generation cannot fail current build" $ do
        let state = BuildRunning 7 3 Nothing Nothing
        transitionBuild state (BuildCompleted 6 3 (Left ("stale", Nothing))) @?= (state, NoBuildEffect)
        transitionBuild state (BuildCompleted 7 2 (Left ("stale", Nothing))) @?= (state, NoBuildEffect)
    , testCase "closed lifecycle cannot launch work" $
        transitionBuild BuildClosed BuildRequested @?= (BuildClosed, NoBuildEffect)
    , testCase "existing user files do not open template resources" $
        fixture "exit 0" $ \(BuildContext resources config _) _ -> do
          generate resources config
          writeFile (config </> "Main.hs") "user content"
          generate resources{templates = "/missing/templates"} config
          readFile (config </> "Main.hs") >>= (@?= "user content")
    , testCase "worker validates immutable copy, retries full ingress and reports full log" $
        fixture "exit 0" $ \context directory -> do
          session <- newBuildSession context
          events <- newIORef []
          attempts <- newIORef (0 :: Int)
          let send event = do
                n <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                if n < 3 then pure Full else modifyIORef' events (<> [event]) >> pure Accepted
          ( do
              requestBuild session send
              finished <- awaitCompletion events
              ready <- completeBuild session send finished
              ready @?= True
              target <- readyTarget session >>= maybe (assertFailure "no validated target" >> fail "missing") pure
              case finished of
                BuildCompleted epoch generation (Right _) -> do
                  let current = BuildRunning (epoch + 1) generation Nothing Nothing
                  transitionBuild current finished @?= (current, NoBuildEffect)
                  transitionBuild (BuildRunning epoch generation (Just (generation + 1)) Nothing) finished @?= (BuildRunning epoch (generation + 1) Nothing Nothing, StartBuild epoch (generation + 1))
                _ -> assertFailure "valid build did not complete successfully"
              validatedPath target /= directory </> "image" @? "candidate copied away from Cabal output"
              writeFile (directory </> "image") "overwritten"
              readFile (validatedPath target) >>= assertBool "immutable candidate retained" . (/= "overwritten")
              doesFileExist (validatedLog target) >>= assertBool "full disk log exists"
              readIORef attempts >>= assertBool "Full retried" . (>= 5)
            )
            `finally` closeBuildSession session
    , testCase "compile success and invalid checker never become ready" $
        fixture "exit 78" $ \context _ -> do
          session <- newBuildSession context
          events <- newIORef []
          let send event = modifyIORef' events (<> [event]) >> pure Accepted
          ( do
              requestBuild session send
              event <- awaitCompletion events
              completeBuild session send event >>= (@?= False)
              readyTarget session >>= (@?= Nothing)
            )
            `finally` closeBuildSession session
    , testCase "diagnostic write failure does not abort the owner or future builds" $
        fixture "exit 78" $ \context directory -> do
          session <- newBuildSession context
          events <- newIORef []
          let send event = modifyIORef' events (<> [event]) >> pure Accepted
          ( do
              requestBuild session send
              event <- awaitCompletion events
              bracket (dup (Fd 2)) closeFd $ \saved ->
                bracket (openFd "/dev/full" WriteOnly defaultFileFlags) closeFd $ \full ->
                  (void (dupTo full (Fd 2)) >> completeBuild session send event >>= (@?= False))
                    `finally` void (dupTo saved (Fd 2))
              readyTarget session >>= (@?= Nothing)
              writeIORef events []
              writeFile (directory </> "image") "#!/bin/sh\nexit 0\n"
              requestBuild session send
              next <- awaitCompletion events
              completeBuild session send next >>= (@?= True)
            )
            `finally` closeBuildSession session
    , testCase "validated promotion precedes exec and immediate failure rolls back" $
        fixture "exit 0" $ \(BuildContext resources config state) directory -> do
          target <- build resources config state
          let profile = directory </> "profile"
              old = directory </> "old-image"
              saved = directory </> "saved-state"
              launch =
                Launch
                  { launchFirstTarget = Left XdgRuntimeDirMissing
                  , launchReconnectTarget = Left XdgRuntimeDirMissing
                  , launchTarget = Right profile
                  , launchStateDir = Right directory
                  , launchExec = \path args _ -> do
                      path @?= profile
                      args @?= ["--restore", saved]
                      readSymbolicLink profile >>= (@?= validatedPath target)
                      readFile saved >>= (@?= "serialized before promotion")
                      throwIO (userError "injected exec failure")
                  }
          writeFile old "old"
          writeFile saved "serialized before promotion"
          createSymbolicLink old profile
          _ <- execValidatedRestart launch target saved
          readSymbolicLink profile >>= (@?= old)
    , testCase "a checked replacement does not depend on the old executable still existing" $
        fixture "exit 0" $ \(BuildContext resources config state) directory -> do
          target <- build resources config state
          let old = directory </> "missing-old-image"
              launch =
                Launch
                  { launchFirstTarget = Right (UnixSocketPath "/unused")
                  , launchReconnectTarget = Right (UnixSocketPath "/unused")
                  , launchTarget = Right old
                  , launchStateDir = Right directory
                  , launchExec = \_ _ _ -> throwIO (userError "must not exec during preflight")
                  }
          checkRestartTarget launch RecordedTarget >>= (@?= Left (TargetMissing old))
          checkRestartTarget launch (CheckedTarget target) >>= (@?= Right ())
          doesPathExist (validatedProfile target) >>= (@?= False)
    , testCase "direct fallback promotes canonical profile and removes it on exec failure" $
        fixture "exit 0" $ \(BuildContext resources config state) directory -> do
          target <- build resources config state
          let raw = directory </> "raw-default"
              profile = directory </> "profile"
              launch =
                Launch
                  { launchFirstTarget = Left XdgRuntimeDirMissing
                  , launchReconnectTarget = Left XdgRuntimeDirMissing
                  , launchTarget = Right raw
                  , launchStateDir = Right directory
                  , launchExec = \path _ _ -> do
                      path @?= profile
                      readSymbolicLink profile >>= (@?= validatedPath target)
                      throwIO (userError "injected fallback exec failure")
                  }
          writeFile raw "raw default stays intact"
          _ <- execValidatedRestart launch target (directory </> "saved")
          doesPathExist profile >>= (@?= False)
          readFile raw >>= (@?= "raw default stays intact")
    , testCase "close cancels and reaps held compiler process group" $
        fixture "exit 0" $ \(BuildContext resources config state) directory -> do
          writeFile (environment resources) ("#!/bin/sh\ntouch '" <> directory </> "started'\nwhile [ ! -f '" <> directory </> "release' ]; do sleep 0.02; done\ntouch '" <> directory </> "escaped'\n")
          session <- newBuildSession (BuildContext resources config state)
          requestBuild session (const (pure Accepted))
          await (doesFileExist (directory </> "started"))
          closeBuildSession session
          closeBuildSession session
          writeFile (directory </> "release") ""
          threadDelay 100000
          doesFileExist (directory </> "escaped") >>= (@?= False)
    ]

fixture :: String -> (BuildContext -> FilePath -> IO ()) -> IO ()
fixture check body = withSystemTempDirectory "seki-build-test" $ \directory -> do
  let wrapper = directory </> "environment"
      image = directory </> "image"
      templatesPath = directory </> "templates"
  createDirectory templatesPath
  mapM_ (\name -> writeFile (templatesPath </> name) "") ["Main.hs", "seki-user.cabal", "cabal.project"]
  writeFile image ("#!/bin/sh\n" <> check <> "\n")
  setFileMode image 0o700
  writeFile wrapper ("#!/bin/sh\nfor arg do\ncase \"$arg\" in\nbuild) echo compiler-output; exit 0;;\nlist-bin) printf '%s\\n' '" <> image <> "'; exit 0;;\nesac\ndone\nexit 2\n")
  setFileMode wrapper 0o700
  body (BuildContext (Resources "/archive" "/compiler" "/cabal" wrapper templatesPath) directory directory) directory

await :: IO Bool -> IO ()
await predicate = do
  result <- timeout 3000000 loop
  result @?= Just ()
 where
  loop = predicate >>= \ready -> if ready then pure () else threadDelay 1000 >> loop

awaitCompletion :: IORef [BuildEvent] -> IO BuildEvent
awaitCompletion events = do
  await (any completed <$> readIORef events)
  values <- readIORef events
  case filter completed values of
    event : _ -> pure event
    _ -> fail "missing completion"
 where
  completed BuildCompleted{} = True
  completed _ = False
