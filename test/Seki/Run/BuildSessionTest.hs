{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.BuildSessionTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, tryTakeTMVar)
import Control.Exception (bracket, finally, throwIO)
import Control.Monad (void)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Seki.Internal.Core.Action (restart)
import Seki.Internal.Core.ManageHook (idHook)
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Layout.Full qualified as Layout
import Seki.Run.BuildSession
import Seki.Run.ConfigBuild
import Seki.Run.External
import Seki.Run.Inventory (emptyRiverInventory)
import Seki.Run.Policy (ExternalInput (..), PolicyConfig (..), closePolicy, configureBuild, newPolicy, policyIngress, policyRuntimeHandlers, policyValidatedTarget)
import Seki.Run.Restart
import Seki.Run.Runtime (RiverInputSnapshot (..), RiverRuntimeHandlers (..), RiverRuntimeSnapshot (..))
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
    [ testCase "duplicate requests bound queue and invalidate older failure" $ do
        let running = transitionBuild BuildIdle BuildRequested
            queued = foldl (\state _ -> transitionBuild state BuildRequested) running [1 .. 1000 :: Int]
        queued @?= BuildRunning 1 True Nothing
        transitionBuild queued (BuildFinished (Left ("obsolete", Nothing))) @?= BuildRunning 2 False Nothing
    , testCase "policy build completion wakes the owner through a full command queue" $
        fixture "exit 0" $ \context@(BuildContext resources _ _) directory -> do
          writeFile (environment resources) ("#!/bin/sh\nfor arg do\ncase \"$arg\" in\nbuild) touch '" <> directory </> "started'; while [ ! -f '" <> directory </> "release' ]; do sleep 0.02; done; exit 0;;\nlist-bin) printf '%s\\n' '" <> directory </> "image'; exit 0;;\nesac\ndone\nexit 2\n")
          policy <- newPolicy (PolicyConfig [] [] Layout.Full [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [] Nothing (const (pure (Right ()))) (\_ -> pure ())) >>= either (assertFailure . show) pure
          configureBuild policy context
          let handlers = policyRuntimeHandlers policy
              ingress = policyIngress policy
              snapshot = RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) []
          handle <- maybe (assertFailure "missing ingress") pure (riverExternalIngress handlers)
          let takeWake = isJust <$> atomically (tryTakeTMVar (ingressWake handle))
          ( do
              submitExternal ingress (ExternalInput restart Nothing) >>= (@?= Accepted)
              _ <- prepareRiverManagePlan handlers snapshot
              await (doesFileExist (directory </> "started"))
              mapM_ (\_ -> submitExternal ingress (ExternalInput (pure ()) Nothing)) [1 .. 256 :: Int]
              submitExternal ingress (ExternalInput (pure ()) Nothing) >>= (@?= Full)
              _ <- takeWake
              writeFile (directory </> "release") ""
              await (isJust <$> policyValidatedTarget policy)
              await takeWake
            )
            `finally` closePolicy policy
    , testCase "existing user files do not open template resources" $
        fixture "exit 0" $ \(BuildContext resources config _) _ -> do
          generate resources config
          writeFile (config </> "Main.hs") "user content"
          generate resources{templates = "/missing/templates"} config
          readFile (config </> "Main.hs") >>= (@?= "user content")
    , testCase "worker validates immutable copy, reports full log and wakes the owner" $
        fixture "exit 0" $ \context directory -> do
          wakes <- newIORef (0 :: Int)
          session <- newBuildSession (modifyIORef' wakes (+ 1)) context
          ( do
              requestBuild session
              target <- awaitReady session
              validatedPath target /= directory </> "image" @? "candidate copied away from Cabal output"
              writeFile (directory </> "image") "overwritten"
              readFile (validatedPath target) >>= assertBool "immutable candidate retained" . (/= "overwritten")
              doesFileExist (validatedLog target) >>= assertBool "full disk log exists"
              await ((>= 2) <$> readIORef wakes)
            )
            `finally` closeBuildSession session
    , testCase "compile success and invalid checker never become ready" $
        fixture "exit 78" $ \context _ -> do
          session <- newBuildSession (pure ()) context
          ( do
              requestBuild session
              awaitStatus session "failed" 1
              readyTarget session >>= (@?= Nothing)
            )
            `finally` closeBuildSession session
    , testCase "diagnostic write failure does not abort the owner or future builds" $
        fixture "exit 78" $ \context directory -> do
          session <- newBuildSession (pure ()) context
          ( do
              bracket (dup (Fd 2)) closeFd $ \saved ->
                bracket (openFd "/dev/full" WriteOnly defaultFileFlags) closeFd $ \full ->
                  ( do
                      void (dupTo full (Fd 2))
                      requestBuild session
                      awaitStatus session "failed" 1
                      requestBuild session
                      awaitStatus session "failed" 2
                  )
                    `finally` void (dupTo saved (Fd 2))
              readyTarget session >>= (@?= Nothing)
              writeFile (directory </> "image") "#!/bin/sh\nexit 0\n"
              requestBuild session
              void (awaitReady session)
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
          writeFile (environment resources) ("#!/bin/sh\necho started >> '" <> directory </> "started'\nwhile [ ! -f '" <> directory </> "release' ]; do sleep 0.02; done\ntouch '" <> directory </> "escaped'\n")
          wakes <- newIORef (0 :: Int)
          session <- newBuildSession (modifyIORef' wakes (+ 1)) (BuildContext resources config state)
          ( do
              requestBuild session
              await (doesFileExist (directory </> "started"))
              held <- buildStatus session
              assertEqual "held build state" (Just (String "building")) (field "state" held)
              assertBool "held build shows its log path" (maybe False isString (field "log_path" held))
              readIORef wakes >>= assertEqual "owner woken for the log path" 1
            )
            `finally` closeBuildSession session
          closeBuildSession session
          requestBuild session
          writeFile (directory </> "release") ""
          threadDelay 100000
          doesFileExist (directory </> "escaped") >>= (@?= False)
          readFile (directory </> "started") >>= assertEqual "closed session starts no compiler" "started\n"
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

awaitReady :: BuildSession -> IO ValidatedTarget
awaitReady session = do
  await (maybe False (const True) <$> readyTarget session)
  readyTarget session >>= maybe (fail "missing validated target") pure

awaitStatus :: BuildSession -> String -> Integer -> IO ()
awaitStatus session state generation =
  await ((\status -> field "state" status == Just (String (Text.pack state)) && field "generation" status == Just (Number (fromInteger generation))) <$> buildStatus session)

field :: KeyMap.Key -> Value -> Maybe Value
field name = \case
  Object fields -> KeyMap.lookup name fields
  _ -> Nothing

isString :: Value -> Bool
isString = \case
  String _ -> True
  _ -> False

await :: IO Bool -> IO ()
await predicate = do
  result <- timeout 3000000 loop
  result @?= Just ()
 where
  loop = predicate >>= \ready -> if ready then pure () else threadDelay 1000 >> loop
