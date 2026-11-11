{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.BuildSessionTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, tryTakeTMVar)
import Control.Exception (bracket, bracket_, finally, throwIO)
import Control.Monad (void)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef
import Data.List (isPrefixOf)
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
import System.Environment (setEnv, unsetEnv)
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
        fixture "exit 0" $ \context@(BuildContext resources _ _ _) directory -> do
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
    , testCase "a restart requested while a target is ready builds again instead of stopping" $
        fixture "exit 0" $ \context _ -> do
          checks <- newIORef (0 :: Int)
          policy <- newPolicy (PolicyConfig [] [] Layout.Full [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [] Nothing (\_ -> modifyIORef' checks (+ 1) >> pure (Right ())) (\_ -> pure ())) >>= either (assertFailure . show) pure
          configureBuild policy context
          let handlers = policyRuntimeHandlers policy
              snapshot = RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) []
              manageAfter action = do
                submitExternal (policyIngress policy) (ExternalInput action Nothing) >>= (@?= Accepted)
                void (prepareRiverManagePlan handlers snapshot)
          ( do
              manageAfter restart
              await (isJust <$> policyValidatedTarget policy)
              manageAfter restart
              readIORef checks >>= assertEqual "no restart check while the requested build runs" 0
              await (isJust <$> policyValidatedTarget policy)
              manageAfter (pure ())
              readIORef checks >>= assertEqual "the next manage checks the new target" 1
            )
            `finally` closePolicy policy
    , testCase "existing user files do not open template resources" $
        fixture "exit 0" $ \(BuildContext resources _ config _) _ -> do
          generate resources config
          writeFile (config </> "Main.hs") "user content"
          generate resources{templates = "/missing/templates"} config
          readFile (config </> "Main.hs") >>= (@?= "user content")
    , testCase "an existing cabal.project follows the installed archive and compiler" $
        fixture "exit 0" $ \(BuildContext resources _ config _) _ -> do
          let project = config </> "cabal.project"
              installed = resources{archive = "/store/new-seki-config-source/seki-0.2.0.0.tar.gz", compiler = "/store/new-ghc/bin/ghc"}
          writeFile project "packages: . \"/store/old-seki-config-source/seki-0.1.0.0.tar.gz\" \"../mine/mine-1.0.tar.gz\"\nwith-compiler: /store/old-ghc/bin/ghc\noptimization: 1\n"
          generate installed config
          readFile project >>= (@?= "packages: . \"/store/new-seki-config-source/seki-0.2.0.0.tar.gz\" \"../mine/mine-1.0.tar.gz\"\nwith-compiler: /store/new-ghc/bin/ghc\noptimization: 1\n")
    , testCase "a cabal.project that names the installed resources stays byte for byte" $
        fixture "exit 0" $ \(BuildContext resources _ config _) _ -> do
          let project = config </> "cabal.project"
              installed = resources{archive = "/store/seki-config-source/seki-0.1.0.0.tar.gz", compiler = "/store/ghc/bin/ghc"}
              current = "-- mine\npackages: . \"/store/seki-config-source/seki-0.1.0.0.tar.gz\"\nwith-compiler: /store/ghc/bin/ghc"
          writeFile project current
          generate installed config
          readFile project >>= (@?= current)
    , testCase "a with-compiler path to another kind of compiler stays" $
        fixture "exit 0" $ \(BuildContext resources _ config _) _ -> do
          let project = config </> "cabal.project"
              chosen = "with-compiler: /opt/compilers/ghc-9.10.3\n"
          writeFile project chosen
          generate resources{compiler = "/store/ghc/bin/ghc"} config
          readFile project >>= (@?= chosen)
    , testCase "the launcher's answer reads back as the resources it was rendered from" $
        parseResources (renderResources delivery) @?= Right delivery
    , testCase "an answer may carry lines that this seki does not know" $
        parseResources ("bash: warning: setlocale\nSEKI_CONFIG_DEFAULT=/store/seki/bin/seki-default\n" <> renderResources delivery) @?= Right delivery
    , testCase "an answer that lacks a resource names the missing variable" $
        parseResources (unlines (filter (not . isPrefixOf "SEKI_CONFIG_GHC=") (lines (renderResources delivery)))) @?= Left "SEKI_CONFIG_GHC must name an absolute installed resource"
    , testCase "an answer with a relative path is refused" $
        parseResources (renderResources delivery{templates = "share/templates"}) @?= Left "SEKI_CONFIG_TEMPLATES must name an absolute installed resource"
    , testCase "each build asks the launcher again and follows the delivery installed now" $
        fixture "exit 0" $ \(BuildContext started _ config state) directory -> do
          let first = started{archive = "/store/first/seki-0.1.0.0.tar.gz", compiler = "/store/first-ghc/bin/ghc"}
              second = started{archive = "/store/second/seki-0.2.0.0.tar.gz", compiler = "/store/second-ghc/bin/ghc"}
              project = config </> "cabal.project"
              launcher = directory </> "launcher"
              projectFor resources = "packages: . " <> show (archive resources) <> "\nwith-compiler: " <> compiler resources <> "\n"
          writeScript (directory </> "first-launcher") (answering first)
          writeScript (directory </> "second-launcher") (answering second)
          pointAt launcher (directory </> "first-launcher")
          writeFile project (projectFor started{archive = "/store/started/seki-0.1.0.0.tar.gz", compiler = "/store/started-ghc/bin/ghc"})
          session <- newBuildSession (pure ()) (BuildContext started (Just (Right launcher)) config state)
          ( do
              requestBuild session
              awaitStatus session "validated" 1
              readFile project >>= (@?= projectFor first)
              pointAt launcher (directory </> "second-launcher")
              requestBuild session
              awaitStatus session "validated" 2
              readFile project >>= (@?= projectFor second)
              target <- awaitReady session
              readFile (validatedLog target) >>= assertBool "the build log names the archive" . isPrefixOf "seki: building against /store/second/seki-0.2.0.0.tar.gz\n"
            )
            `finally` closeBuildSession session
    , testCase "a launcher that cannot answer leaves the build on the resources the session runs, and the build log says so" $
        fixture "exit 0" $ \(BuildContext started _ config state) directory -> do
          let running = started{archive = "/store/running/seki-0.1.0.0.tar.gz"}
              project = config </> "cabal.project"
              launcher = directory </> "launcher"
          writeScript launcher "#!/bin/sh\necho 'usage: seki [--check | --recompile | --restore FILE]' >&2\nexit 1\n"
          writeFile project "packages: . \"/store/older/seki-0.1.0.0.tar.gz\"\n"
          session <- newBuildSession (pure ()) (BuildContext running (Just (Right launcher)) config state)
          ( do
              requestBuild session
              target <- awaitReady session
              readFile project >>= (@?= "packages: . \"/store/running/seki-0.1.0.0.tar.gz\"\n")
              logged <- lines <$> readFile (validatedLog target)
              take 1 logged @?= ["seki: building against /store/running/seki-0.1.0.0.tar.gz"]
              assertBool ("the log says why the build keeps the session's resources: " <> show (take 2 logged)) (any (isPrefixOf ("seki: cannot read the installed delivery through " <> launcher)) (take 2 logged))
            )
            `finally` closeBuildSession session
    , testCase "a launcher setting that is not an absolute path leaves builds on the resources the session runs, with a warning" $
        fixture "exit 0" $ \(BuildContext started _ _ _) _ ->
          withVariables (("SEKI_CONFIG_LAUNCHER", "bin/seki") : resourceVariables started) $ do
            context <- installedContext >>= maybe (assertFailure "no build context") pure
            Resolution resources warning <- resolve context
            resources @?= started
            warning @?= Just ("SEKI_CONFIG_LAUNCHER must name an absolute installed resource; builds use " <> archive started <> ", which this session runs")
    , testCase "a launcher that only repeats the asking session's variables has not answered" $
        fixture "exit 0" $ \(BuildContext started _ config state) directory -> do
          let launcher = directory </> "launcher"
          writeScript launcher "#!/bin/sh\nenv | grep '^SEKI_CONFIG_'\n"
          session <- newBuildSession (pure ()) (BuildContext started (Just (Right launcher)) config state)
          withVariables (resourceVariables started{archive = "/store/echo/seki-0.1.0.0.tar.gz"}) $
            ( do
                requestBuild session
                target <- awaitReady session
                logged <- lines <$> readFile (validatedLog target)
                take 1 logged @?= ["seki: building against /archive"]
                assertBool ("the log reports the launcher: " <> show (take 2 logged)) (any (isPrefixOf ("seki: cannot read the installed delivery through " <> launcher)) (take 2 logged))
            )
              `finally` closeBuildSession session
    , testCase "the status warns that the launcher cannot answer, before any build and after one" $
        fixture "exit 0" $ \(BuildContext started _ config state) directory -> do
          let launcher = directory </> "missing-launcher"
              warned status = case field "warning" status of
                Just (String message) -> ("cannot read the installed delivery through " <> Text.pack launcher) `Text.isPrefixOf` message
                _ -> False
          wakes <- newIORef (0 :: Int)
          session <- newBuildSession (modifyIORef' wakes (+ 1)) (BuildContext started (Just (Right launcher)) config state)
          ( do
              await (warned <$> buildStatus session)
              idle <- buildStatus session
              (field "state" idle, field "generation" idle) @?= (Just (String "idle"), Just (Number 0))
              await ((>= 1) <$> readIORef wakes)
              requestBuild session
              void (awaitReady session)
              buildStatus session >>= assertBool "the warning stays after the build" . warned
            )
            `finally` closeBuildSession session
    , testCase "the status warns while the session does not run the installed delivery, and not once it does" $
        fixture "exit 0" $ \(BuildContext started _ config state) directory -> do
          let running = started{archive = "/store/running/seki-0.1.0.0.tar.gz"}
              installed = started{archive = "/store/installed/seki-0.2.0.0.tar.gz"}
              launcher = directory </> "launcher"
          writeScript (directory </> "installed-launcher") (answering installed)
          writeScript (directory </> "running-launcher") (answering running)
          pointAt launcher (directory </> "installed-launcher")
          session <- newBuildSession (pure ()) (BuildContext running (Just (Right launcher)) config state)
          ( do
              await ((== Just (String "this session does not run the installed delivery (installed archive /store/installed/seki-0.2.0.0.tar.gz, session archive /store/running/seki-0.1.0.0.tar.gz); a recompile builds against the installed delivery")) . field "warning" <$> buildStatus session)
              pointAt launcher (directory </> "running-launcher")
              requestBuild session
              void (awaitReady session)
              buildStatus session >>= (@?= Just Null) . field "warning"
            )
            `finally` closeBuildSession session
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
        fixture "exit 0" $ \(BuildContext resources _ config state) directory -> do
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
    , testCase "a restart hands the next image the resources its candidate was built from" $
        fixture "exit 0" $ \(BuildContext started _ config state) directory -> do
          target <- build started{archive = "/store/installed/seki-0.2.0.0.tar.gz"} config state
          handed <- newIORef []
          let launch =
                Launch
                  { launchFirstTarget = Left XdgRuntimeDirMissing
                  , launchReconnectTarget = Left XdgRuntimeDirMissing
                  , launchTarget = Right (directory </> "profile")
                  , launchStateDir = Right directory
                  , launchExec = \_ _ env -> writeIORef handed env >> throwIO (userError "injected exec failure")
                  }
          _ <- withVariables (("SEKI_CONFIG_LAUNCHER", "/installed/bin/seki") : resourceVariables started) (execValidatedRestart launch target (directory </> "saved"))
          env <- readIORef handed
          [value | ("SEKI_CONFIG_ARCHIVE", value) <- env] @?= ["/store/installed/seki-0.2.0.0.tar.gz"]
          [value | ("SEKI_CONFIG_ENV", value) <- env] @?= [environment started]
          lookup "SEKI_CONFIG_LAUNCHER" env @?= Just "/installed/bin/seki"
    , testCase "a checked replacement does not depend on the old executable still existing" $
        fixture "exit 0" $ \(BuildContext resources _ config state) directory -> do
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
        fixture "exit 0" $ \(BuildContext resources _ config state) directory -> do
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
        fixture "exit 0" $ \(BuildContext resources _ config state) directory -> do
          writeFile (environment resources) ("#!/bin/sh\necho started >> '" <> directory </> "started'\nwhile [ ! -f '" <> directory </> "release' ]; do sleep 0.02; done\ntouch '" <> directory </> "escaped'\n")
          wakes <- newIORef (0 :: Int)
          session <- newBuildSession (modifyIORef' wakes (+ 1)) (BuildContext resources Nothing config state)
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

delivery :: Resources
delivery = Resources "/store/source/seki-0.1.0.0.tar.gz" "/store/ghc/bin/ghc" "/store/cabal/bin/cabal" "/store/env/bin/seki-config-env" "/store/templates"

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
  body (BuildContext (Resources "/archive" "/compiler" "/cabal" wrapper templatesPath) Nothing directory directory) directory

answering :: Resources -> String
answering resources = "#!/bin/sh\n[ \"$1\" = --resources ] || exit 64\ncat <<'ANSWER'\n" <> renderResources resources <> "ANSWER\n"

writeScript :: FilePath -> String -> IO ()
writeScript path script = writeFile path script >> setFileMode path 0o700

pointAt :: FilePath -> FilePath -> IO ()
pointAt link target = createSymbolicLink target (link <> ".next") >> renameFile (link <> ".next") link

withVariables :: [(String, String)] -> IO a -> IO a
withVariables variables = bracket_ (mapM_ (uncurry setEnv) variables) (mapM_ (unsetEnv . fst) variables)

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
