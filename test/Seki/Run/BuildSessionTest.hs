{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.BuildSessionTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, tryTakeTMVar)
import Control.Exception (IOException, bracket, bracket_, finally, throwIO, try)
import Control.Monad (void)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef
import Data.List (isPrefixOf, sort)
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
import Seki.Run.Policy (ExternalInput (..), Policy, PolicyConfig (..), closePolicy, configureBuild, configureBuildSession, newPolicy, policyBuildStatus, policyIngress, policyRuntimeHandlers, policyValidatedTarget)
import Seki.Run.Restart
import Seki.Run.Runtime (RiverInputSnapshot (..), RiverRuntimeHandlers (..), RiverRuntimeSnapshot (..))
import Seki.Wire.Transport (ConnectionDiscoveryError (..), ConnectionTarget (..))
import System.Directory
import System.Environment (getExecutablePath, setEnv, unsetEnv)
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
        queued @?= BuildRunning 1 Superseded Nothing
        transitionBuild queued (BuildFinished (Left ("obsolete", Nothing))) @?= BuildRunning 2 OnRequest Nothing
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
          session <- requestedSession (pure ()) (BuildContext started (Just (Right launcher)) config state)
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
          session <- requestedSession (pure ()) (BuildContext running (Just (Right launcher)) config state)
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
    , testCase "a launcher that does not answer by its deadline is stopped and reaped" $
        fixture "exit 0" $ \(BuildContext _ _ _ state) directory -> do
          let launcher = directory </> "hanging-launcher"
              pidFile = directory </> "launcher.pid"
          writeScript launcher ("#!/bin/sh\necho $$ > '" <> pidFile <> "'\nexec sleep 600\n")
          outcome <- timeout 10000000 (try (installedResources 1 state launcher) :: IO (Either IOException Resources))
          assertBool ("the lookup ends with an error at its deadline: " <> show outcome) (maybe False (either (const True) (const False)) outcome)
          pid <- takeWhile (/= '\n') <$> readFile pidFile
          doesPathExist ("/proc/" <> pid) >>= assertBool ("launcher " <> pid <> " is gone") . not
    , testCase "a launcher that only repeats the asking session's variables has not answered" $
        fixture "exit 0" $ \(BuildContext started _ config state) directory -> do
          let launcher = directory </> "launcher"
          writeScript launcher "#!/bin/sh\nenv | grep '^SEKI_CONFIG_'\n"
          session <- requestedSession (pure ()) (BuildContext started (Just (Right launcher)) config state)
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
          session <- requestedSession (modifyIORef' wakes (+ 1)) (BuildContext started (Just (Right launcher)) config state)
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
          session <- requestedSession (pure ()) (BuildContext running (Just (Right launcher)) config state)
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
          session <- requestedSession (modifyIORef' wakes (+ 1)) context
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
          session <- requestedSession (pure ()) context
          ( do
              requestBuild session
              awaitStatus session "failed" 1
              readyTarget session >>= (@?= Nothing)
            )
            `finally` closeBuildSession session
    , testCase "diagnostic write failure does not abort the owner or future builds" $
        fixture "exit 78" $ \context directory -> do
          session <- requestedSession (pure ()) context
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
          let installedEnvironment = directory </> "installed-environment"
              installedTemplates = directory </> "installed-templates"
              installed = Resources "/store/installed/seki-0.2.0.0.tar.gz" "/store/installed/ghc" "/store/installed/cabal" installedEnvironment installedTemplates
          createSymbolicLink (environment started) installedEnvironment
          createSymbolicLink (templates started) installedTemplates
          target <- build installed config state
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
          sort [variable | variable@(name, _) <- env, "SEKI_CONFIG_" `isPrefixOf` name]
            @?= sort (("SEKI_CONFIG_LAUNCHER", "/installed/bin/seki") : resourceVariables installed)
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
    , testCase "a request during a startup build supersedes it, and its result starts the next generation on request" $ do
        let superseded = transitionBuild (BuildRunning 1 AtStartup Nothing) BuildRequested
        superseded @?= BuildRunning 1 Superseded Nothing
        transitionBuild superseded (BuildFinished (Left ("obsolete", Nothing))) @?= BuildRunning 2 OnRequest Nothing
    , testCase "a session that owes a startup build shows it as building before its owner manages, and runs no tool until then" $
        fixture "exit 0" $ \context@(BuildContext resources _ _ _) directory -> do
          writeScript (environment resources) (buildTool directory ("touch '" <> directory </> "started'"))
          session <- startupSession (pure ()) (directory </> "other") context
          ( do
              awaitStatus session "building" 1
              threadDelay 200000
              doesFileExist (directory </> "started") >>= assertEqual "the tool ran before the owner managed" False
              ownerManaged session False
              await (doesFileExist (directory </> "started"))
            )
            `finally` closeBuildSession session
    , testCase "a session that owes no startup build starts idle" $
        fixture "exit 0" $ \context _ -> do
          session <- requestedSession (pure ()) context
          (buildStatus session >>= (@?= Just (String "idle")) . field "state") `finally` closeBuildSession session
    , testCase "a startup build that reproduces the running image ends unchanged, wakes the owner, and keeps no copy" $
        fixture "exit 0" $ \context directory -> do
          wakes <- newIORef (0 :: Int)
          session <- startupSession (modifyIORef' wakes (+ 1)) (directory </> "image") context
          ( do
              ownerManaged session False
              awaitStatus session "unchanged" 1
              readyTarget session >>= (@?= Nothing)
              await ((>= 2) <$> readIORef wakes)
              status <- buildStatus session
              assertBool "the unchanged build shows its log path" (maybe False isString (field "log_path" status))
              candidates directory >>= assertEqual "copies of the reproduced image" []
            )
            `finally` closeBuildSession session
    , testCase "a startup build that differs from the running image is ready" $
        fixture "exit 0" $ \context directory -> do
          writeFile (directory </> "other") "the running image has other bytes"
          session <- startupSession (pure ()) (directory </> "other") context
          (ownerManaged session False >> awaitStatus session "validated" 1 >> void (awaitReady session)) `finally` closeBuildSession session
    , testCase "a running image that cannot be read counts as different" $
        fixture "exit 0" $ \context directory -> do
          session <- startupSession (pure ()) (directory </> "deleted-image") context
          (ownerManaged session False >> void (awaitReady session)) `finally` closeBuildSession session
    , testCase "a failed startup build reports its error and log, wakes the owner, and is never ready" $
        fixture "exit 78" $ \context directory -> do
          wakes <- newIORef (0 :: Int)
          session <- startupSession (modifyIORef' wakes (+ 1)) (directory </> "other") context
          ( do
              ownerManaged session False
              awaitStatus session "failed" 1
              status <- buildStatus session
              assertBool "the failure carries an error" (maybe False isString (field "error" status))
              assertBool "the failure carries its log path" (maybe False isString (field "log_path" status))
              await ((>= 2) <$> readIORef wakes)
              readyTarget session >>= (@?= Nothing)
            )
            `finally` closeBuildSession session
    , testCase "a request after an unchanged startup build restarts onto the same bytes" $
        fixture "exit 0" $ \context directory -> do
          session <- startupSession (pure ()) (directory </> "image") context
          ( do
              ownerManaged session False
              awaitStatus session "unchanged" 1
              requestBuild session
              awaitStatus session "validated" 2
              target <- awaitReady session
              readFile (validatedPath target) >>= \copy -> readFile (directory </> "image") >>= (@?= copy)
            )
            `finally` closeBuildSession session
    , testCase "a request during a startup build runs the tool twice in order, and only the second result is ready" $
        fixture "exit 0" $ \context@(BuildContext resources _ _ _) directory -> do
          let busy = directory </> "busy"
          writeScript (environment resources) (buildTool directory ("[ -e '" <> busy <> "' ] && echo overlap >> '" <> directory </> "overlaps'; touch '" <> busy <> "'; echo started >> '" <> directory </> "started'; while [ ! -f '" <> directory </> "release' ]; do sleep 0.02; done; rm '" <> busy <> "'"))
          writeFile (directory </> "other") "the running image has other bytes"
          session <- startupSession (pure ()) (directory </> "other") context
          ( do
              ownerManaged session False
              await (doesFileExist (directory </> "started"))
              requestBuild session
              held <- buildStatus session
              (field "generation" held, field "queued" held) @?= (Just (Number 1), Just (Number 2))
              readyTarget session >>= (@?= Nothing)
              writeFile (directory </> "release") ""
              awaitStatus session "validated" 2
              void (awaitReady session)
              readFile (directory </> "started") >>= (@?= "started\nstarted\n")
              doesFileExist (directory </> "overlaps") >>= assertEqual "two build tools at once" False
            )
            `finally` closeBuildSession session
    , testCase "a session without a build context answers the startup build with its reason" $ do
        session <- newBuildSession (pure ()) StartupBuild (pure "/unused") (Left "build context unavailable: XDG_STATE_HOME must be absolute")
        ( do
            ownerManaged session False
            awaitStatus session "failed" 1
            buildStatus session >>= assertBool "the reason reaches the status" . maybe False (\case String message -> "XDG_STATE_HOME must be absolute" `Text.isInfixOf` message; _ -> False) . field "error"
          )
          `finally` closeBuildSession session
    , testCase "a startup build that reproduces a candidate that crashed is not handed over, and the marker stays" $
        heldBack Crashed "crashed before it proved healthy"
    , testCase "a startup build that reproduces a candidate whose second trial left no record is not handed over" $
        heldBack SecondTrial "ended two starts before it proved healthy"
    , testCase "a startup build that reproduces a candidate whose first trial left no record is ready for one more trial" $
        fixture "exit 0" $ \context directory -> do
          let unrecorded = directory </> "candidate-unrecorded"
          readFile (directory </> "image") >>= writeFile unrecorded
          writeFile (directory </> "other") "the default runtime has other bytes"
          markUnproven directory (Unproven unrecorded FirstTrial)
          session <- startupSession (pure ()) (directory </> "other") context
          ( do
              ownerManaged session False
              target <- awaitReady session
              readFile (validatedPath target) >>= \copy -> readFile unrecorded >>= (@?= copy)
              readUnproven directory >>= (@?= Just (Unproven unrecorded FirstTrial))
            )
            `finally` closeBuildSession session
    , testCase "a request under the default runtime tries the candidate that crashed again" $
        fixture "exit 0" $ \context directory -> do
          let dead = directory </> "candidate-dead"
          readFile (directory </> "image") >>= writeFile dead
          markUnproven directory (Unproven dead Crashed)
          session <- requestedSession (pure ()) context
          (requestBuild session >> void (awaitReady session)) `finally` closeBuildSession session
    , testCase "an image stays on trial until its startup build ends and a manage sequence follows the one that ran its hook" $
        fixture "exit 0" $ \context@(BuildContext resources _ _ _) directory -> do
          let image = directory </> "image"
          writeScript (environment resources) (buildTool directory ("while [ ! -f '" <> directory </> "release' ]; do sleep 0.02; done"))
          markUnproven directory (Unproven image FirstTrial)
          session <- startupSession (pure ()) image context
          ( do
              ownerManaged session True
              ownerManaged session True
              readUnproven directory >>= assertEqual "on trial while the startup build runs" (Just (Unproven image FirstTrial))
              writeFile (directory </> "release") ""
              awaitStatus session "unchanged" 1
              ownerManaged session True
              readUnproven directory >>= assertEqual "proved healthy" Nothing
            )
            `finally` closeBuildSession session
    , testCase "an image that owes no build stays on trial until a manage sequence follows the one that ran its hook" $
        fixture "exit 0" $ \context directory -> do
          let image = directory </> "image"
          markUnproven directory (Unproven image FirstTrial)
          session <- newBuildSession (pure ()) NoStartupBuild (pure image) (Right context)
          ( do
              ownerManaged session False
              ownerManaged session False
              readUnproven directory >>= assertEqual "the hook has not run" (Just (Unproven image FirstTrial))
              ownerManaged session True
              readUnproven directory >>= assertEqual "the hook ran in this sequence" (Just (Unproven image FirstTrial))
              ownerManaged session True
              readUnproven directory >>= assertEqual "proved healthy" Nothing
            )
            `finally` closeBuildSession session
    , testCase "a marker that names another image survives this image's proof" $
        fixture "exit 0" $ \context directory -> do
          let dead = directory </> "candidate-dead"
          writeFile dead "died"
          markUnproven directory (Unproven dead FirstTrial)
          session <- newBuildSession (pure ()) NoStartupBuild (pure (directory </> "image")) (Right context)
          (ownerManaged session True >> ownerManaged session True >> readUnproven directory >>= (@?= Just (Unproven dead FirstTrial))) `finally` closeBuildSession session
    , testCase "a refused ready target becomes a failure with the refusal" $
        fixture "exit 0" $ \context _ -> do
          session <- requestedSession (pure ()) context
          ( do
              requestBuild session
              void (awaitReady session)
              refuseTarget session "XDG_RUNTIME_DIR is unset or not absolute"
              awaitStatus session "failed" 1
              readyTarget session >>= (@?= Nothing)
              buildStatus session >>= (@?= Just (String "restart refused: XDG_RUNTIME_DIR is unset or not absolute")) . field "error"
            )
            `finally` closeBuildSession session
    , testCase "an unchanged startup build never checks a restart target" $
        fixture "exit 0" $ \context directory -> do
          checks <- newIORef (0 :: Int)
          policy <- startupPolicy checks (Right ()) (directory </> "image") context
          ( do
              manage policy
              await ((== Just (Just (String "unchanged"))) . fmap (field "state") <$> policyBuildStatus policy)
              manage policy >> manage policy
              readIORef checks >>= assertEqual "restart checks" 0
            )
            `finally` closePolicy policy
    , testCase "a startup build that differs checks its target at the next manage sequence" $
        fixture "exit 0" $ \context directory -> do
          checks <- newIORef (0 :: Int)
          policy <- startupPolicy checks (Right ()) (directory </> "other") context
          ( do
              manage policy
              await (isJust <$> policyValidatedTarget policy)
              manage policy
              readIORef checks >>= assertEqual "restart checks" 1
            )
            `finally` closePolicy policy
    , testCase "a refused startup target is reported once and not checked again" $
        fixture "exit 0" $ \context directory -> do
          checks <- newIORef (0 :: Int)
          policy <- startupPolicy checks (Left NoStateDirectory) (directory </> "other") context
          ( do
              manage policy
              await (isJust <$> policyValidatedTarget policy)
              manage policy >> manage policy >> manage policy
              readIORef checks >>= assertEqual "restart checks" 1
              policyBuildStatus policy >>= (@?= Just (Just (String "restart refused: XDG_RUNTIME_DIR is unset or not absolute"))) . fmap (field "error")
            )
            `finally` closePolicy policy
    , testCase "close cancels and reaps held compiler process group" $
        fixture "exit 0" $ \(BuildContext resources _ config state) directory -> do
          writeFile (environment resources) ("#!/bin/sh\necho started >> '" <> directory </> "started'\nwhile [ ! -f '" <> directory </> "release' ]; do sleep 0.02; done\ntouch '" <> directory </> "escaped'\n")
          wakes <- newIORef (0 :: Int)
          session <- requestedSession (modifyIORef' wakes (+ 1)) (BuildContext resources Nothing config state)
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

{- | The default runtime's startup build reproduces a candidate whose trial
ended this way. It ends failed with this reason, keeps no copy, and leaves
the marker.
-}
heldBack :: Trial -> Text.Text -> IO ()
heldBack trial reason = fixture "exit 0" $ \context directory -> do
  let dead = directory </> "candidate-dead"
  readFile (directory </> "image") >>= writeFile dead
  writeFile (directory </> "other") "the default runtime has other bytes"
  markUnproven directory (Unproven dead trial)
  session <- startupSession (pure ()) (directory </> "other") context
  ( do
      ownerManaged session False
      awaitStatus session "failed" 1
      buildStatus session >>= assertBool "the error names the candidate and what happened to it" . maybe False (\case String message -> Text.pack dead `Text.isInfixOf` message && reason `Text.isInfixOf` message; _ -> False) . field "error"
      readyTarget session >>= (@?= Nothing)
      candidates directory >>= assertEqual "only the held candidate remains" ["candidate-dead"]
      readUnproven directory >>= (@?= Just (Unproven dead trial))
    )
    `finally` closeBuildSession session

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

requestedSession :: IO () -> BuildContext -> IO BuildSession
requestedSession wake = newBuildSession wake NoStartupBuild getExecutablePath . Right

-- | A session that owes a startup build and runs the given image.
startupSession :: IO () -> FilePath -> BuildContext -> IO BuildSession
startupSession wake image = newBuildSession wake StartupBuild (pure image) . Right

-- | A build tool that runs the shell text for its build step and resolves the fixture's image.
buildTool :: FilePath -> String -> String
buildTool directory step = "#!/bin/sh\nfor arg do\ncase \"$arg\" in\nbuild) " <> step <> "; exit 0;;\nlist-bin) printf '%s\\n' '" <> directory </> "image'; exit 0;;\nesac\ndone\nexit 2\n"

candidates :: FilePath -> IO [FilePath]
candidates directory = sort . filter ("candidate" `isPrefixOf`) <$> listDirectory directory

-- | A policy that owes a startup build, runs the given image, and counts its restart checks.
startupPolicy :: IORef Int -> Either RestartRefusal () -> FilePath -> BuildContext -> IO (Policy Int Layout.Full)
startupPolicy checks answer image context = do
  policy <- newPolicy (PolicyConfig [] [] Layout.Full [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [] Nothing (\_ -> modifyIORef' checks (+ 1) >> pure answer) (\_ -> pure ())) >>= either (assertFailure . show) pure
  configureBuildSession policy StartupBuild (pure image) (Right context)
  pure policy

manage :: Policy Int Layout.Full -> IO ()
manage policy = void (prepareRiverManagePlan (policyRuntimeHandlers policy) (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) []))

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
