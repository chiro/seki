{-# LANGUAGE OverloadedStrings #-}

module Seki.Latency.Live (runWM, runLive) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (foldM, forM, unless, void)
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Seki.Fixture.Probe
import Seki.Integration.Harness
import Seki.Internal.Core.Action (sendMessage, windows)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize)
import Seki.Internal.Core.Layout (Resize (..))
import Seki.Internal.Core.ManageHook (appId, doFloat, isDialog, (-->), (=?))
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Core.Scratchpad.Types (Scratchpad (..))
import Seki.Internal.Core.StackSet qualified as S
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Latency.Commands
import Seki.Latency.Workload
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.ConfigBuild (BuildContext (..), Resources (..))
import Seki.Run.IPC (ipcPath, withIPCAt)
import Seki.Run.Inventory (riverInventoryWindows)
import Seki.Run.Latency
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Run.Sequence (RiverSequencePhase (AwaitingManageOrRender))
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (ConnectionTarget (UnixSocketPath))
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Environment (getEnv, getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR1, sigUSR2, signalProcess)
import System.Process (CreateProcess (..), StdStream (UseHandle), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

runWM :: WorkloadName -> FilePath -> FilePath -> IO ()
runWM workload socket directory = do
  recorder <- newRecorder 100000
  policy <- newPolicy config >>= requireRight "latency policy"
  serve <- case workloadDriver workload of
    KeyDriver -> pure id
    IPCDriver -> do
      getEnv "XDG_RUNTIME_DIR" >>= buildFailingCheck . (</> "build") >>= configureBuild policy
      path <- ipcPath
      pure (withIPCAt path policy . (publish (directory </> "wm.ipc") path >>))
  let handlers = (policyRuntimeHandlers policy){riverSequenceRecorder = Just recorder}
  session <- openRiverSessionAt (UnixSocketPath socket) defaultObjectLimit handlers >>= requireRight "latency WM open"
  pid <- getProcessID
  writeFile (directory </> "wm.pid") (show pid)
  _ <-
    installHandler
      sigUSR1
      ( Catch $ do
          _ <-
            waitUntil
              "WM drained"
              assertionTimeoutMicroseconds
              ( do
                  inventory <- readRiverInventory session
                  state <- readPolicyState policy
                  phase <- readRiverSequencePhase session
                  pure
                    ( null (riverInventoryWindows inventory) && case coreWorkspaces state of
                        AttachedState ss _ -> null (S.allWindows ss) && phase == Right AwaitingManageOrRender
                        _ -> False
                    )
              )
              id
          writeFile (directory </> "wm.drained") "true\n"
      )
      Nothing
  snapshots <- newIORef (0 :: Int)
  _ <-
    installHandler
      sigUSR2
      ( Catch $ do
          _ <- waitUntil "completed WM render before RTS checkpoint" assertionTimeoutMicroseconds (readRiverSequencePhase session) (== Right AwaitingManageOrRender)
          index <- atomicModifyIORef' snapshots (\n -> (n + 1, n))
          stats <- getRTSStats
          let path = directory </> ("wm-checkpoint-" <> show index <> ".json")
          BL.writeFile
            (path <> ".tmp")
            ( encode
                ( object
                    [ "allocated_bytes" .= allocated_bytes stats
                    , "gcs" .= gcs stats
                    , "major_gcs" .= major_gcs stats
                    , "gc_elapsed_ns" .= gc_elapsed_ns stats
                    , "gc_cpu_ns" .= gc_cpu_ns stats
                    ]
                )
            )
          renameFile (path <> ".tmp") path
      )
      Nothing
  result <- serve (runRiverSession session)
  recording <- readRecording recorder
  stats <- getRTSStats
  BL.writeFile
    (directory </> "wm.json")
    ( encode
        ( object
            [ "mode" .= ("wm" :: String)
            , "pid" .= (fromIntegral pid :: Int)
            , "clean_exit" .= (result == Right RiverEnded)
            , "successful" .= recordingSuccessful recording
            , "failed" .= recordingFailed recording
            , "dropped" .= recordingDropped recording
            , "samples" .= [object ["kind" .= kind (sampleKind s), "start_ns" .= sampleStartedNS s, "end_ns" .= sampleSubmittedNS s] | s <- recordingSamples recording]
            , "allocated_bytes" .= allocated_bytes stats
            , "max_live_bytes" .= max_live_bytes stats
            , "gcs" .= gcs stats
            , "major_gcs" .= major_gcs stats
            , "gc_cpu_ns" .= gc_cpu_ns stats
            , "gc_elapsed_ns" .= gc_elapsed_ns stats
            , "nonmoving_sync_max_ns" .= nonmoving_gc_sync_max_elapsed_ns stats
            ]
        )
    )
  unless (result == Right RiverEnded) (fail (show result))
 where
  kind ManageSequence = "manage" :: String
  kind RenderSequence = "render"
  config :: PolicyConfig String Tall
  config =
    PolicyConfig
      { policyScratchpads = [Scratchpad padName "the driver opens the pad" (appId =? padAppId) padRect | PadRole _ <- windowRoles workload]
      , policyInputRules = []
      , policyLayout = Tall 1 (1 / 10) (1 / 2)
      , policyTags = ["1", "2"]
      , policyBindings =
          zipWith
            (\n action -> (RiverKeyBinding n (River.RiverSeatV1ModifiersFlags 0) (Just 0), action))
            [0xffbe ..]
            [windows S.focusDown, windows S.focusUp, windows (S.greedyView "2"), windows (S.greedyView "1"), sendMessage Shrink, sendMessage Expand]
      , policySceneStyle = defaultSceneStyle
      , policyManageHook = isDialog --> doFloat
      , policyStartupHook = pure ()
      , policyPointerBindings = []
      , policyRestore = Nothing
      , policyCheckRestart = const (pure (Right ()))
      , policyLog = \_ -> pure ()
      , policyLaunch = \_ -> fail "unexpected spawn"
      }

padRect :: OutputRect
padRect = let (x, y, w, h) = padContentBox in either (error . show) OutputRect (mkSize (fromIntegral w) (fromIntegral h) >>= mkRect (Point (fromIntegral x) (fromIntegral y)))

checkFailure :: String
checkFailure = "seki-latency stub: the built configuration fails its check"

buildFailingCheck :: FilePath -> IO BuildContext
buildFailingCheck directory = do
  createDirectoryIfMissing True directory
  mapM_ (\name -> writeFile (directory </> name) "") ["Main.hs", "seki-user.cabal", "cabal.project"]
  let stub = directory </> "compiler"
      candidate = directory </> "built"
  writeFile candidate ("#!/bin/sh\necho '" <> checkFailure <> "' >&2\nexit 1\n")
  writeFile stub ("#!/bin/sh\ncase \"$3\" in\n  build) exit 0 ;;\n  list-bin) echo '" <> candidate <> "' ;;\n  *) exit 2 ;;\nesac\n")
  mapM_ (`setFileMode` 0o700) [stub, candidate]
  pure (BuildContext Resources{archive = "/unused", compiler = "/unused", cabal = "cabal", environment = stub, templates = directory} Nothing directory directory)

runLive :: FilePath -> WorkloadName -> Word64 -> Int -> String -> IO ()
runLive directory workload seed steps collector = do
  unless (steps > 0 && steps <= 10000 && collector `elem` ["normal", "nonmoving", "profile"]) (fail "invalid live workload")
  requireExecutables ["river", "wtype", "wlr-randr"]
  createDirectoryIfMissing True directory
  executable <- getExecutablePath
  host <- getEnvironment
  withSystemTempDirectory "seki-latency" $ \temp -> do
    let runtime = temp </> "runtime"
        config = temp </> "config"
        initSocket = temp </> "init.sock"
    createPrivateDirectory runtime
    createPrivateDirectory config
    let environment = fixtureEnvironment host runtime config
        riverEnv = filter ((/= "WLR_RENDERER") . fst) environment <> [("WLR_BACKENDS", "headless"), ("WLR_HEADLESS_OUTPUTS", "1"), ("WLR_RENDERER", "pixman")]
    withUnixListener initSocket $ \listener ->
      bracket (startManagedProcess "river" riverEnv (proc "river" (riverArguments False executable initSocket))) stopManagedProcess $ \river -> do
        riverPid <- managedProcessPid river
        driverPid <- fromIntegral <$> getProcessID
        let baseProcesses = [("river", riverPid), ("driver", Just driverPid)]
        writeProcesses directory baseProcesses
        names <- waitForRiverInit listener river
        let clientEnv = environment <> [("WAYLAND_DISPLAY", nestedWaylandDisplay names)]
            socket = runtime </> nestedWaylandDisplay names
            flags =
              ["-N1", "-T", "-s"] <> case collector of
                "nonmoving" -> ["--nonmoving-gc"]
                "profile" -> ["-l", "-ol" <> directory </> "wm.eventlog"]
                _ -> []
        withFile (directory </> "wm.stdout") WriteMode $ \out ->
          withFile (directory </> "wm.rts") WriteMode $ \err ->
            bracket
              (createProcess (proc executable (["wm", socket, directory, workloadText workload, "+RTS"] <> flags <> ["-RTS"])){env = Just clientEnv, std_out = UseHandle out, std_err = UseHandle err, create_group = True})
              (\(_, _, _, process) -> terminateProcess process >> void (waitForProcess process))
              $ \(_, _, _, process) -> do
                wmPid <- getPid process
                let wmProcesses = ("wm", fmap fromIntegral wmPid) : baseProcesses
                writeProcesses directory wmProcesses
                _ <- waitUntil "WM connected" assertionTimeoutMicroseconds (doesFileExist (directory </> "wm.pid")) id
                outputs <- outputNames socket
                name <- case outputs of [(_, value)] -> pure value; _ -> fail "expected one native output"
                void (runProcessCapture clientEnv "wlr-randr" ["--output", Text.unpack name, "--custom-mode", "800x600", "--pos", "0,0", "--scale", "1"])
                bracket (startManagedProcess "keyboard" clientEnv (proc "wtype" ["-s", "3600000"])) stopManagedProcess $ \keeper -> do
                  keeperPid <- managedProcessPid keeper
                  writeProcesses directory (("keyboard", keeperPid) : wmProcesses)
                  withProbe socket $ \probe -> do
                    clients <- foldM (\opened role -> (\w -> opened <> [w]) <$> openRole probe opened role) [] (windowRoles workload)
                    let byIndex = Map.fromList (zip [0 ..] (reverse clients))
                        indexOf = Map.fromList (zip (reverse clients) [0 ..])
                        observe = do
                          owner <- keyboardOwner <$> keyboardSnapshot probe
                          sizes <- committedSizes probe
                          pure (Observation (owner >>= (`Map.lookup` indexOf)) (Map.fromList [(i, size) | (w, size) <- Map.toList sizes, Just i <- [Map.lookup w indexOf]]))
                        await model = void (waitUntil "native observation" assertionTimeoutMicroseconds observe (== expectedObservation model))
                        key number = runInputDriver clientEnv ["-k", "F" <> show (number :: Int)]
                        drive model step = case step of
                          EnterFullscreen -> requestFullscreen probe (byIndex Map.! modelFocus model)
                          ExitFullscreen -> mapM_ (requestExitFullscreen probe . (byIndex Map.!)) (modelFullscreen model)
                          _ -> key (stepCode step + 1)
                        withDriver body = case workloadDriver workload of
                          KeyDriver -> body (\model step _ -> [] <$ drive model step)
                          IPCDriver -> do
                            _ <- waitUntil "WM serving IPC" assertionTimeoutMicroseconds (doesFileExist (directory </> "wm.ipc")) id
                            path <- readFile (directory </> "wm.ipc")
                            withCommands path (\c -> body (\_ step next -> timeCommand c observe next step))
                    withDriver $ \act -> do
                      let perform (model, timed) step = do
                            let next = stepModel model step
                            samples <- act model step next
                            await next
                            pure (next, foldl' (\m (endpoint, ns) -> Map.insertWith (<>) endpoint [ns] m) timed samples)
                          awaitPicture model =
                            void (waitUntil "native picture" assertionTimeoutMicroseconds (pictureMismatch (expectedLayers model) <$> captureOutput socket name) (== Nothing))
                          run model = foldM perform (model, Map.empty)
                          start0 = initialModel workload
                      await start0
                      awaitPicture start0
                      (warmed, _) <- run start0 (take 24 (plan workload seed start0))
                      let measured = take steps (plan workload (seed + 1) warmed)
                      checkpoint wmPid 0
                      start <- getMonotonicTimeNSec
                      (final, timings) <- run warmed measured
                      checkpoint wmPid 1
                      end <- getMonotonicTimeNSec
                      awaitPicture final
                      (restored, _) <- run final (restoreSteps final)
                      keyCount <- keyboardSnapshot probe
                      runInputDriver clientEnv ["q"]
                      _ <- waitUntil "focused client receives q" assertionTimeoutMicroseconds (keyboardSnapshot probe) $ \after ->
                        all (\client -> Map.findWithDefault 0 client (pressedKeys after) == Map.findWithDefault 0 client (pressedKeys keyCount) + if client == byIndex Map.! modelFocus restored then 1 else 0) clients
                      awaitPicture restored
                      BL.writeFile
                        (directory </> "driver.json")
                        ( encode
                            ( object
                                ( [ "mode" .= ("live" :: String)
                                  , "workload" .= workloadText workload
                                  , "seed" .= seed
                                  , "steps" .= steps
                                  , "observed_actions" .= steps
                                  , "windows" .= length clients
                                  , "warmup" .= (24 :: Int)
                                  , "start_ns" .= start
                                  , "end_ns" .= end
                                  , "final_model"
                                      .= object
                                        [ "focus" .= modelFocus final
                                        , "shown" .= modelShown final
                                        , "ratio" .= show (modelRatio final)
                                        , "fullscreen" .= modelFullscreen final
                                        , "raise" .= modelRaise final
                                        ]
                                  , "exercised" .= object [Key.fromString n .= c | (n, c) <- exerciseCounts (exercised warmed measured)]
                                  , "native_checks" .= True
                                  , "action_codes" .= map stepCode measured
                                  , "valid" .= True
                                  ]
                                    <> endpointFields workload measured timings
                                )
                            )
                        )
                case wmPid of
                  Nothing -> fail "WM PID unavailable"
                  Just pid -> signalProcess sigUSR1 pid
                _ <- waitUntil "WM close reconciliation complete" assertionTimeoutMicroseconds (doesFileExist (directory </> "wm.drained")) id
                stopManagedProcess river
                exit <- timeout assertionTimeoutMicroseconds (waitForProcess process)
                unless (exit == Just ExitSuccess) (fail ("WM did not exit cleanly: " <> show exit))
                renderManagedProcessLogs river >>= writeFile (directory </> "river.log")
 where
  checkpoint pid index = do
    maybe (fail "WM PID unavailable") (signalProcess sigUSR2) pid
    let path = directory </> ("wm-checkpoint-" <> show (index :: Int) <> ".json")
    void (waitUntil "WM RTS checkpoint" assertionTimeoutMicroseconds (doesFileExist path) id)

timeCommand :: Commands -> IO Observation -> Model -> Step -> IO [(Endpoint, Word64)]
timeCommand commands observe next step = do
  threadDelay idleGapMicroseconds
  (operation, endpoints) <- maybe (fail ("m3 has no command for " <> show step)) pure (stepCommand step)
  Receipt started revision <- command commands operation
  forM endpoints $ \endpoint -> do
    ended <- case endpoint of
      ExternalWake -> observed
      ScratchpadToggle -> observed
      IPCUpdate -> publishedAt <$> (awaitPublished commands assertionTimeoutMicroseconds ((>= revision) . publishedRevision) >>= publishes)
      RecompileCompletion -> publishedAt <$> (awaitPublished commands assertionTimeoutMicroseconds (finished (publishedFinishedBuilds expected)) >>= failedByStub)
    pure (endpoint, ended - started)
 where
  expected = expectedPublication next
  observed = waitUntilEvery ("probe outcome of " <> show step) assertionTimeoutMicroseconds pollMicroseconds observe (== expectedObservation next) >> getMonotonicTimeNSec
  publishes p = do
    unless (Text.unpack (publishedFocus p) == publishedFocusedTag expected) (fail ("subscriber published " <> show p <> " for " <> show next))
    pure p
  finished generation p = maybe False (\b -> buildGeneration b == fromIntegral generation && buildState b /= "building") (publishedBuild p)
  failedByStub p = do
    let report = publishedBuild p
    unless (fmap buildState report == Just "failed" && maybe False (Text.isInfixOf (Text.pack checkFailure)) (report >>= buildError)) (fail ("build did not fail at the stub check: " <> show report))
    pure p

idleGapMicroseconds :: Int
idleGapMicroseconds = 1000

pollMicroseconds :: Int
pollMicroseconds = 100

endpointFields :: WorkloadName -> [Step] -> Map Endpoint [Word64] -> [Pair]
endpointFields workload measured timings
  | Map.null planned = []
  | otherwise =
      [ "endpoints" .= object [Key.fromString (endpointText e) .= reverse (Map.findWithDefault [] e timings) | e <- Map.keys planned]
      , "endpoint_steps" .= object [Key.fromString (endpointText e) .= n | (e, n) <- Map.toList planned]
      , "idle_gap_us" .= idleGapMicroseconds
      , "observation_poll_us" .= pollMicroseconds
      ]
 where
  planned = Map.fromListWith (+) [(e, 1 :: Int) | s <- measured, e <- stepEndpoints workload s]

openRole :: Probe -> [ProbeWindow] -> WindowRole -> IO ProbeWindow
openRole probe _ (TileRole color) = openWindow probe color
openRole probe opened (DialogRole color (width, height) parent) = openWindowWith probe (windowSpec color){specParent = Just (opened !! parent), specCommit = CommitFixed width height}
openRole probe _ (PadRole color) = openWindowWith probe (windowSpec color){specAppId = CustomAppId (Text.pack padAppId)}

publish :: FilePath -> String -> IO ()
publish path contents = writeFile (path <> ".tmp") contents >> renameFile (path <> ".tmp") path

writeProcesses :: FilePath -> [(String, Maybe Int)] -> IO ()
writeProcesses directory processes = do
  entries <- forM processes $ \(role, pidValue) -> do
    pid <- maybe (fail ("no PID for " <> role)) pure pidValue
    raw <- readFile ("/proc/" <> show pid <> "/stat")
    let fields = words (drop 1 (dropWhile (/= ')') raw))
        startTicks = read (fields !! 19) :: Integer
    pure (object ["role" .= role, "pid" .= pid, "start_ticks" .= startTicks, "owned_group" .= (role /= "driver")])
  let path = directory </> "processes.json"
  BL.writeFile (path <> ".tmp") (encode entries)
  renameFile (path <> ".tmp") path
