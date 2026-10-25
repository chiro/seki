{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Latency.Live (runWM, runLive) where

import Control.Exception (bracket)
import Control.Monad (foldM, forM, unless, void)
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Seki.Fixture.Probe
import Seki.Integration.Harness
import Seki.Internal.Core.Action (sendMessage, windows)
import Seki.Internal.Core.Layout (Resize (..))
import Seki.Internal.Core.ManageHook (doFloat, isDialog, (-->))
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Core.StackSet qualified as S
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Latency.Workload
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory (riverInventoryWindows)
import Seki.Run.Latency
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Run.Sequence (RiverSequencePhase (AwaitingManageOrRender))
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (ConnectionTarget (UnixSocketPath))
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR1, sigUSR2, signalProcess)
import System.Process (CreateProcess (..), StdStream (UseHandle), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

runWM :: FilePath -> FilePath -> IO ()
runWM socket directory = do
  recorder <- newRecorder 100000
  policy <- newPolicy config >>= requireRight "latency policy"
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
  result <- runRiverSession session
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
      { policyLayout = Tall 1 (1 / 10) (1 / 2)
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
      , policyCheckRestart = pure (Right ())
      , policyLog = \_ -> pure ()
      , policyLaunch = \_ -> fail "unexpected spawn"
      }

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
              (createProcess (proc executable (["wm", socket, directory, "+RTS"] <> flags <> ["-RTS"])){env = Just clientEnv, std_out = UseHandle out, std_err = UseHandle err, create_group = True})
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
                        perform model step = do
                          drive model step
                          let next = stepModel model step
                          await next
                          pure next
                        awaitPicture model =
                          void (waitUntil "native picture" assertionTimeoutMicroseconds (pictureMismatch (expectedLayers model) <$> captureOutput socket name) (== Nothing))
                        start0 = initialModel workload
                    await start0
                    awaitPicture start0
                    warmed <- foldM perform start0 (take 24 (plan workload seed start0))
                    let measured = take steps (plan workload (seed + 1) warmed)
                    checkpoint wmPid 0
                    start <- getMonotonicTimeNSec
                    final <- foldM perform warmed measured
                    checkpoint wmPid 1
                    end <- getMonotonicTimeNSec
                    awaitPicture final
                    restored <- foldM perform final (restoreSteps final)
                    keyCount <- keyboardSnapshot probe
                    runInputDriver clientEnv ["q"]
                    _ <- waitUntil "focused client receives q" assertionTimeoutMicroseconds (keyboardSnapshot probe) $ \after ->
                      all (\client -> Map.findWithDefault 0 client (pressedKeys after) == Map.findWithDefault 0 client (pressedKeys keyCount) + if client == byIndex Map.! modelFocus restored then 1 else 0) clients
                    awaitPicture restored
                    BL.writeFile
                      (directory </> "driver.json")
                      ( encode
                          ( object
                              [ "mode" .= ("live" :: String)
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

openRole :: Probe -> [ProbeWindow] -> WindowRole -> IO ProbeWindow
openRole probe _ (TileRole color) = openWindow probe color
openRole probe opened (DialogRole color (width, height) parent) = openWindowWith probe (windowSpec color){specParent = Just (opened !! parent), specCommit = CommitFixed width height}

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
