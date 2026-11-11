{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Input (inputScenarios, inputWorkerMain) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.Aeson (eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Seki.Integration.Fixture
import Seki.Integration.Harness
import Seki.Integration.Waybar (waybarFixture)
import Seki.Internal.Config (Config (..), def)
import Seki.Internal.Core.Action qualified as Action
import Seki.Internal.Input
import Seki.Protocol.RiverLibinputConfigV1 qualified as L
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Config (policyConfigOf, runConfig)
import Seki.Run.IPC (withIPCAt)
import Seki.Run.Input
import Seki.Run.Libinput (FacetLifecycle (..), encodeDoubles)
import Seki.Run.Policy (PolicyConfig (..), observePolicy)
import Seki.Run.Runtime (RiverInputSnapshot (..), RiverRuntimeSnapshot (..), readRiverInputSnapshot)
import Seki.Run.Startup
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Run.XkbConfig
import Seki.Wire.Connection
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry
import Seki.Wire.Transport (ConnectionTarget (..))
import System.Directory (copyFile, doesFileExist, getCurrentDirectory)
import System.Environment (getEnv, getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc)
import System.Timeout (timeout)

inputScenarios :: [(String, IO ())]
inputScenarios = [("m3-input-v1", inputNative True False), ("m3-input-v2", inputNative False False), ("m3-input-optional-absent", missing), ("m3-input-waybar", inputNative False True), ("m3-input-curve-maximum", nativeCurve), ("m3-input-keymap-invalid", keymapFailure True), ("m3-input-keymap-missing", keymapFailure False), ("m3-input-perf", inputPerf), ("m3-input-worker-lifecycle", workerLifecycle), ("m3-input-output-mapping", outputMapping), ("m3-input-preload-boundary", preloadBoundary)]

inputNative :: Bool -> Bool -> IO ()
inputNative v1 bar = do
  backend <- getEnv "SEKI_INPUT_BACKEND"
  patternValue <- either (failIntegration . show) pure (compileDevicePattern "seki-private-backend-keyboard")
  pressed <- newIORef False
  allKeyboards <- either (failIntegration . show) pure (compileDevicePattern "*keyboard")
  let rules = [InputRule allKeyboards (Just Keyboard) [Keymap (takeDirectory backend </> "two-layouts.xkb"), CapsLock True], InputRule patternValue (Just Keyboard) [RepeatInfo 37 410, ActiveLayout (LayoutByIndex 1), Tap True]]
      environment = [("SEKI_FIXTURE_XKB_VERSION", if v1 then "1" else if bar then "3" else "2"), ("LD_PRELOAD", backend)] <> [("SEKI_FIXTURE_INPUT_V1", "1") | v1] <> [("SEKI_FIXTURE_INITIAL", "1") | v1] <> [("SEKI_FIXTURE_LAYOUT_CYCLE", "1") | bar]
  let config = (policyConfigOf def{inputRules = rules}){policyBindings = [(RiverKeyBinding 64 (River.RiverSeatV1ModifiersFlags 0) (Just 0), Action.spawn "fixture-fixed")], policyLaunch = \_ -> writeIORef pressed True}
  withInputFixture environment config (if bar then "m3-input-waybar" else if v1 then "m3-input-v1" else "m3-input-v2") 1 $ \fixture probe -> do
    updates <- newIORef []
    observePolicy (mPolicy fixture) (\_ _ snapshot -> modifyIORef' updates (riverRuntimeInput snapshot :))
    inventory <- waitUntil "native input and XKB ready" assertionTimeoutMicroseconds (readRiverInputSnapshot (mSession fixture)) $ \case
      InputAvailable i -> any ready (Map.elems (inputDevices i)) && any (\d -> inputName d == Just "seki-private-second-keyboard" && fmap fst (xkbLayout (xkbCurrent (inputXkb d))) == Just 0) (Map.elems (inputDevices i))
      _ -> False
    case inventory of
      InputUnavailable _ -> failIntegration "input unexpectedly unavailable"
      InputAvailable i -> do
        assertEqual "two independent native keyboards" 2 (length [() | d <- Map.elems (inputDevices i), inputType d == Just Keyboard])
        assertEqual "second keyboard retains layout zero" True (any (\d -> inputName d == Just "seki-private-second-keyboard" && fmap fst (xkbLayout (xkbCurrent (inputXkb d))) == Just 0) (Map.elems (inputDevices i)))
    void $ waitUntil "removed devices replaced by fresh generations" assertionTimeoutMicroseconds (readRiverInputSnapshot (mSession fixture)) $ \case
      InputAvailable i -> any (\d -> inputGeneration d >= InputGeneration 3 && ready d) (Map.elems (inputDevices i))
      _ -> False
    received <- readIORef updates
    assertEqual "input-only state wakes policy projection" True (length received >= 2)
    _ <- waitUntil "fixed US binding fires with Japanese current layout" assertionTimeoutMicroseconds (readIORef pressed) id
    saveCapture fixture "input-layouts"
    if bar then waybarFixture True fixture probe else pure ()
 where
  ready d = inputName d == Just "seki-private-backend-keyboard" && xkbLifecycle (inputXkb d) == FacetReady && fmap fst (xkbLayout (xkbCurrent (inputXkb d))) == Just 1 && fmap settingOutcome (Map.lookup ActiveLayoutKey (inputResults d)) == Just SettingObserved && fmap settingOutcome (Map.lookup RepeatInfoKey (inputResults d)) == Just SettingSent && fmap settingOutcome (Map.lookup TapKey (inputResults d)) == Just SettingUnavailable

missing :: IO ()
missing = do
  backend <- getEnv "SEKI_INPUT_BACKEND"
  withInputFixture [("LD_PRELOAD", backend), ("SEKI_FIXTURE_NO_INPUT", "1")] (policyConfigOf def) "m3-input-optional-absent" 1 $ \fixture _ -> do
    readRiverInputSnapshot (mSession fixture) >>= assertEqual "optional input absence" (InputUnavailable [])
    saveCapture fixture "core-live"

-- This exercises native transient libinput config objects, without a device.
nativeCurve :: IO ()
nativeCurve = withNestedRiver 1 $ \river ->
  bracket (openWaylandSessionAt (UnixSocketPath (riverSocket river)) defaultObjectLimit >>= requireRight "curve connection") closeWaylandSession $ \session -> do
    catalog <- readWaylandGlobalCatalog session >>= requireRight "curve catalog"
    globals <- requireRight "libinput global" (selectGlobals L.riverLibinputConfigV1Interface 2 catalog)
    global <- case globals of
      [g] -> pure g
      _ -> failIntegration "expected one libinput manager"
    let connection = waylandConnection session
    manager <- bindGlobalWithRoute connection (waylandRegistry session) global (eventRoute L.decodeRiverLibinputConfigV1Event (\_ _ -> pure ())) >>= requireRight "curve manager"
    config <- submitNewObjectRequest connection L.riverLibinputConfigV1Interface manager L.riverLibinputAccelConfigV1Interface 1 (\object -> L.encodeRiverLibinputConfigV1CreateAccelConfig (objectId object) L.RiverLibinputDeviceV1AccelProfileEnumCustom) >>= requireRight "custom config"
    response <- newIORef Nothing
    _ <- submitNewObjectRequestWithRoute connection L.riverLibinputAccelConfigV1Interface config L.riverLibinputResultV1Interface 1 (\result -> L.encodeRiverLibinputAccelConfigV1SetPoints (objectId result) L.RiverLibinputAccelConfigV1AccelTypeEnumMotion (encodeDoubles [1]) (encodeDoubles (replicate 64 0.5))) (eventRoute L.decodeRiverLibinputResultV1Event (\_ event -> writeIORef response (Just event))) >>= requireRight "maximum points"
    let receive = do
          current <- readIORef response
          case current of
            Just event -> pure event
            Nothing -> dispatchNextEvent connection >>= requireRight "curve dispatch" >> receive
    result <- timeout 5000000 receive
    event <- maybe (failIntegration "native maximum curve result timed out") pure result
    case event of
      L.RiverLibinputResultV1Success -> pure ()
      _ -> failIntegration "native libinput rejected maximum validated curve"
    _ <- submitRequest connection L.riverLibinputAccelConfigV1Interface config L.encodeRiverLibinputAccelConfigV1Destroy >>= requireRight "destroy custom config"
    pure ()

keymapFailure :: Bool -> IO ()
keymapFailure invalid = do
  backend <- getEnv "SEKI_INPUT_BACKEND"
  patternValue <- either (failIntegration . show) pure (compileDevicePattern "seki-private-backend-keyboard")
  let name = if invalid then "m3-input-keymap-invalid" else "m3-input-keymap-missing"
      path = takeDirectory backend </> (if invalid then "invalid.xkb" else "missing.xkb")
      rules = [InputRule patternValue (Just Keyboard) [Keymap path, ActiveLayout (LayoutByIndex 1)]]
      outcome = if invalid then SettingRejectedInvalid else SettingUnavailable
  withInputFixture [("LD_PRELOAD", backend)] (policyConfigOf def{inputRules = rules}) name 1 $ \fixture _ -> do
    _ <- waitUntil "keymap failure is a nonfatal setting outcome" assertionTimeoutMicroseconds (readRiverInputSnapshot (mSession fixture)) $ \case
      InputAvailable i -> any (\d -> inputGeneration d >= InputGeneration 3 && fmap settingOutcome (Map.lookup KeymapKey (inputResults d)) == Just outcome && fmap settingOutcome (Map.lookup ActiveLayoutKey (inputResults d)) /= Just SettingSent) (Map.elems (inputDevices i))
      _ -> False
    saveCapture fixture "keymap-failure-core-live"

inputPerf :: IO ()
inputPerf = withSystemTempDirectory "seki-input-perf" $ \directory -> do
  backend <- getEnv "SEKI_INPUT_BACKEND"
  cwd <- getCurrentDirectory
  patternValue <- either (failIntegration . show) pure (compileDevicePattern "seki-private-backend-keyboard")
  let receipts = directory </> "source.txt"
      result = directory </> "subscriber.json"
      rules = [InputRule patternValue (Just Keyboard) [Keymap (takeDirectory backend </> "two-layouts.xkb"), RepeatInfo 37 410, CapsLock True]]
      environment = [("LD_PRELOAD", backend), ("SEKI_FIXTURE_LAYOUT_CYCLE", "1"), ("SEKI_FIXTURE_FAST_CYCLE", "1"), ("SEKI_FIXTURE_LAYOUT_RECEIPTS", receipts)]
  withInputFixture environment (policyConfigOf def{inputRules = rules}) "m3-input-perf" 1 $ \fixture _ -> do
    root <- maybe (failIntegration "missing private runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
    withIPCAt (ipcSocketIn root) (mPolicy fixture) $ do
      summary <- runProcessCaptureWithin 30000000 (mEnvironment fixture) "python3" [cwd </> "integration/input/subscriber.py", receipts, result]
      announce ("M3_INPUT_PERF " <> show summary)
    lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR" >>= mapM_ (\path -> copyFile result (path </> "input-perf.json"))

inputWorkerMain :: FilePath -> IO ()
inputWorkerMain path = do
  preload <- lookupEnv "LD_PRELOAD"
  assertEqual "worker probe explicitly preloads only hold.so" (Just (takeDirectory path </> "hold.so")) preload
  announce "worker probe preload is explicitly scoped to hold.so"
  patternValue <- either (fail . show) pure (compileDevicePattern "*keyboard")
  runConfig [] def{inputRules = [InputRule patternValue (Just Keyboard) [Keymap path]]} >>= either (fail . show) pure

workerLifecycle :: IO ()
workerLifecycle = do
  backend <- getEnv "SEKI_INPUT_BACKEND"
  executable <- getExecutablePath
  cli <- getEnv "SEKICTL_EXECUTABLE"
  withNestedRiverEnvironment [("LD_PRELOAD", backend)] 1 $ \river -> do
    let root = riverRuntimeDirectory river
        held = root </> "keymap-held"
        environment = ("LD_PRELOAD", takeDirectory backend </> "hold.so") : ("SEKI_KEYMAP_HELD", held) : ("SEKI_KEYMAP_RELEASE", root </> "keymap-release") : riverEnvironment river
        keymap = takeDirectory backend </> "two-layouts.xkb"
        holds = either (const 0) (length . lines) <$> try @IOException (readFile held)
    bracket (startManagedProcess "held keymap runtime" environment (proc executable ["--input-worker", keymap])) stopManagedProcess $ \runtime -> do
      pid <- managedProcessPid runtime >>= maybe (failIntegration "held keymap runtime PID missing") pure
      let exited = do
            status <- try @IOException (readFile ("/proc/" <> show pid <> "/status"))
            pure (either (const True) (isInfixOf "State:\tZ") status)
      _ <- waitUntil "first keymap load held in open" assertionTimeoutMicroseconds holds (>= 1)
      _ <- waitUntil "IPC live while a keymap load is held" assertionTimeoutMicroseconds (doesFileExist (ipcSocketIn root)) id
      reply <- runProcessCapture (riverEnvironment river) cli ["workspace", "3"]
      value <- requireRight "held-load reply JSON" (eitherDecodeStrict' reply)
      revision <- requireRight "held-load applied revision" (parseEither (withObject "reply" (\o -> o .: "result" >>= withObject "result" (.: "revision"))) value)
      assertEqual "normal command acknowledged while a load is held" True (revision > (0 :: Integer))
      _ <- waitUntil "replacement generation loads while the first load is held" assertionTimeoutMicroseconds holds (>= 2)
      stopManagedProcess (riverProcess river)
      _ <- waitUntil "runtime exits with keymap loads still held" assertionTimeoutMicroseconds exited id
      pure ()

outputMapping :: IO ()
outputMapping = do
  backend <- getEnv "SEKI_INPUT_BACKEND"
  patternValue <- either (failIntegration . show) pure (compileDevicePattern "*pointer")
  let rules = [InputRule patternValue (Just Pointer) [MapOutput (Just "HEADLESS-1")]]
      result outcome snapshot = case snapshot of
        InputAvailable i -> any (\d -> fmap settingOutcome (Map.lookup MapOutputKey (inputResults d)) == Just outcome) (Map.elems (inputDevices i))
        _ -> False
  withInputFixture [("LD_PRELOAD", backend)] (policyConfigOf def{inputRules = rules}) "m3-input-output-mapping" 1 $ \fixture _ -> do
    _ <- waitUntil "named output mapping submitted" assertionTimeoutMicroseconds (readRiverInputSnapshot (mSession fixture)) (result SettingSent)
    _ <- runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", "HEADLESS-1", "--off"]
    _ <- waitUntil "disabled output mapping unavailable" assertionTimeoutMicroseconds (readRiverInputSnapshot (mSession fixture)) (result SettingUnavailable)
    _ <- runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", "HEADLESS-1", "--on", "--custom-mode", "800x600"]
    _ <- waitUntil "readded named output mapping resubmitted" assertionTimeoutMicroseconds (readRiverInputSnapshot (mSession fixture)) (result SettingSent)
    saveCapture fixture "mapping-resubmitted"

preloadBoundary :: IO ()
preloadBoundary = do
  backend <- getEnv "SEKI_INPUT_BACKEND"
  seki <- getEnv "SEKI_EXECUTABLE"
  cli <- getEnv "SEKICTL_EXECUTABLE"
  withNestedRiverEnvironment [("LD_PRELOAD", backend)] 1 $ \river ->
    bracket (startManagedProcess "actual seki" (riverEnvironment river) (proc seki [])) stopManagedProcess $ \manager -> do
      _ <- waitUntil "actual seki IPC ready" assertionTimeoutMicroseconds (doesFileExist (ipcSocketIn (riverRuntimeDirectory river))) id
      _ <- runProcessCapture (riverEnvironment river) cli ["snapshot"]
      assertNoProcessPreload "actual seki" manager
      stopManagedProcess (riverProcess river)
