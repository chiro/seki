{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Harness (
  mainWith,
  DisplayNames (..),
  requireExecutables,
  createPrivateDirectory,
  fixtureEnvironment,
  withUnixListener,
  startManagedProcess,
  renderManagedProcessLogs,
  managedProcessPid,
  stopManagedProcess,
  riverArguments,
  waitForRiverInit,
  requireRight,
  waitForRuntimeExit,
  requireRuntimeExit,
  assertEqual,
  announce,
  runProcessCapture,
  runInputDriver,
  waitUntil,
  assertionTimeoutMicroseconds,
  pollIntervalMicroseconds,
  waitForMVar,
  failIntegration,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (
  MVar,
  isEmptyMVar,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  withMVar,
 )
import Control.Exception (
  Exception,
  IOException,
  SomeException,
  bracket,
  catch,
  displayException,
  finally,
  onException,
  throwIO,
  try,
 )
import Control.Monad (filterM, forM, forM_, forever, unless, void, when)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', withObject, (.:))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.Char (ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate, nub, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16, Word32)
import Foreign.C.Types (CInt (..), CULong (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekByteOff)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  Socket,
  SocketType (Stream),
  accept,
  bind,
  close,
  connect,
  defaultProtocol,
  listen,
  socket,
  socketToHandle,
 )
import Network.Socket.ByteString qualified as SocketByteString
import Seki.Internal.Core.Action (kill, spawn)
import Seki.Internal.Core.ManageHook (idHook)
import Seki.Internal.Core.Scene (SceneStyle (..), defaultSceneStyle)
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Internal.Layout.Full (Full (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory (
  RiverInventory,
  RiverOutputState (..),
  RiverRectangle (..),
  RiverSize (..),
  RiverWindowState (..),
  riverInventoryOutputs,
  riverInventoryWindows,
  riverOutputRectangle,
 )
import Seki.Run.Policy (Policy, PolicyConfig (..), newPolicy, policyRuntimeHandlers, readPolicyState)
import Seki.Run.Runtime (
  RiverExit (..),
  RiverKeyBindingState (..),
  RiverRuntimeError,
  RiverSeatBindings (..),
  RiverSession,
  openRiverSessionAt,
  readRiverInventory,
  readRiverSeatBindings,
  riverWaylandSession,
  runRiverSession,
 )
import Seki.Run.Startup (readWaylandGlobalCatalog)
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Connection.ObjectTable qualified as ObjectTable
import Seki.Wire.Connection.Registry (
  announcedGlobalInterface,
  announcedGlobals,
 )
import Seki.Wire.Transport (ConnectionTarget (UnixSocketPath))
import Seki.Wire.Types (mkObjectId)
import System.Directory (
  canonicalizePath,
  createDirectory,
  doesFileExist,
  doesPathExist,
  findExecutable,
  removeFile,
 )
import System.Environment (
  getArgs,
  getEnvironment,
  getExecutablePath,
  lookupEnv,
 )
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (
  BufferMode (NoBuffering),
  Handle,
  IOMode (ReadWriteMode),
  hClose,
  hGetLine,
  hPutStrLn,
  hSetBinaryMode,
  hSetBuffering,
  stderr,
  stdin,
  stdout,
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.Signals (
  Signal,
  sigKILL,
  sigTERM,
  signalProcessGroup,
 )
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  StdStream (CreatePipe),
  createProcess,
  getPid,
  getProcessExitCode,
  proc,
 )
import System.Timeout (timeout)

data IntegrationFailure = IntegrationFailure String
  deriving (Show)

instance Exception IntegrationFailure

data DisplayNames = DisplayNames
  { nestedWaylandDisplay :: FilePath
  , nestedXDisplay :: Maybe String
  }
  deriving (Eq, Show)

data BoundedLog = BoundedLog
  { boundedLogLabel :: String
  , boundedLogBytes :: IORef ByteString
  , boundedLogFinished :: MVar ()
  }

data ManagedProcess = ManagedProcess
  { managedProcessStdin :: Maybe Handle
  , managedProcessHandle :: ProcessHandle
  , managedProcessStdout :: BoundedLog
  , managedProcessStderr :: BoundedLog
  }

managedProcessPid :: ManagedProcess -> IO (Maybe Int)
managedProcessPid process = fmap fromIntegral <$> getPid (managedProcessHandle process)

data ProbeSize = ProbeSize
  { probeRows :: Int
  , probeColumns :: Int
  , probePixelWidth :: Int
  , probePixelHeight :: Int
  }
  deriving (Eq, Show)

data TerminalProbeMode
  = PlainTerminalProbe
  | ImeTerminalProbe
  deriving (Eq, Show)

data RuntimeObservation = RuntimeObservation
  { observationInventory :: RiverInventory
  , observationSeats :: [RiverSeatBindings]
  }
  deriving (Eq, Show)

data RiverFixture = RiverFixture
  { fixtureClientEnvironment :: [(String, String)]
  , fixtureExecutable :: FilePath
  , fixtureProbeListener :: Socket
  , fixtureProbeSocketPath :: FilePath
  , fixtureTerminalProcess :: MVar ManagedProcess
  , fixturePolicy :: Policy String Full
  , fixtureRiverSession :: RiverSession
  , fixtureSessionResult :: MVar (Either RiverRuntimeError RiverExit)
  }

newtype WlrRandrOutput = WlrRandrOutput
  { wlrRandrOutputName :: String
  }
  deriving (Eq, Show)

instance FromJSON WlrRandrOutput where
  parseJSON = withObject "wlr-randr output" $ \objectValue ->
    WlrRandrOutput <$> objectValue .: "name"

logLimit :: Int
logLimit = 64 * 1024

pollIntervalMicroseconds :: Int
pollIntervalMicroseconds = 20 * 1000

startupTimeoutMicroseconds :: Int
startupTimeoutMicroseconds = 10 * 1000 * 1000

assertionTimeoutMicroseconds :: Int
assertionTimeoutMicroseconds = 10 * 1000 * 1000

foreign import ccall unsafe "ioctl"
  c_ioctl :: CInt -> CULong -> Ptr () -> IO CInt

mainWith :: [(String, IO ())] -> IO ()
mainWith additionalScenarios = do
  getArgs >>= \case
    ["--river-init", socketPath] -> riverInitHelper socketPath
    ["--terminal-probe", socketPath] -> terminalProbe PlainTerminalProbe socketPath
    ["--ime-terminal-probe", socketPath] -> terminalProbe ImeTerminalProbe socketPath
    [] -> integrationMain additionalScenarios
    arguments -> do
      hPutStrLn stderr ("seki-integration: unsupported arguments: " <> show arguments)
      exitFailure

integrationMain :: [(String, IO ())] -> IO ()
integrationMain additionalScenarios = do
  enabled <- lookupEnv "SEKI_RUN_INTEGRATION"
  if enabled /= Just "1"
    then putStrLn "seki-integration: real-river suite disabled (set SEKI_RUN_INTEGRATION=1 to enable)"
    else do
      selection <- lookupEnv "SEKI_INTEGRATION_SCENARIOS"
      selected <-
        either failIntegration pure . selectScenarios selection $
          [ ("native-wayland-full", runNativeWaylandFull)
          , ("two-output-full", runTwoOutputFull)
          , ("xwayland", runXwayland)
          , ("fcitx5-mozc", runFcitx5Mozc)
          ]
            <> additionalScenarios
      manifest <- lookupEnv "SEKI_INTEGRATION_MANIFEST"
      initializeManifest manifest
      results <- forM selected (uncurry (runScenario manifest))
      unless (and results) exitFailure

selectScenarios :: Maybe String -> [(String, a)] -> Either String [(String, a)]
selectScenarios selection scenarios = case filter (not . null) (map trimAscii (splitCommas (fromMaybe "" selection))) of
  [] -> Right scenarios
  names -> case filter (`notElem` known) names of
    [] -> Right (filter ((`elem` names) . fst) scenarios)
    unknown -> Left ("SEKI_INTEGRATION_SCENARIOS names unknown scenarios " <> intercalate ", " unknown <> "; known scenarios: " <> intercalate ", " known)
 where
  known = map fst scenarios
  splitCommas value = case break (== ',') value of
    (name, []) -> [name]
    (name, _ : rest) -> name : splitCommas rest

runScenario :: Maybe FilePath -> String -> IO () -> IO Bool
runScenario manifest scenarioName action = do
  result <- try @SomeException action
  case result of
    Right () -> do
      appendManifest manifest (scenarioName <> "=passed")
      putStrLn ("seki-integration: " <> scenarioName <> " passed")
      pure True
    Left errorValue -> do
      appendManifest manifest (scenarioName <> "=failed")
      hPutStrLn stderr ("seki-integration: " <> scenarioName <> " failed: " <> displayException errorValue)
      pure False

initializeManifest :: Maybe FilePath -> IO ()
initializeManifest Nothing = pure ()
initializeManifest (Just path) = writeFile path "enabled=true\n"

appendManifest :: Maybe FilePath -> String -> IO ()
appendManifest Nothing _ = pure ()
appendManifest (Just path) entry = appendFile path (entry <> "\n")

runNativeWaylandFull :: IO ()
runNativeWaylandFull =
  withRiverFixture "native-wayland-full" 1 False PlainTerminalProbe ["river", "foot", "wtype"] runNativeAssertions

runTwoOutputFull :: IO ()
runTwoOutputFull =
  withRiverFixture "two-output-full" 2 False PlainTerminalProbe ["river", "foot", "wtype", "wlr-randr"] runTwoOutputAssertions

runXwayland :: IO ()
runXwayland =
  withRiverFixture "xwayland" 1 True PlainTerminalProbe ["river", "foot", "wtype", "xterm", "xdotool"] runXwaylandAssertions

runFcitx5Mozc :: IO ()
runFcitx5Mozc =
  withRiverFixture
    "fcitx5-mozc"
    1
    False
    ImeTerminalProbe
    ["river", "foot", "wtype", "dbus-daemon", "dbus-test-tool", "fcitx5", "fcitx5-remote", "gdbus"]
    runFcitx5MozcAssertions

withRiverFixture ::
  String ->
  Int ->
  Bool ->
  TerminalProbeMode ->
  [String] ->
  (RiverFixture -> IO ()) ->
  IO ()
withRiverFixture scenarioName outputCount enableXwayland probeMode prerequisites assertions = do
  requireExecutables prerequisites
  announce (scenarioName <> ": prerequisites available: " <> intercalate ", " prerequisites)
  executable <- getExecutablePath
  hostEnvironment <- getEnvironment
  withSystemTempDirectory "seki-integration" $ \fixtureDirectory -> do
    let runtimeDirectory = fixtureDirectory </> "runtime"
        configDirectory = fixtureDirectory </> "config"
        initSocketPath = fixtureDirectory </> "river-init.sock"
        probeSocketPath = fixtureDirectory </> "terminal-probe.sock"
        footConfigPath = configDirectory </> "foot.ini"
    createPrivateDirectory runtimeDirectory
    createPrivateDirectory configDirectory
    writeFootConfig footConfigPath
    let riverEnvironment =
          fixtureEnvironment hostEnvironment runtimeDirectory configDirectory
            <> [("WLR_BACKENDS", "headless"), ("WLR_HEADLESS_OUTPUTS", show outputCount)]
    withUnixListener initSocketPath $ \initListener ->
      bracket
        (startManagedProcess "river" riverEnvironment (proc "river" (riverArguments enableXwayland executable initSocketPath)))
        stopManagedProcess
        $ \riverProcess -> do
          displayNames <- waitForRiverInit initListener riverProcess
          announce (scenarioName <> ": river ready on " <> nestedWaylandDisplay displayNames)
          xDisplay <-
            if enableXwayland
              then case nestedXDisplay displayNames of
                Nothing -> failIntegration "Xwayland unavailable: river init did not report DISPLAY"
                Just value -> pure (Just value)
              else pure Nothing
          let clientEnvironment =
                fixtureEnvironment hostEnvironment runtimeDirectory configDirectory
                  <> [("WAYLAND_DISPLAY", nestedWaylandDisplay displayNames)]
                  <> maybe [] (\value -> [("DISPLAY", value)]) xDisplay
              waylandSocketPath = runtimeDirectory </> nestedWaylandDisplay displayNames
          withUnixListener probeSocketPath $ \probeListener -> do
            terminalProcess <- newEmptyMVar
            policy <-
              newPolicy (integrationConfig probeMode executable footConfigPath probeSocketPath clientEnvironment terminalProcess)
                >>= requireRight "build production policy"
            let handlers = policyRuntimeHandlers policy
            session <- do
              opened <-
                timeout startupTimeoutMicroseconds $
                  openRiverSessionAt
                    (UnixSocketPath waylandSocketPath)
                    defaultObjectLimit
                    handlers
              case opened of
                Nothing -> do
                  logs <- renderManagedProcessLogs riverProcess
                  failIntegration ("timeout opening private river session" <> logs)
                Just (Left errorValue) -> do
                  logs <- renderManagedProcessLogs riverProcess
                  failIntegration ("open private river session failed: " <> show errorValue <> logs)
                Just (Right value) -> pure value
            announce (scenarioName <> ": seki runtime connected")
            sessionResult <- newEmptyMVar
            void . forkIO $ runRiverSession session >>= putMVar sessionResult
            let cleanup = do
                  stopManagedProcess riverProcess
                  waitForRuntimeExit sessionResult
                  tryReadMVar terminalProcess >>= mapM_ stopManagedProcess
                scenario = do
                  assertions
                    RiverFixture
                      { fixtureClientEnvironment = clientEnvironment
                      , fixtureExecutable = executable
                      , fixtureProbeListener = probeListener
                      , fixtureProbeSocketPath = probeSocketPath
                      , fixtureTerminalProcess = terminalProcess
                      , fixturePolicy = policy
                      , fixtureRiverSession = session
                      , fixtureSessionResult = sessionResult
                      }
                  stopManagedProcess riverProcess
                  runtimeResult <- requireRuntimeExit sessionResult
                  assertEqual "river runtime shutdown" (Right RiverEnded) runtimeResult
                  announce (scenarioName <> ": river shutdown observed as clean EOF")
            scenario
              `onException` printFixtureDiagnostics fixtureDirectory riverProcess terminalProcess session sessionResult
              `finally` cleanup

integrationConfig ::
  TerminalProbeMode ->
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  MVar ManagedProcess ->
  PolicyConfig String Full
integrationConfig probeMode executable footConfigPath probeSocketPath clientEnvironment terminalProcess =
  PolicyConfig
    { policyLayout = Full
    , policyTags = ["1"]
    , policyBindings = [(functionKeyBinding 0xffbe, spawn "integration-terminal"), (functionKeyBinding 0xffbf, kill)]
    , policySceneStyle = defaultSceneStyle{sceneBorderWidth = 0}
    , policyManageHook = idHook
    , policyStartupHook = pure ()
    , policyPointerBindings = []
    , policyRestore = Nothing
    , policyCheckRestart = pure (Right ())
    , policyLog = announce
    , policyLaunch = \_ -> do
        process <-
          startManagedProcess
            "foot"
            clientEnvironment
            ( proc
                "foot"
                [ "-c"
                , footConfigPath
                , "-a"
                , "seki-integration-probe"
                , executable
                , terminalProbeArgument probeMode
                , probeSocketPath
                ]
            )
        putMVar terminalProcess process
        pure ()
    }
 where
  functionKeyBinding keysym =
    RiverKeyBinding
      { riverKeyBindingKeysym = keysym
      , riverKeyBindingModifiers = River.RiverSeatV1ModifiersFlags 0
      , riverKeyBindingLayoutOverride = Just 0
      }

terminalProbeArgument :: TerminalProbeMode -> String
terminalProbeArgument PlainTerminalProbe = "--terminal-probe"
terminalProbeArgument ImeTerminalProbe = "--ime-terminal-probe"

runNativeAssertions :: RiverFixture -> IO ()
runNativeAssertions fixture = do
  initial <-
    waitForObservation
      "one complete output, one seat, and initialized bindings"
      session
      sessionResult
      (readyObservation 1)
  assertEqual "usable output count" 1 (length (completeOutputs (observationInventory initial)))
  announce "native-wayland-full: output, seat, and bindings ready"
  announce "native-wayland-full: injecting terminal binding"
  runInputDriver clientEnvironment ["-s", "100", "-k", "F1"]
  announce "native-wayland-full: terminal binding injected"
  footProcess <- waitForMVar "foot process launch" terminalProcess
  announce "native-wayland-full: foot process launched"
  probeHandle <- waitForProbe probeListener footProcess
  announce "native-wayland-full: terminal launched and probe connected"
  flip finally (hClose probeHandle) $ do
    managed <-
      waitForObservation
        "managed foot dimensions"
        session
        sessionResult
        managedFootObservation
    (rectangle, window) <- selectManagedFoot managed
    let expectedSize = RiverSize (riverRectangleWidth rectangle) (riverRectangleHeight rectangle)
    assertEqual "Full window dimensions" (Just expectedSize) (riverWindowDimensions window)
    waitForTerminalExtent expectedSize probeHandle
    announce "native-wayland-full: Full dimensions and terminal extent verified"
    runInputDriver clientEnvironment ["-s", "200", "-d", "15", "seki-focus-check\n"]
    received <- waitForProbeInput probeHandle
    assertEqual "focused terminal input" "seki-focus-check" received
    announce "native-wayland-full: keyboard focus verified"
    runInputDriver clientEnvironment ["-s", "100", "-k", "F2"]
    _ <-
      waitForObservation
        "foot removal from runtime inventory"
        session
        sessionResult
        (null . riverInventoryWindows . observationInventory)
    exitCode <- waitForProcessWithTimeout "foot exit" footProcess
    announce ("native-wayland-full: close lifecycle verified; foot exited with " <> show exitCode)
 where
  clientEnvironment = fixtureClientEnvironment fixture
  probeListener = fixtureProbeListener fixture
  terminalProcess = fixtureTerminalProcess fixture
  session = fixtureRiverSession fixture
  sessionResult = fixtureSessionResult fixture

runTwoOutputAssertions :: RiverFixture -> IO ()
runTwoOutputAssertions fixture = do
  _ <-
    waitForObservation
      "two initial outputs, one seat, and initialized bindings"
      session
      sessionResult
      (readyObservation 2)
  announce "two-output-full: both headless outputs discovered"
  outputNames <- queryWlrRandrOutputNames clientEnvironment
  configureTwoOutputs clientEnvironment outputNames
  configured <-
    waitForObservation
      "two distinct configured output rectangles"
      session
      sessionResult
      configuredTwoOutputObservation
  selectedRectangle <- assertCurrentOutputSelected (fixturePolicy fixture) configured
  announce ("two-output-full: selected lowest-ID rectangle " <> show selectedRectangle)
  runInputDriver clientEnvironment ["-s", "100", "-k", "F1"]
  footProcess <- waitForMVar "two-output foot process launch" terminalProcess
  probeHandle <- waitForProbe probeListener footProcess
  flip finally (hClose probeHandle) $ do
    _ <-
      waitForObservation
        "foot dimensions on selected output"
        session
        sessionResult
        (windowMatchesRectangle selectedRectangle)
    let selectedSize =
          RiverSize
            (riverRectangleWidth selectedRectangle)
            (riverRectangleHeight selectedRectangle)
    waitForTerminalExtent selectedSize probeHandle
    announce "two-output-full: foot dimensions match the lowest-ID output"
    runInputDriver clientEnvironment ["-s", "100", "-k", "F2"]
    _ <-
      waitForObservation
        "two-output foot removal from runtime inventory"
        session
        sessionResult
        (null . riverInventoryWindows . observationInventory)
    exitCode <- waitForProcessWithTimeout "two-output foot exit" footProcess
    announce ("two-output-full: foot closed with " <> show exitCode)
 where
  clientEnvironment = fixtureClientEnvironment fixture
  probeListener = fixtureProbeListener fixture
  terminalProcess = fixtureTerminalProcess fixture
  session = fixtureRiverSession fixture
  sessionResult = fixtureSessionResult fixture

runXwaylandAssertions :: RiverFixture -> IO ()
runXwaylandAssertions fixture =
  bracket startXterm stopManagedProcess $ \xtermProcess ->
    (runAssertions xtermProcess `onException` printManagedProcessDiagnostics xtermProcess)
 where
  clientEnvironment = fixtureClientEnvironment fixture
  session = fixtureRiverSession fixture
  sessionResult = fixtureSessionResult fixture
  startXterm =
    startManagedProcess
      "xterm"
      clientEnvironment
      ( proc
          "xterm"
          [ "-title"
          , "seki-integration-xterm"
          , "-fa"
          , "monospace"
          , "-fs"
          , "12"
          , "-e"
          , fixtureExecutable fixture
          , "--terminal-probe"
          , fixtureProbeSocketPath fixture
          ]
      )
  runAssertions xtermProcess = do
    probeHandle <- waitForProbe (fixtureProbeListener fixture) xtermProcess
    flip finally (hClose probeHandle) $ do
      managed <-
        waitForObservation
          "managed Xwayland xterm"
          session
          sessionResult
          managedXtermObservation
      xtermWindow <- requireSingleWindow "managed Xwayland xterm" managed
      case riverWindowAppId xtermWindow of
        Just appId
          | appId `elem` map Text.pack ["xterm", "XTerm"] -> pure ()
        appId -> failIntegration ("unexpected Xterm river app ID: " <> show appId)
      announce ("xwayland: managed app ID " <> show (riverWindowAppId xtermWindow))
      focusedName <-
        waitUntil
          "Xwayland input focus"
          assertionTimeoutMicroseconds
          (queryX11FocusedWindowName clientEnvironment)
          (== "seki-integration-xterm")
      assertEqual "focused Xwayland window title" "seki-integration-xterm" focusedName
      announce "xwayland: X input focus verified"
      runInputDriver clientEnvironment ["-s", "100", "-k", "F2"]
      _ <-
        waitForObservation
          "Xterm removal from runtime inventory"
          session
          sessionResult
          (null . riverInventoryWindows . observationInventory)
      exitCode <- waitForProcessWithTimeout "xterm exit" xtermProcess
      announce ("xwayland: close lifecycle verified; xterm exited with " <> show exitCode)

managedXtermObservation :: RuntimeObservation -> Bool
managedXtermObservation observation =
  case riverInventoryWindows (observationInventory observation) of
    [window] ->
      isJust (riverWindowAppId window)
        && isJust (riverWindowDimensions window)
        && isJust (riverWindowNode window)
    _ -> False

requireSingleWindow :: String -> RuntimeObservation -> IO RiverWindowState
requireSingleWindow phase observation =
  case riverInventoryWindows (observationInventory observation) of
    [window] -> pure window
    windows -> failIntegration (phase <> " expected one window, got: " <> show windows)

queryX11FocusedWindowName :: [(String, String)] -> IO String
queryX11FocusedWindowName childEnvironment = do
  payload <-
    runProcessCapture
      childEnvironment
      "xdotool"
      ["getwindowfocus", "getwindowname"]
  case reverse (filter (not . null) (map trimAscii (lines (ByteString8.unpack payload)))) of
    focusedName : _ -> pure focusedName
    [] -> failIntegration ("xdotool returned no focused window name: " <> show payload)

trimAscii :: String -> String
trimAscii = reverse . dropWhile (`elem` [' ', '\t', '\r', '\n']) . reverse . dropWhile (`elem` [' ', '\t', '\r', '\n'])

runFcitx5MozcAssertions :: RiverFixture -> IO ()
runFcitx5MozcAssertions fixture = do
  preflightImeProtocols fixture
  preflightFootIme (fixtureClientEnvironment fixture)
  imeBaseEnvironment <- prepareFcitxEnvironment (fixtureClientEnvironment fixture)
  withPrivateDbus imeBaseEnvironment $ \imeEnvironment ->
    bracket
      (startManagedProcess "fcitx5" imeEnvironment (proc "fcitx5" ["-D", "-k"]))
      stopManagedProcess
      $ \fcitxProcess ->
        (runAssertions imeEnvironment fcitxProcess `onException` printManagedProcessDiagnostics fcitxProcess)
 where
  runAssertions imeEnvironment fcitxProcess = do
    waitForFcitxReady imeEnvironment fcitxProcess
    announce "fcitx5-mozc: private D-Bus and fcitx5 ready"
    bracket
      ( startManagedProcess
          "fcitx5 virtual-keyboard owner"
          imeEnvironment
          (proc "dbus-test-tool" ["echo", "--name=org.fcitx.Fcitx5.VirtualKeyboard", "--session"])
      )
      stopManagedProcess
      $ \virtualKeyboardOwner -> do
        bracket startKeyboardKeeper stopManagedProcess $ \keyboardKeeper ->
          (runWithKeyboard imeEnvironment virtualKeyboardOwner keyboardKeeper `onException` printManagedProcessDiagnostics keyboardKeeper)
  startKeyboardKeeper =
    startManagedProcess
      "wtype keyboard keeper"
      (fixtureClientEnvironment fixture)
      ((proc "wtype" ["-s", "100", "-"]){std_in = CreatePipe})
  runWithKeyboard imeEnvironment virtualKeyboardOwner keyboardKeeper = do
    runInputDriver (fixtureClientEnvironment fixture) ["-s", "200", "-k", "F1"]
    requireManagedProcessRunning "persistent virtual keyboard" keyboardKeeper
    footProcess <- waitForMVar "IME foot process" (fixtureTerminalProcess fixture)
    probeHandle <- waitForProbe (fixtureProbeListener fixture) footProcess
    flip finally (hClose probeHandle) $ do
      _ <-
        waitForObservation
          "focused IME foot window"
          (fixtureRiverSession fixture)
          (fixtureSessionResult fixture)
          managedFootObservation
      _ <- runProcessCapture imeEnvironment "fcitx5-remote" ["-s", "mozc"]
      _ <- runProcessCapture imeEnvironment "fcitx5-remote" ["-o"]
      currentInputMethod <- runProcessCapture imeEnvironment "fcitx5-remote" ["-n"]
      assertEqual "fcitx5 current input method" "mozc" (trimAscii (ByteString8.unpack currentInputMethod))
      currentState <- runProcessCapture imeEnvironment "fcitx5-remote" []
      assertEqual "fcitx5 active state" "2" (trimAscii (ByteString8.unpack currentState))
      announce "fcitx5-mozc: Mozc selected and active for focused foot"
      _ <-
        runProcessCapture
          imeEnvironment
          "gdbus"
          [ "call"
          , "--session"
          , "--dest"
          , "org.fcitx.Fcitx5"
          , "--object-path"
          , "/virtualkeyboard"
          , "--method"
          , "org.fcitx.Fcitx.VirtualKeyboard1.ShowVirtualKeyboard"
          ]
      waitForFcitxVirtualKeyboardBackend imeEnvironment virtualKeyboardOwner
      _ <-
        runProcessCapture
          imeEnvironment
          "gdbus"
          [ "call"
          , "--session"
          , "--dest"
          , "org.fcitx.Fcitx5.VirtualKeyboardBackend"
          , "--object-path"
          , "/virtualkeyboard"
          , "--method"
          , "org.fcitx.Fcitx5.VirtualKeyboardBackend1.SetVirtualKeyboardFunctionMode"
          , "1"
          ]
      sendFcitxVirtualKeyboardSequence
        imeEnvironment
        -- Fcitx expects XKB keycodes (the evdev code plus 8).
        [ (ord 'n', 57)
        , (ord 'i', 31)
        , (ord 'h', 43)
        , (ord 'o', 32)
        , (ord 'n', 57)
        , (ord 'g', 42)
        , (ord 'o', 32)
        , (ord ' ', 65)
        , (0xff0d, 36)
        ]
      -- Mozc consumes the commit Return; wtype supplies only the line delimiter.
      runInputDriver (fixtureClientEnvironment fixture) ["-s", "100", "-k", "Return"]
      committed <- waitForProbeInput probeHandle
      assertEqual "Mozc UTF-8 commit" "日本語" committed
      announce "fcitx5-mozc: exact UTF-8 commit verified"
      runInputDriver (fixtureClientEnvironment fixture) ["-s", "100", "-k", "F2"]
      _ <-
        waitForObservation
          "IME foot removal from runtime inventory"
          (fixtureRiverSession fixture)
          (fixtureSessionResult fixture)
          (null . riverInventoryWindows . observationInventory)
      exitCode <- waitForProcessWithTimeout "IME foot exit" footProcess
      announce ("fcitx5-mozc: close lifecycle verified; foot exited with " <> show exitCode)

preflightImeProtocols :: RiverFixture -> IO ()
preflightImeProtocols fixture = do
  catalog <-
    readWaylandGlobalCatalog (riverWaylandSession (fixtureRiverSession fixture))
      >>= requireRight "read Wayland globals for IME preflight"
  let advertised = map announcedGlobalInterface (announcedGlobals catalog)
      required = ["zwp_input_method_manager_v2", "zwp_text_input_manager_v3"]
      missing = filter (`notElem` advertised) required
  unless (null missing) $
    failIntegration
      ( "missing required Wayland IME protocols: "
          <> intercalate ", " (map Text.unpack missing)
      )
  announce "fcitx5-mozc: input-method-v2 and text-input-v3 advertised"

preflightFootIme :: [(String, String)] -> IO ()
preflightFootIme childEnvironment = do
  version <- runProcessCapture childEnvironment "foot" ["--version"]
  unless ("+ime" `elem` words (ByteString8.unpack version)) $
    failIntegration
      ( "foot was built without IME support; foot --version output: "
          <> show version
      )
  announce "fcitx5-mozc: foot +ime build feature verified"

sendFcitxVirtualKeyboardSequence :: [(String, String)] -> [(Int, Int)] -> IO ()
sendFcitxVirtualKeyboardSequence childEnvironment keys =
  forM_ keys $ \key -> do
    sendKey False key
    sendKey True key
 where
  sendKey isRelease (keysym, keycode) = do
    eventTime <- monotonicMilliseconds
    _ <-
      runProcessCapture
        childEnvironment
        "gdbus"
        [ "call"
        , "--session"
        , "--dest"
        , "org.fcitx.Fcitx5.VirtualKeyboardBackend"
        , "--object-path"
        , "/virtualkeyboard"
        , "--method"
        , "org.fcitx.Fcitx5.VirtualKeyboardBackend1.ProcessKeyEvent"
        , show keysym
        , show keycode
        , "0"
        , if isRelease then "true" else "false"
        , show eventTime
        ]
    pure ()

monotonicMilliseconds :: IO Word32
monotonicMilliseconds =
  fromIntegral . (`div` 1000000) <$> getMonotonicTimeNSec

waitForFcitxVirtualKeyboardBackend :: [(String, String)] -> ManagedProcess -> IO ()
waitForFcitxVirtualKeyboardBackend childEnvironment ownerProcess = do
  _ <-
    waitUntil
      "fcitx5 virtual-keyboard D-Bus backend"
      startupTimeoutMicroseconds
      observe
      (\(exitCode, payload) -> exitCode == ExitSuccess && "ProcessKeyEvent" `ByteString8.isInfixOf` payload)
  pure ()
 where
  observe = do
    requireManagedProcessRunning "fcitx5 virtual-keyboard owner" ownerProcess
    (exitCode, stdoutBytes, _) <-
      runProcessOutcome
        childEnvironment
        "gdbus"
        [ "introspect"
        , "--session"
        , "--dest"
        , "org.fcitx.Fcitx5.VirtualKeyboardBackend"
        , "--object-path"
        , "/virtualkeyboard"
        ]
    pure (exitCode, stdoutBytes)

prepareFcitxEnvironment :: [(String, String)] -> IO [(String, String)]
prepareFcitxEnvironment childEnvironment = do
  configDirectory <- requireEnvironmentValue "XDG_CONFIG_HOME" childEnvironment
  let fcitxDirectory = configDirectory </> "fcitx5"
      homeDirectory = configDirectory </> "ime-home"
      dataDirectory = configDirectory </> "ime-data"
      cacheDirectory = configDirectory </> "ime-cache"
      stateDirectory = configDirectory </> "ime-state"
  mapM_ createPrivateDirectory [fcitxDirectory, homeDirectory, dataDirectory, cacheDirectory, stateDirectory]
  writeFcitxProfile (fcitxDirectory </> "profile")
  pure $
    replaceEnvironment
      [ ("HOME", homeDirectory)
      , ("XDG_DATA_HOME", dataDirectory)
      , ("XDG_CACHE_HOME", cacheDirectory)
      , ("XDG_STATE_HOME", stateDirectory)
      , ("LANG", "ja_JP.UTF-8")
      , ("LC_ALL", "ja_JP.UTF-8")
      , ("XDG_SESSION_TYPE", "wayland")
      , ("XDG_CURRENT_DESKTOP", "river")
      , ("GTK_IM_MODULE", "fcitx")
      , ("QT_IM_MODULE", "fcitx")
      , ("XMODIFIERS", "@im=fcitx")
      , ("SDL_IM_MODULE", "fcitx")
      ]
      childEnvironment

withPrivateDbus :: [(String, String)] -> ([(String, String)] -> IO value) -> IO value
withPrivateDbus childEnvironment action = do
  runtimeDirectory <- requireEnvironmentValue "XDG_RUNTIME_DIR" childEnvironment
  let busPath = runtimeDirectory </> "dbus-session-bus"
      busAddress = "unix:path=" <> busPath
      dbusEnvironment = replaceEnvironment [("DBUS_SESSION_BUS_ADDRESS", busAddress)] childEnvironment
  configArguments <- dbusSessionConfigArguments
  bracket
    ( startManagedProcess
        "dbus-daemon"
        childEnvironment
        (proc "dbus-daemon" (configArguments <> ["--nofork", "--nopidfile", "--address=" <> busAddress]))
    )
    stopManagedProcess
    $ \dbusProcess -> do
      _ <-
        waitUntil
          "private D-Bus socket"
          startupTimeoutMicroseconds
          (privateDbusReady busPath dbusProcess)
          id
      action dbusEnvironment `onException` printManagedProcessDiagnostics dbusProcess

-- Nix builds dbus to read its session configuration from /etc, which a CI
-- runner may lack, so prefer the copy installed beside the daemon.
dbusSessionConfigArguments :: IO [String]
dbusSessionConfigArguments =
  findExecutable "dbus-daemon" >>= \case
    Nothing -> pure ["--session"]
    Just daemon -> do
      prefix <- takeDirectory . takeDirectory <$> canonicalizePath daemon
      let config = prefix </> "share" </> "dbus-1" </> "session.conf"
      present <- doesFileExist config
      pure (if present then ["--config-file=" <> config] else ["--session"])

privateDbusReady :: FilePath -> ManagedProcess -> IO Bool
privateDbusReady busPath dbusProcess =
  getProcessExitCode (managedProcessHandle dbusProcess) >>= \case
    Nothing -> doesPathExist busPath
    Just exitCode -> do
      logs <- renderManagedProcessLogs dbusProcess
      failIntegration ("private dbus-daemon exited with " <> show exitCode <> logs)

waitForFcitxReady :: [(String, String)] -> ManagedProcess -> IO ()
waitForFcitxReady childEnvironment fcitxProcess = do
  _ <-
    waitUntil
      "fcitx5 D-Bus service"
      startupTimeoutMicroseconds
      checkReady
      (== ExitSuccess)
  pure ()
 where
  checkReady =
    getProcessExitCode (managedProcessHandle fcitxProcess) >>= \case
      Nothing -> do
        (exitCode, _, _) <- runProcessOutcome childEnvironment "fcitx5-remote" ["--check"]
        pure exitCode
      Just exitCode -> do
        logs <- renderManagedProcessLogs fcitxProcess
        failIntegration ("fcitx5 exited during startup with " <> show exitCode <> logs)

writeFcitxProfile :: FilePath -> IO ()
writeFcitxProfile path =
  writeFile
    path
    ( unlines
        [ "[Groups/0]"
        , "Name=Default"
        , "Default Layout=us"
        , "DefaultIM=mozc"
        , ""
        , "[Groups/0/Items/0]"
        , "Name=keyboard-us"
        , "Layout="
        , ""
        , "[Groups/0/Items/1]"
        , "Name=mozc"
        , "Layout="
        , ""
        , "[GroupOrder]"
        , "0=Default"
        ]
    )

readyObservation :: Int -> RuntimeObservation -> Bool
readyObservation outputCount observation =
  length (completeOutputs (observationInventory observation)) == outputCount
    && case observationSeats observation of
      [seat] ->
        length (riverSeatKeyBindings seat) == 2
          && all riverKeyBindingInitialized (riverSeatKeyBindings seat)
      _ -> False

configuredTwoOutputObservation :: RuntimeObservation -> Bool
configuredTwoOutputObservation observation =
  readyObservation 2 observation
    && rectanglesMatch expectedTwoOutputRectangles (completeOutputRectangles (observationInventory observation))

expectedTwoOutputRectangles :: [RiverRectangle]
expectedTwoOutputRectangles =
  [ RiverRectangle 0 0 1024 768
  , RiverRectangle 1024 0 1600 900
  ]

completeOutputRectangles :: RiverInventory -> [RiverRectangle]
completeOutputRectangles inventory =
  [rectangle | output <- completeOutputs inventory, Just rectangle <- [riverOutputRectangle output]]

rectanglesMatch :: [RiverRectangle] -> [RiverRectangle] -> Bool
rectanglesMatch expected actual =
  length expected == length actual
    && all (`elem` actual) expected

assertCurrentOutputSelected :: Policy String Full -> RuntimeObservation -> IO RiverRectangle
assertCurrentOutputSelected policy observation = do
  state <- readPolicyState policy
  case coreWorkspaces state of
    HeadlessState{} -> failIntegration "production policy has no current output"
    AttachedState stackSet _ ->
      case [ rectangle | output <- completeOutputs (observationInventory observation), ObjectTable.objectId (riverOutputObject output) == outputObjectId (StackSet.output (StackSet.current stackSet)), Just rectangle <- [riverOutputRectangle output]
           ] of
        [rectangle] -> pure rectangle
        _ -> failIntegration "current core output has no rectangle"
 where
  outputObjectId (StackSet.OutputId identifier) = fromMaybe (error "invalid output ID") (mkObjectId identifier)

windowMatchesRectangle :: RiverRectangle -> RuntimeObservation -> Bool
windowMatchesRectangle rectangle observation =
  case riverInventoryWindows (observationInventory observation) of
    [window] ->
      riverWindowAppId window == Just (Text.pack "seki-integration-probe")
        && riverWindowDimensions window
          == Just (RiverSize (riverRectangleWidth rectangle) (riverRectangleHeight rectangle))
        && isJust (riverWindowNode window)
    _ -> False

managedFootObservation :: RuntimeObservation -> Bool
managedFootObservation observation =
  case riverInventoryWindows (observationInventory observation) of
    [window] ->
      riverWindowAppId window == Just (Text.pack "seki-integration-probe")
        && isJust (riverWindowDimensions window)
        && isJust (riverWindowNode window)
    _ -> False

completeOutputs :: RiverInventory -> [RiverOutputState]
completeOutputs inventory =
  [output | output <- riverInventoryOutputs inventory, isJust (riverOutputRectangle output)]

selectManagedFoot :: RuntimeObservation -> IO (RiverRectangle, RiverWindowState)
selectManagedFoot observation =
  case (completeOutputRectangles inventory, riverInventoryWindows inventory) of
    ([rectangle], [window]) -> pure (rectangle, window)
    state -> failIntegration ("unexpected managed foot state: " <> show state)
 where
  inventory = observationInventory observation

terminalExtentError :: RiverSize -> ProbeSize -> Maybe String
terminalExtentError window size
  | width <= 0 || height <= 0 || columns <= 0 || rows <= 0 =
      Just ("terminal reported non-positive size: " <> show size)
  | cellWidth <= 0 || cellHeight <= 0 =
      Just ("terminal reported invalid derived cell size: " <> show size)
  | widthGap < 0 || widthGap >= cellWidth || heightGap < 0 || heightGap >= cellHeight =
      Just
        ( "terminal pixel extent does not fill the river window: window="
            <> show window
            <> ", probe="
            <> show size
            <> ", derived-cell="
            <> show (cellWidth, cellHeight)
        )
  | otherwise = Nothing
 where
  width = probePixelWidth size
  height = probePixelHeight size
  columns = probeColumns size
  rows = probeRows size
  windowWidth = fromIntegral (riverSizeWidth window)
  windowHeight = fromIntegral (riverSizeHeight window)
  cellWidth = width `div` columns
  cellHeight = height `div` rows
  widthGap = windowWidth - width
  heightGap = windowHeight - height

waitForObservation ::
  String ->
  RiverSession ->
  MVar (Either RiverRuntimeError RiverExit) ->
  (RuntimeObservation -> Bool) ->
  IO RuntimeObservation
waitForObservation phase session sessionResult predicate =
  waitUntil phase assertionTimeoutMicroseconds readObservation predicate
 where
  readObservation = do
    finished <- tryReadMVar sessionResult
    case finished of
      Nothing -> RuntimeObservation <$> readRiverInventory session <*> readRiverSeatBindings session
      Just result -> failIntegration ("river runtime exited during " <> phase <> ": " <> show result)

waitUntil :: (Show value) => String -> Int -> IO value -> (value -> Bool) -> IO value
waitUntil phase timeoutMicroseconds observe predicate = do
  started <- getMonotonicTimeNSec
  let deadline = started + fromIntegral timeoutMicroseconds * 1000
      loop lastValue = do
        value <- observe
        if predicate value
          then pure value
          else do
            now <- getMonotonicTimeNSec
            if now >= deadline
              then failIntegration ("timeout waiting for " <> phase <> "; last state: " <> showValue lastValue value)
              else threadDelay pollIntervalMicroseconds >> loop (Just value)
  loop Nothing
 where
  showValue _ value = show value

waitForMVar :: String -> MVar value -> IO value
waitForMVar phase variable =
  timeout startupTimeoutMicroseconds (readMVar variable) >>= \case
    Nothing -> failIntegration ("timeout waiting for " <> phase)
    Just value -> pure value

tryReadMVar :: MVar value -> IO (Maybe value)
tryReadMVar variable = do
  empty <- isEmptyMVar variable
  if empty then pure Nothing else Just <$> readMVar variable

waitForRiverInit :: Socket -> ManagedProcess -> IO DisplayNames
waitForRiverInit listener riverProcess = do
  accepted <- timeout startupTimeoutMicroseconds (accept listener)
  case accepted of
    Nothing -> do
      status <- getProcessExitCode (managedProcessHandle riverProcess)
      logs <- renderManagedProcessLogs riverProcess
      failIntegration
        ( "timeout waiting for river init handshake; river status="
            <> show status
            <> logs
        )
    Just (connection, _) -> bracket (pure connection) close receiveDisplayNames

receiveDisplayNames :: Socket -> IO DisplayNames
receiveDisplayNames connection = do
  payload <- receiveAll connection
  let entries = map breakEntry (lines (ByteString8.unpack payload))
      waylandDisplay = lookup "WAYLAND_DISPLAY" entries
      xDisplay = lookup "DISPLAY" entries >>= nonEmpty
  case waylandDisplay >>= nonEmpty of
    Nothing -> failIntegration ("river init helper did not report WAYLAND_DISPLAY: " <> show payload)
    Just display -> pure (DisplayNames display xDisplay)
 where
  breakEntry entry =
    let (name, rest) = break (== '=') entry
     in (name, drop 1 rest)
  nonEmpty "" = Nothing
  nonEmpty value = Just value

receiveAll :: Socket -> IO ByteString
receiveAll connection = go []
 where
  go chunks =
    SocketByteString.recv connection 4096 >>= \chunk ->
      if ByteString.null chunk
        then pure (ByteString.concat (reverse chunks))
        else go (chunk : chunks)

riverInitHelper :: FilePath -> IO ()
riverInitHelper socketPath = do
  waylandDisplay <- requireEnvironment "WAYLAND_DISPLAY"
  xDisplay <- lookupEnv "DISPLAY"
  bracket (socket AF_UNIX Stream defaultProtocol) close $ \connection -> do
    connect connection (SockAddrUnix socketPath)
    SocketByteString.sendAll connection . ByteString8.pack $
      "WAYLAND_DISPLAY=" <> waylandDisplay <> "\nDISPLAY=" <> maybe "" id xDisplay <> "\n"

terminalProbe :: TerminalProbeMode -> FilePath -> IO ()
terminalProbe probeMode socketPath = do
  hSetBinaryMode stdin True
  hSetBuffering stdout NoBuffering
  when (probeMode == ImeTerminalProbe) $
    ByteString.hPut stdout "\ESC[?737769h"
  bracket (socket AF_UNIX Stream defaultProtocol) close $ \connection -> do
    connect connection (SockAddrUnix socketPath)
    sendLock <- newMVar ()
    lastSize <- newIORef Nothing
    let send payload =
          withMVar sendLock $ \() ->
            SocketByteString.sendAll connection payload
        reportSize = do
          size <- getTerminalSize
          changed <-
            atomicModifyIORef' lastSize $ \previous ->
              (Just size, previous /= Just size)
          when changed . send . ByteString8.pack $
            "SIZE "
              <> unwords
                ( map
                    show
                    [ probeRows size
                    , probeColumns size
                    , probePixelWidth size
                    , probePixelHeight size
                    ]
                )
              <> "\n"
    reportSize
    void . forkIO . forever $ reportSize >> threadDelay pollIntervalMicroseconds
    input <- ByteString8.hGetLine stdin
    send (ByteString8.pack "INPUT " <> input <> ByteString8.pack "\n")
    threadDelay (60 * 1000 * 1000)

getTerminalSize :: IO ProbeSize
getTerminalSize =
  allocaBytes 8 $ \pointer -> do
    result <- c_ioctl 0 0x5413 (castPtr pointer)
    when (result /= 0) (failIntegration "terminal probe TIOCGWINSZ failed")
    rows <- fromIntegral <$> (peekByteOff pointer 0 :: IO Word16)
    columns <- fromIntegral <$> (peekByteOff pointer 2 :: IO Word16)
    pixelWidth <- fromIntegral <$> (peekByteOff pointer 4 :: IO Word16)
    pixelHeight <- fromIntegral <$> (peekByteOff pointer 6 :: IO Word16)
    pure (ProbeSize rows columns pixelWidth pixelHeight)

waitForProbe :: Socket -> ManagedProcess -> IO Handle
waitForProbe listener footProcess = do
  accepted <- timeout startupTimeoutMicroseconds (accept listener)
  case accepted of
    Nothing -> do
      status <- getProcessExitCode (managedProcessHandle footProcess)
      logs <- renderManagedProcessLogs footProcess
      failIntegration
        ( "timeout waiting for terminal probe; foot status="
            <> show status
            <> logs
        )
    Just (connection, _) -> do
      handle <- socketToHandle connection ReadWriteMode
      hSetBuffering handle NoBuffering
      hSetBinaryMode handle True
      pure handle

waitForTerminalExtent :: RiverSize -> Handle -> IO ()
waitForTerminalExtent expectedSize handle = do
  lastError <- newIORef "terminal probe has not reported a size"
  result <- timeout assertionTimeoutMicroseconds (loop lastError)
  case result of
    Nothing -> do
      errorMessage <- readIORef lastError
      failIntegration ("timeout waiting for terminal pixel extent; last report: " <> errorMessage)
    Just () -> pure ()
 where
  loop lastError = do
    line <- hGetLine handle
    size <- parseProbeSize line
    case terminalExtentError expectedSize size of
      Nothing -> pure ()
      Just errorMessage -> do
        atomicModifyIORef' lastError (const (errorMessage, ()))
        loop lastError

parseProbeSize :: String -> IO ProbeSize
parseProbeSize line =
  case words line of
    ["SIZE", rows, columns, pixelWidth, pixelHeight] ->
      ProbeSize
        <$> parseInt "rows" rows
        <*> parseInt "columns" columns
        <*> parseInt "pixel width" pixelWidth
        <*> parseInt "pixel height" pixelHeight
    _ -> failIntegration ("invalid terminal size report: " <> show line)

waitForProbeInput :: Handle -> IO String
waitForProbeInput handle =
  timeout assertionTimeoutMicroseconds nextInput >>= \case
    Nothing -> failIntegration "timeout waiting for focused terminal input"
    Just value -> pure value
 where
  nextInput = do
    line <- hGetLine handle
    case wordsPrefix "INPUT " line of
      Just value ->
        Text.unpack
          <$> requireRight
            "decode terminal probe input as UTF-8"
            (TextEncoding.decodeUtf8' (ByteString8.pack value))
      Nothing
        | isJust (wordsPrefix "SIZE " line) -> nextInput
        | otherwise -> failIntegration ("invalid terminal probe report: " <> show line)

wordsPrefix :: String -> String -> Maybe String
wordsPrefix prefix value
  | take (length prefix) value == prefix = Just (drop (length prefix) value)
  | otherwise = Nothing

parseInt :: String -> String -> IO Int
parseInt label value =
  case reads value of
    [(parsed, "")] -> pure parsed
    _ -> failIntegration ("invalid " <> label <> " in terminal size report: " <> show value)

queryWlrRandrOutputNames :: [(String, String)] -> IO [String]
queryWlrRandrOutputNames childEnvironment = do
  payload <- runProcessCapture childEnvironment "wlr-randr" ["--json"]
  outputs <-
    case eitherDecodeStrict' payload of
      Left errorValue ->
        failIntegration
          ( "could not decode wlr-randr --json output: "
              <> errorValue
              <> "; payload="
              <> show payload
          )
      Right value -> pure value
  let names = sort (map wlrRandrOutputName outputs)
  unless (length names == 2 && length (nub names) == 2 && all (not . null) names) $
    failIntegration ("expected two uniquely named wlr-randr outputs, got: " <> show names)
  pure names

configureTwoOutputs :: [(String, String)] -> [String] -> IO ()
configureTwoOutputs childEnvironment outputNames =
  case outputNames of
    [firstOutput, secondOutput] -> do
      _ <-
        runProcessCapture
          childEnvironment
          "wlr-randr"
          [ "--output"
          , firstOutput
          , "--custom-mode"
          , "1024x768"
          , "--pos"
          , "0,0"
          , "--output"
          , secondOutput
          , "--custom-mode"
          , "1600x900"
          , "--pos"
          , "1024,0"
          ]
      announce
        ( "two-output-full: configured "
            <> firstOutput
            <> " as 1024x768 and "
            <> secondOutput
            <> " as 1600x900"
        )
    _ -> failIntegration ("cannot configure unexpected output names: " <> show outputNames)

runProcessCapture :: [(String, String)] -> FilePath -> [String] -> IO ByteString
runProcessCapture childEnvironment command arguments =
  bracket
    (startManagedProcess command childEnvironment (proc command arguments))
    stopManagedProcess
    $ \process -> do
      exitCode <- waitForProcessWithTimeout (command <> " exit") process
      stdoutBytes <- waitForBoundedLog (managedProcessStdout process)
      unless (exitCode == ExitSuccess) $ do
        logs <- renderManagedProcessLogs process
        failIntegration (command <> " failed with " <> show exitCode <> logs)
      pure stdoutBytes

runProcessOutcome ::
  [(String, String)] ->
  FilePath ->
  [String] ->
  IO (ExitCode, ByteString, ByteString)
runProcessOutcome childEnvironment command arguments =
  bracket
    (startManagedProcess command childEnvironment (proc command arguments))
    stopManagedProcess
    $ \process -> do
      exitCode <- waitForProcessWithTimeout (command <> " exit") process
      stdoutBytes <- waitForBoundedLog (managedProcessStdout process)
      stderrBytes <- waitForBoundedLog (managedProcessStderr process)
      pure (exitCode, stdoutBytes, stderrBytes)

runInputDriver :: [(String, String)] -> [String] -> IO ()
runInputDriver childEnvironment arguments =
  bracket
    (startManagedProcess "wtype" childEnvironment (proc "wtype" arguments))
    stopManagedProcess
    $ \inputProcess -> do
      exitCode <- waitForProcessWithTimeout "wtype exit" inputProcess
      unless (exitCode == ExitSuccess) $ do
        logs <- renderManagedProcessLogs inputProcess
        failIntegration ("wtype failed with " <> show exitCode <> logs)

startManagedProcess :: String -> [(String, String)] -> CreateProcess -> IO ManagedProcess
startManagedProcess label childEnvironment specification = do
  (stdinPipe, stdoutPipe, stderrPipe, processHandle) <-
    createProcess
      specification
        { env = Just childEnvironment
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        }
  case (stdoutPipe, stderrPipe) of
    (Just processStdout, Just processStderr) -> do
      stdoutLog <- startBoundedLog (label <> " stdout") processStdout
      stderrLog <- startBoundedLog (label <> " stderr") processStderr
      pure (ManagedProcess stdinPipe processHandle stdoutLog stderrLog)
        `onException` stopProcessGroup processHandle
    _ -> do
      stopProcessGroup processHandle
      failIntegration ("failed to capture " <> label <> " output")

startBoundedLog :: String -> Handle -> IO BoundedLog
startBoundedLog label handle = do
  bytes <- newIORef ByteString.empty
  finished <- newEmptyMVar
  let capture =
        ByteString.hGetSome handle 4096 >>= \chunk ->
          if ByteString.null chunk
            then hClose handle
            else appendBounded bytes chunk >> capture
  void . forkIO $
    (capture `catch` \(_ :: IOException) -> pure ())
      `finally` putMVar finished ()
  pure (BoundedLog label bytes finished)

waitForBoundedLog :: BoundedLog -> IO ByteString
waitForBoundedLog logCapture = do
  completed <- timeout assertionTimeoutMicroseconds (readMVar (boundedLogFinished logCapture))
  case completed of
    Nothing -> failIntegration ("timeout draining " <> boundedLogLabel logCapture)
    Just () -> readIORef (boundedLogBytes logCapture)

appendBounded :: IORef ByteString -> ByteString -> IO ()
appendBounded bytes chunk =
  atomicModifyIORef' bytes $ \current ->
    let combined = current <> chunk
        excess = ByteString.length combined - logLimit
     in (if excess > 0 then ByteString.drop excess combined else combined, ())

renderManagedProcessLogs :: ManagedProcess -> IO String
renderManagedProcessLogs process = do
  stdoutLog <- renderBoundedLog (managedProcessStdout process)
  stderrLog <- renderBoundedLog (managedProcessStderr process)
  pure ("\n" <> stdoutLog <> "\n" <> stderrLog)

printManagedProcessDiagnostics :: ManagedProcess -> IO ()
printManagedProcessDiagnostics process = do
  status <- getProcessExitCode (managedProcessHandle process)
  logs <- renderManagedProcessLogs process
  hPutStrLn stderr ("client status: " <> show status <> logs)

requireManagedProcessRunning :: String -> ManagedProcess -> IO ()
requireManagedProcessRunning label process =
  getProcessExitCode (managedProcessHandle process) >>= \case
    Nothing -> pure ()
    Just exitCode -> do
      logs <- renderManagedProcessLogs process
      failIntegration (label <> " exited with " <> show exitCode <> logs)

renderBoundedLog :: BoundedLog -> IO String
renderBoundedLog logCapture = do
  bytes <- readIORef (boundedLogBytes logCapture)
  pure ("--- " <> boundedLogLabel logCapture <> " (last 64 KiB) ---\n" <> ByteString8.unpack bytes)

stopManagedProcess :: ManagedProcess -> IO ()
stopManagedProcess process = do
  mapM_ (\handle -> hClose handle `catch` \(_ :: IOException) -> pure ()) (managedProcessStdin process)
  stopProcessGroup (managedProcessHandle process)

stopProcessGroup :: ProcessHandle -> IO ()
stopProcessGroup processHandle = do
  getProcessExitCode processHandle >>= \case
    Just _ -> pure ()
    Nothing -> do
      signalGroup processHandle sigTERM
      waitForExitCode (2 * 1000 * 1000) processHandle >>= \case
        Just _ -> pure ()
        Nothing -> do
          signalGroup processHandle sigKILL
          void (waitForExitCode (2 * 1000 * 1000) processHandle)

signalGroup :: ProcessHandle -> Signal -> IO ()
signalGroup processHandle signal =
  getPid processHandle >>= \case
    Nothing -> pure ()
    Just processId ->
      signalProcessGroup signal processId
        `catch` \(_ :: IOException) -> pure ()

waitForProcessWithTimeout :: String -> ManagedProcess -> IO ExitCode
waitForProcessWithTimeout phase process =
  waitForExitCode assertionTimeoutMicroseconds (managedProcessHandle process) >>= \case
    Nothing -> do
      logs <- renderManagedProcessLogs process
      failIntegration ("timeout waiting for " <> phase <> logs)
    Just exitCode -> pure exitCode

waitForExitCode :: Int -> ProcessHandle -> IO (Maybe ExitCode)
waitForExitCode timeoutMicroseconds processHandle = do
  started <- getMonotonicTimeNSec
  let deadline = started + fromIntegral timeoutMicroseconds * 1000
      loop =
        getProcessExitCode processHandle >>= \case
          Just exitCode -> pure (Just exitCode)
          Nothing -> do
            now <- getMonotonicTimeNSec
            if now >= deadline
              then pure Nothing
              else threadDelay pollIntervalMicroseconds >> loop
  loop

waitForRuntimeExit :: MVar (Either RiverRuntimeError RiverExit) -> IO ()
waitForRuntimeExit sessionResult =
  timeout (2 * 1000 * 1000) (readMVar sessionResult) >>= \case
    Nothing -> pure ()
    Just _ -> pure ()

requireRuntimeExit :: MVar (Either RiverRuntimeError RiverExit) -> IO (Either RiverRuntimeError RiverExit)
requireRuntimeExit sessionResult =
  timeout assertionTimeoutMicroseconds (readMVar sessionResult) >>= \case
    Nothing -> failIntegration "timeout waiting for river runtime shutdown"
    Just result -> pure result

printFixtureDiagnostics ::
  FilePath ->
  ManagedProcess ->
  MVar ManagedProcess ->
  RiverSession ->
  MVar (Either RiverRuntimeError RiverExit) ->
  IO ()
printFixtureDiagnostics fixtureDirectory riverProcess terminalProcess session sessionResult = do
  inventory <- readRiverInventory session
  seats <- readRiverSeatBindings session
  globals <- readWaylandGlobalCatalog (riverWaylandSession session)
  runtimeResult <- tryReadMVar sessionResult
  riverStatus <- getProcessExitCode (managedProcessHandle riverProcess)
  riverLogs <- renderManagedProcessLogs riverProcess
  footDetails <-
    tryReadMVar terminalProcess >>= \case
      Nothing -> pure "foot: not launched"
      Just process -> do
        status <- getProcessExitCode (managedProcessHandle process)
        logs <- renderManagedProcessLogs process
        pure ("foot status: " <> show status <> logs)
  hPutStrLn stderr $
    intercalate
      "\n"
      [ "fixture: " <> fixtureDirectory
      , "Wayland globals: " <> show globals
      , "inventory: " <> show inventory
      , "seat bindings: " <> show seats
      , "runtime result: " <> show runtimeResult
      , "river status: " <> show riverStatus <> riverLogs
      , footDetails
      ]

requireExecutables :: [String] -> IO ()
requireExecutables names = do
  missing <- filterM (fmap (not . isJust) . findExecutable) names
  unless (null missing) $
    failIntegration ("missing enabled-suite prerequisites: " <> intercalate ", " missing)

fixtureEnvironment ::
  [(String, String)] ->
  FilePath ->
  FilePath ->
  [(String, String)]
fixtureEnvironment hostEnvironment runtimeDirectory configDirectory =
  filter (not . isIsolatedVariable . fst) hostEnvironment
    <> [("XDG_RUNTIME_DIR", runtimeDirectory), ("XDG_CONFIG_HOME", configDirectory)]

replaceEnvironment :: [(String, String)] -> [(String, String)] -> [(String, String)]
replaceEnvironment replacements environment =
  filter ((`notElem` replacementNames) . fst) environment <> replacements
 where
  replacementNames = map fst replacements

requireEnvironmentValue :: String -> [(String, String)] -> IO String
requireEnvironmentValue name environment =
  case lookup name environment of
    Nothing -> failIntegration ("required child environment variable missing: " <> name)
    Just value -> pure value

isIsolatedVariable :: String -> Bool
isIsolatedVariable name =
  name
    `elem` [ "WAYLAND_SOCKET"
           , "WAYLAND_DISPLAY"
           , "DISPLAY"
           , "DBUS_SESSION_BUS_ADDRESS"
           , "GTK_IM_MODULE"
           , "QT_IM_MODULE"
           , "XMODIFIERS"
           , "SDL_IM_MODULE"
           , "XDG_RUNTIME_DIR"
           , "XDG_CONFIG_HOME"
           , "WLR_BACKENDS"
           , "WLR_HEADLESS_OUTPUTS"
           ]

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = createDirectory path >> setFileMode path 0o700

writeFootConfig :: FilePath -> IO ()
writeFootConfig path =
  writeFile
    path
    ( unlines
        [ "[main]"
        , "font=monospace:size=12"
        , "pad=0x0"
        , "resize-delay-ms=0"
        ]
    )

withUnixListener :: FilePath -> (Socket -> IO value) -> IO value
withUnixListener path = bracket open close
 where
  open = do
    exists <- doesFileExist path
    when exists (removeFile path)
    listener <- socket AF_UNIX Stream defaultProtocol
    (bind listener (SockAddrUnix path) >> listen listener 1 >> pure listener)
      `onException` close listener

riverInitCommand :: FilePath -> FilePath -> String
riverInitCommand executable socketPath =
  shellQuote executable <> " --river-init " <> shellQuote socketPath

riverArguments :: Bool -> FilePath -> FilePath -> [String]
riverArguments enableXwayland executable socketPath =
  (if enableXwayland then [] else ["-no-xwayland"])
    <> ["-c", riverInitCommand executable socketPath]

shellQuote :: String -> String
shellQuote value = "'" <> concatMap quoteCharacter value <> "'"
 where
  quoteCharacter '\'' = "'\\''"
  quoteCharacter character = [character]

requireEnvironment :: String -> IO String
requireEnvironment name =
  lookupEnv name >>= \case
    Nothing -> failIntegration ("required environment variable missing: " <> name)
    Just value -> pure value

requireRight :: (Show errorValue) => String -> Either errorValue value -> IO value
requireRight phase = \case
  Left errorValue -> failIntegration (phase <> " failed: " <> show errorValue)
  Right value -> pure value

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual =
  unless (actual == expected) $
    failIntegration (label <> ": expected " <> show expected <> ", got " <> show actual)

failIntegration :: String -> IO value
failIntegration = throwIO . IntegrationFailure

announce :: String -> IO ()
announce message = hPutStrLn stderr ("seki-integration: " <> message)
