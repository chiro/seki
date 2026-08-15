{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

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
import Control.Monad (filterM, forever, unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Word (Word16)
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
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Default (
  DefaultConfig (..),
  defaultRuntimeHandlers,
 )
import Seki.Run.Full (selectFullOutputRectangle)
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
import Seki.Run.Runtime (
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
import Seki.Wire.Transport (ConnectionTarget (UnixSocketPath))
import System.Directory (
  createDirectory,
  doesFileExist,
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
import System.FilePath ((</>))
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
  }

data ManagedProcess = ManagedProcess
  { managedProcessHandle :: ProcessHandle
  , managedProcessStdout :: BoundedLog
  , managedProcessStderr :: BoundedLog
  }

data ProbeSize = ProbeSize
  { probeRows :: Int
  , probeColumns :: Int
  , probePixelWidth :: Int
  , probePixelHeight :: Int
  }
  deriving (Eq, Show)

data RuntimeObservation = RuntimeObservation
  { observationInventory :: RiverInventory
  , observationSeats :: [RiverSeatBindings]
  }
  deriving (Eq, Show)

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

main :: IO ()
main = do
  getArgs >>= \case
    ["--river-init", socketPath] -> riverInitHelper socketPath
    ["--terminal-probe", socketPath] -> terminalProbe socketPath
    [] -> integrationMain
    arguments -> do
      hPutStrLn stderr ("seki-integration: unsupported arguments: " <> show arguments)
      exitFailure

integrationMain :: IO ()
integrationMain = do
  enabled <- lookupEnv "SEKI_RUN_INTEGRATION"
  if enabled /= Just "1"
    then putStrLn "seki-integration: real-river suite disabled (set SEKI_RUN_INTEGRATION=1 to enable)"
    else do
      manifest <- lookupEnv "SEKI_INTEGRATION_MANIFEST"
      initializeManifest manifest
      result <- try @SomeException runNativeWaylandFull
      case result of
        Right () -> do
          appendManifest manifest "native-wayland-full=passed"
          putStrLn "seki-integration: native-wayland-full passed"
        Left errorValue -> do
          appendManifest manifest "native-wayland-full=failed"
          hPutStrLn stderr ("seki-integration: native-wayland-full failed: " <> displayException errorValue)
          exitFailure

initializeManifest :: Maybe FilePath -> IO ()
initializeManifest Nothing = pure ()
initializeManifest (Just path) = writeFile path "enabled=true\n"

appendManifest :: Maybe FilePath -> String -> IO ()
appendManifest Nothing _ = pure ()
appendManifest (Just path) entry = appendFile path (entry <> "\n")

runNativeWaylandFull :: IO ()
runNativeWaylandFull = do
  requireExecutables ["river", "foot", "wtype"]
  announce "native-wayland-full: prerequisites available"
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
            <> [("WLR_BACKENDS", "headless"), ("WLR_HEADLESS_OUTPUTS", "1")]
    withUnixListener initSocketPath $ \initListener ->
      bracket
        (startManagedProcess "river" riverEnvironment (proc "river" ["-no-xwayland", "-c", riverInitCommand executable initSocketPath]))
        stopManagedProcess
        $ \riverProcess -> do
          displayNames <- waitForRiverInit initListener riverProcess
          announce ("native-wayland-full: river ready on " <> nestedWaylandDisplay displayNames)
          let clientEnvironment =
                fixtureEnvironment hostEnvironment runtimeDirectory configDirectory
                  <> [("WAYLAND_DISPLAY", nestedWaylandDisplay displayNames)]
              waylandSocketPath = runtimeDirectory </> nestedWaylandDisplay displayNames
          withUnixListener probeSocketPath $ \probeListener -> do
            terminalProcess <- newEmptyMVar
            handlers <-
              requireRight "build default runtime handlers" $
                defaultRuntimeHandlers
                  (integrationConfig executable footConfigPath probeSocketPath clientEnvironment terminalProcess)
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
            announce "native-wayland-full: seki runtime connected"
            sessionResult <- newEmptyMVar
            void . forkIO $ runRiverSession session >>= putMVar sessionResult
            let cleanup = do
                  stopManagedProcess riverProcess
                  waitForRuntimeExit sessionResult
                  tryReadMVar terminalProcess >>= mapM_ stopManagedProcess
                scenario = do
                  runNativeAssertions clientEnvironment probeListener terminalProcess session sessionResult
                  stopManagedProcess riverProcess
                  runtimeResult <- requireRuntimeExit sessionResult
                  assertEqual "river runtime shutdown" (Right ()) runtimeResult
                  announce "native-wayland-full: river shutdown observed as clean EOF"
            scenario
              `onException` printFixtureDiagnostics fixtureDirectory riverProcess terminalProcess session sessionResult
              `finally` cleanup

integrationConfig ::
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  MVar ManagedProcess ->
  DefaultConfig
integrationConfig executable footConfigPath probeSocketPath clientEnvironment terminalProcess =
  DefaultConfig
    { defaultTerminalBinding = functionKeyBinding 0xffbe
    , defaultCloseBinding = functionKeyBinding 0xffbf
    , defaultTerminalProcess = do
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
                , "--terminal-probe"
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

runNativeAssertions ::
  [(String, String)] ->
  Socket ->
  MVar ManagedProcess ->
  RiverSession ->
  MVar (Either RiverRuntimeError ()) ->
  IO ()
runNativeAssertions clientEnvironment probeListener terminalProcess session sessionResult = do
  initial <-
    waitForObservation
      "one complete output, one seat, and initialized bindings"
      session
      sessionResult
      readyObservation
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

readyObservation :: RuntimeObservation -> Bool
readyObservation observation =
  length (completeOutputs (observationInventory observation)) == 1
    && case observationSeats observation of
      [seat] ->
        length (riverSeatKeyBindings seat) == 2
          && all riverKeyBindingInitialized (riverSeatKeyBindings seat)
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
  case (selectFullOutputRectangle inventory, riverInventoryWindows inventory) of
    (Just rectangle, [window]) -> pure (rectangle, window)
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
  MVar (Either RiverRuntimeError ()) ->
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

terminalProbe :: FilePath -> IO ()
terminalProbe socketPath = do
  hSetBinaryMode stdin True
  hSetBuffering stdout NoBuffering
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
      Just value -> pure value
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
  (_, stdoutPipe, stderrPipe, processHandle) <-
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
      pure (ManagedProcess processHandle stdoutLog stderrLog)
        `onException` stopProcessGroup processHandle
    _ -> do
      stopProcessGroup processHandle
      failIntegration ("failed to capture " <> label <> " output")

startBoundedLog :: String -> Handle -> IO BoundedLog
startBoundedLog label handle = do
  bytes <- newIORef ByteString.empty
  let capture =
        ByteString.hGetSome handle 4096 >>= \chunk ->
          if ByteString.null chunk
            then hClose handle
            else appendBounded bytes chunk >> capture
  void . forkIO $ capture `catch` \(_ :: IOException) -> pure ()
  pure (BoundedLog label bytes)

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

renderBoundedLog :: BoundedLog -> IO String
renderBoundedLog logCapture = do
  bytes <- readIORef (boundedLogBytes logCapture)
  pure ("--- " <> boundedLogLabel logCapture <> " (last 64 KiB) ---\n" <> ByteString8.unpack bytes)

stopManagedProcess :: ManagedProcess -> IO ()
stopManagedProcess = stopProcessGroup . managedProcessHandle

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

waitForRuntimeExit :: MVar (Either RiverRuntimeError ()) -> IO ()
waitForRuntimeExit sessionResult =
  timeout (2 * 1000 * 1000) (readMVar sessionResult) >>= \case
    Nothing -> pure ()
    Just _ -> pure ()

requireRuntimeExit :: MVar (Either RiverRuntimeError ()) -> IO (Either RiverRuntimeError ())
requireRuntimeExit sessionResult =
  timeout assertionTimeoutMicroseconds (readMVar sessionResult) >>= \case
    Nothing -> failIntegration "timeout waiting for river runtime shutdown"
    Just result -> pure result

printFixtureDiagnostics ::
  FilePath ->
  ManagedProcess ->
  MVar ManagedProcess ->
  RiverSession ->
  MVar (Either RiverRuntimeError ()) ->
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
