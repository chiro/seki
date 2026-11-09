{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Daily (dailyScenarios) where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, finally, onException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (Value, eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
import Data.Either (isRight)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isPrefixOf, nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import Seki.Fixture.Probe
import Seki.Integration.Fixture (Box (..), NestedRiver (..), Paint (..), outputOrigins, outputPicture, pictureErrorWith, placeOutputs, portedBorders, saveOutputsSideBySide, withHeldLogoIn, withKeyboard, withNestedRiverUsing)
import Seki.Integration.Harness
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, findExecutable, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (readSymbolicLink)
import System.Process (proc)
import System.Timeout (timeout)

dailyScenarios :: [(String, IO ())]
dailyScenarios =
  [ ("m3-daily-two-output", twoOutput)
  , ("m3-daily-build", buildHandover)
  , ("m3-daily-bar", barReconnect)
  , ("m3-daily-hotplug", hotplug)
  , ("m3-daily-ime", imeHandover)
  , ("m3-daily-xwayland", xwaylandManagement)
  , ("m3-daily-load", loadResponsive)
  ]

data DailyOptions = DailyOptions
  { optionOutputs :: Int
  , optionXwayland :: Bool
  , optionRiverEnvironment :: [(String, String)]
  }

dailyOptions :: Int -> DailyOptions
dailyOptions outputs = DailyOptions outputs False []

data DailyHome = DailyHome
  { homeRiver :: NestedRiver
  , homeDirectory :: FilePath
  , homeBin :: FilePath
  , homeLaunchLog :: FilePath
  , homeStateDirectory :: FilePath
  , homeKeymap :: FilePath
  , homeExecutable :: FilePath
  , homeCli :: FilePath
  , homeOutputCount :: Int
  }

data DailySession = DailySession
  { sessionName :: String
  , sessionHome :: DailyHome
  , sessionPid :: Int
  , sessionOutputs :: [Placement]
  , sessionProbe :: Probe
  }

data Placement = Placement {placedOutput :: Text, placedX :: Word32, placedWorkspace :: Text} deriving (Show)

withDailySession :: String -> DailyOptions -> (DailySession -> IO a) -> IO a
withDailySession name options body = withDailyHome options $ \home -> runDaily name home [] body

withDailyHome :: DailyOptions -> (DailyHome -> IO a) -> IO a
withDailyHome options use = do
  executable <- requireSetting "SEKI_DAILY_CONFIG_EXECUTABLE"
  cli <- requireSetting "SEKICTL_EXECUTABLE"
  backend <- requireSetting "SEKI_INPUT_BACKEND"
  withNestedRiverUsing (optionXwayland options) (optionRiverEnvironment options) (optionOutputs options) $ \nested -> withSystemTempDirectory "seki-daily-home" $ \directory -> do
    let home =
          DailyHome
            { homeRiver = nested
            , homeDirectory = directory
            , homeBin = directory </> "bin"
            , homeLaunchLog = directory </> "launches"
            , homeStateDirectory = directory </> ".local" </> "state"
            , homeKeymap = directory </> ".config" </> "seki" </> "keymap.xkb"
            , homeExecutable = executable
            , homeCli = cli
            , homeOutputCount = optionOutputs options
            }
    createDirectoryIfMissing True (homeBin home)
    createDirectoryIfMissing True (takeDirectory (homeKeymap home))
    copyFile (takeDirectory backend </> "daily.xkb") (homeKeymap home)
    forM_ stubPrograms $ \program -> writeExecutable (homeBin home </> program) (loggingStub (homeLaunchLog home))
    use home

dailyEnvironment :: DailyHome -> [(String, String)] -> [(String, String)]
dailyEnvironment home extra = overrides <> filter ((`notElem` map fst overrides) . fst) inheritedWithoutConfigHome
 where
  base = riverEnvironment (homeRiver home)
  overrides =
    [ ("HOME", homeDirectory home)
    , ("PATH", homeBin home <> ":" <> fromMaybe "/usr/bin:/bin" (lookup "PATH" base))
    , ("XDG_STATE_HOME", homeStateDirectory home)
    , ("XDG_DATA_HOME", homeDirectory home </> ".local" </> "share")
    , ("XDG_CACHE_HOME", homeDirectory home </> ".cache")
    ]
      <> extra
  inheritedWithoutConfigHome = filter ((/= "XDG_CONFIG_HOME") . fst) base

-- | Every startup command must have run exactly once when the body starts and again when it ends, so a restart that replays the startup hook fails the lane.
runDaily :: String -> DailyHome -> [(String, String)] -> (DailySession -> IO a) -> IO a
runDaily name home extra body =
  bracket (startManagedProcess name (dailyEnvironment home extra) (proc (homeExecutable home) [])) stopManagedProcess $ \process ->
    ( do
        pid <- managedProcessPid process >>= maybe (failIntegration (name <> ": the daily process has no pid")) pure
        outputs <- sort . map snd <$> waitUntil "seki shows every output" assertionTimeoutMicroseconds (outputNames surface) ((== count) . length)
        placeOutputs base outputs
        withKeyboard base . withProbe surface $ \probe -> do
          let unplaced = DailySession name home pid [] probe
          shown <- waitSnapshot unplaced "IPC shows a workspace on every output" (\s -> length (snapOutputs s) == count && all (isJust . outputWorkspace) (snapOutputs s))
          let session = unplaced{sessionOutputs = [Placement output x tag | (output, x) <- outputOrigins outputs, OutputView (Just shownOutput) (Just tag) <- snapOutputs shown, shownOutput == output]}
          startupRanOnce session "each startup command runs once after the first output"
          result <- body session
          startupRanOnce session "each startup command still ran exactly once at the end of the lane"
          pure result
    )
      `onException` (renderManagedProcessLogs process >>= announce)
 where
  base = riverEnvironment (homeRiver home)
  surface = riverSocket (homeRiver home)
  count = homeOutputCount home

stubPrograms :: [String]
stubPrograms = ["waybar", "swaybg", "fcitx5", "swayidle", "swaylock", "alacritty", "amixer", "brightnessctl"]

startupCommands :: [String]
startupCommands = ["waybar", "swaybg -i /usr/share/backgrounds/archlinux/wave.png -m fill", "fcitx5", "swayidle -w timeout 600 swaylock -f"]

startupRanOnce :: DailySession -> String -> IO ()
startupRanOnce session phase = do
  _ <- waitUntil phase assertionTimeoutMicroseconds (recordedLaunches session) (\logged -> all (`elem` logged) startupCommands)
  logged <- recordedLaunches session
  assertEqual phase [(command, 1 :: Int) | command <- startupCommands] [(command, length (filter (== command) logged)) | command <- startupCommands]

recordedLaunches :: DailySession -> IO [String]
recordedLaunches session = do
  exists <- doesFileExist launches
  if exists then lines . BS8.unpack <$> BS8.readFile launches else pure []
 where
  launches = homeLaunchLog (sessionHome session)

launchedOnce :: DailySession -> String -> String -> IO ()
launchedOnce session phase command = void (waitUntil phase assertionTimeoutMicroseconds (filter (== command) <$> recordedLaunches session) (== [command]))

sessionRiver :: DailySession -> NestedRiver
sessionRiver = homeRiver . sessionHome

clientEnvironment :: DailySession -> [(String, String)]
clientEnvironment = riverEnvironment . sessionRiver

sessionSocket :: DailySession -> FilePath
sessionSocket = riverSocket . sessionRiver

sekictl :: DailySession -> [String] -> IO BS.ByteString
sekictl session = runProcessCapture (clientEnvironment session) (homeCli (sessionHome session))

snapshot :: DailySession -> IO Snapshot
snapshot session = sekictl session ["snapshot"] >>= requireRight "sekictl snapshot reply" . replySnapshot

-- | sekictl makes one attempt, so a call that fails while a restart replaces the IPC socket only means not yet.
waitSnapshot :: DailySession -> String -> (Snapshot -> Bool) -> IO Snapshot
waitSnapshot session phase ready = waitUntil phase assertionTimeoutMicroseconds attempt (either (const False) ready) >>= either failIntegration pure
 where
  attempt = either (\(IntegrationFailure failure) -> Left failure) replySnapshot <$> try @IntegrationFailure (sekictl session ["snapshot"])

waitFocus :: DailySession -> String -> Maybe ProbeWindow -> IO ()
waitFocus session phase wanted = void (waitUntil phase assertionTimeoutMicroseconds (keyboardSnapshot (sessionProbe session)) ((== wanted) . keyboardOwner))

receiptRevision :: BS.ByteString -> Either String Integer
receiptRevision bytes = eitherDecodeStrict' bytes >>= parseEither (withObject "reply" (\reply -> reply .: "result" >>= withObject "result" (.: "revision")))

processImage :: Int -> IO (FilePath, [String])
processImage pid = (,) <$> readSymbolicLink ("/proc/" <> show pid <> "/exe") <*> (map BS8.unpack . filter (not . BS.null) . BS.split 0 <$> BS.readFile ("/proc/" <> show pid <> "/cmdline"))

startedOn :: DailySession -> Text -> IO Placement
startedOn session tag = maybe (failIntegration ("no output showed " <> Text.unpack tag <> " when the session started: " <> show (sessionOutputs session))) pure (find ((== tag) . placedWorkspace) (sessionOutputs session))

sessionExtent :: DailySession -> (Word32, Word32)
sessionExtent session = (800 * fromIntegral (length (sessionOutputs session)), 600)

picture :: DailySession -> Text -> String -> [Paint] -> IO ()
picture session output phase = outputPicture (sessionName session) (sessionSocket session) output phase . pictureErrorWith portedBorders

report :: String -> String -> IO ()
report label value = announce ("M3_DAILY_" <> label <> " " <> value)

capture :: DailySession -> String -> IO ()
capture session label = do
  artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
  forM_ artifacts $ \directory -> do
    createDirectoryIfMissing True directory
    saveOutputsSideBySide (sessionSocket session) (directory </> label <> ".png")
  stamp <- getMonotonicTimeNSec
  report "CAPTURE" (label <> " " <> show stamp)

withSubscription :: DailySession -> String -> (IO [Snapshot] -> IO a) -> IO a
withSubscription session file action =
  withSekictlSubscription (clientEnvironment session) (homeCli (sessionHome session)) path $ \received ->
    action (received >>= mapM (requireRight "subscription snapshot" . replySnapshot)) `finally` keep
 where
  path = riverRuntimeDirectory (sessionRiver session) </> file
  keep = do
    artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
    exists <- doesFileExist path
    forM_ artifacts $ \directory -> when exists (createDirectoryIfMissing True directory >> copyFile path (directory </> file))

streamShows :: IO [Snapshot] -> String -> BarView -> IO ()
streamShows stream phase expected = do
  _ <- waitUntil phase assertionTimeoutMicroseconds (fmap barView . listToMaybe . reverse <$> stream) (== Just expected)
  report "STREAM_BAR" (phase <> ": " <> show expected)

polledShows :: DailySession -> String -> BarView -> IO ()
polledShows session phase expected = do
  _ <- waitSnapshot session phase ((== expected) . barView)
  report "POLLED_BAR" (phase <> ": " <> show expected)

data BarView = BarView
  { barOutputs :: [(Maybe Text, Maybe Text)]
  , barWorkspaces :: [WorkspaceView]
  , barTitle :: Maybe Text
  , barPads :: [(Text, Text)]
  }
  deriving (Eq)

instance Show BarView where
  show view =
    unwords
      [ "outputs=" <> show (barOutputs view)
      , "workspaces=" <> show [(wsTag w, [flag | (flag, True) <- [('O', wsOccupied w), ('V', wsVisible w), ('F', wsFocused w)]], wsLayout w) | w <- barWorkspaces view, w /= emptyWorkspace (wsTag w)]
      , "title=" <> show (barTitle view)
      , "scratchpads=" <> show (barPads view)
      ]

barView :: Snapshot -> BarView
barView shot = BarView (sort [(outputName o, outputWorkspace o) | o <- snapOutputs shot]) (snapWorkspaces shot) (snapTitle shot) (snapPads shot)

data Flag = Occupied | Visible | Focused deriving (Eq)

bar :: [(Text, Text)] -> [WorkspaceView] -> Maybe Text -> [(Text, Text)] -> BarView
bar outputs listed = BarView (sort [(Just output, Just tag) | (output, tag) <- outputs]) [fromMaybe (emptyWorkspace tag) (find ((== tag) . wsTag) listed) | tag <- dailyTags]

emptyWorkspace :: Text -> WorkspaceView
emptyWorkspace tag = workspace tag [] "Tall"

workspace :: Text -> [Flag] -> Text -> WorkspaceView
workspace tag flags layoutName = WorkspaceView tag (Occupied `elem` flags) (Visible `elem` flags) (Focused `elem` flags) (Just layoutName)

dailyTags :: [Text]
dailyTags = ["term", "emacs", "Web", "4", "5", "6", "7", "8", "9"]

data Snapshot = Snapshot
  { snapSession :: Text
  , snapRevision :: Integer
  , snapOutputs :: [OutputView]
  , snapWorkspaces :: [WorkspaceView]
  , snapTitle :: Maybe Text
  , snapPads :: [(Text, Text)]
  , snapKeyboard :: Bool
  , snapDevices :: [DeviceView]
  , snapBuild :: Maybe BuildView
  }
  deriving (Eq, Show)

data OutputView = OutputView {outputName :: Maybe Text, outputWorkspace :: Maybe Text} deriving (Eq, Show)

data WorkspaceView = WorkspaceView {wsTag :: Text, wsOccupied :: Bool, wsVisible :: Bool, wsFocused :: Bool, wsLayout :: Maybe Text} deriving (Eq, Show)

data DeviceView = DeviceView {deviceName :: Maybe Text, deviceLifecycle :: Text, deviceResults :: [(Text, Text)], deviceKeymap :: Maybe FilePath} deriving (Eq, Show)

data BuildView = BuildView {buildState :: Text, buildGeneration :: Integer, buildError :: Maybe Text, buildImage :: Maybe FilePath} deriving (Eq, Show)

replySnapshot :: BS.ByteString -> Either String Snapshot
replySnapshot bytes = eitherDecodeStrict' bytes >>= parseEither (withObject "reply" (\reply -> reply .: "result" >>= withObject "result" (\result -> result .: "snapshot" >>= parseSnapshot)))

parseSnapshot :: Value -> Parser Snapshot
parseSnapshot = withObject "snapshot" $ \o -> do
  outputs <- o .: "outputs" >>= mapM (withObject "output" (\v -> OutputView <$> v .: "name" <*> v .: "workspace"))
  workspaces <- o .: "workspaces" >>= mapM (withObject "workspace" (\v -> WorkspaceView <$> v .: "tag" <*> v .: "occupied" <*> v .: "visible" <*> v .: "focused" <*> v .: "layout"))
  pads <- o .: "scratchpads" >>= mapM (withObject "scratchpad" (\v -> (,) <$> v .: "name" <*> v .: "state"))
  devices <- o .: "input" >>= withObject "input" (\i -> i .: "devices" >>= withObject "devices" (\d -> d .: "items" >>= mapM parseDevice))
  build <- o .:? "build" >>= traverse (withObject "build" (\b -> BuildView <$> b .: "state" <*> b .: "generation" <*> b .:? "error" <*> b .:? "active_image"))
  Snapshot <$> o .: "session" <*> o .: "revision" <*> pure outputs <*> pure workspaces <*> o .: "focused_title" <*> pure pads <*> o .: "keyboard_available" <*> pure devices <*> pure build
 where
  parseDevice = withObject "device" $ \d -> do
    results <- d .: "results" >>= mapM (withObject "result" (\r -> (,,) <$> r .: "setting" <*> r .: "status" <*> r .: "requested"))
    keymap <- traverse (withObject "keymap request" (.: "path")) (listToMaybe [requested | (setting', _, requested) <- results, setting' == ("keymap" :: Text)])
    DeviceView <$> d .: "name" <*> d .: "lifecycle" <*> pure [(setting', status) | (setting', status, _ :: Value) <- results] <*> pure keymap

twoOutput :: IO ()
twoOutput = withDailySession "m3-daily-two-output" (dailyOptions 2) $ \session -> withSubscription session "daily-two-output-subscription.jsonl" $ \stream -> do
  let probe = sessionProbe session
  termOutput <- placedOutput <$> startedOn session "term"
  emacsOutput <- placedOutput <$> startedOn session "emacs"
  let outputs = [(termOutput, "term"), (emacsOutput, "emacs")]
      shown title pads = bar outputs [workspace "term" [Occupied, Visible, Focused] "Mirror Tall", workspace "emacs" [Occupied, Visible] "Tall"] title pads
  report "OUTPUTS" (show outputs)
  _ <- openWindow probe Blue
  _ <- openWindowWith probe (windowSpec Green){specAppId = CustomAppId "Emacs"}
  second <- openWindow probe Green
  waitFocus session "the newest term window has keyboard focus" (Just second)
  setTitle probe second "daily two-output"
  modKeys (clientEnvironment session) ["space"]
  picture session termOutput "term tiles its two windows under Mirror Tall" [Paint Green (Box 1 1 798 298) True, Paint Blue (Box 1 301 798 298) False]
  picture session emacsOutput "emacs shows the Emacs window on the other output" [Paint Green (Box 1 1 798 598) False]
  streamShows stream "the bar shows both outputs, the title, and the layout" (shown (Just "daily two-output") [])
  capture session "daily-two-output-windows"
  modKeys (clientEnvironment session) ["grave"]
  launchedOnce session "Mod-grave launches the scratchpad terminal" "alacritty --class seki-scratchpad"
  streamShows stream "the launched scratchpad is pending" (shown (Just "daily two-output") [("term", "pending")])
  pad <- openWindowWith probe (windowSpec Blue){specAppId = CustomAppId "seki-scratchpad"}
  waitFocus session "the shown pad has keyboard focus" (Just pad)
  setTitle probe pad "daily scratchpad"
  picture session termOutput "the pad floats over term clamped inside its 800 by 600 output" [Paint Blue (Box 1 1 798 598) True]
  streamShows stream "the matching window shows the pad" (shown (Just "daily scratchpad") [("term", "shown")])
  capture session "daily-two-output"
  modKeys (clientEnvironment session) ["grave"]
  waitFocus session "hiding the pad returns focus to the term window" (Just second)
  picture session termOutput "term tiles its two windows again with the pad hidden" [Paint Green (Box 1 1 798 298) True, Paint Blue (Box 1 301 798 298) False]
  streamShows stream "the second Mod-grave hides the pad" (shown (Just "daily two-output") [("term", "hidden")])
  capture session "daily-two-output-hidden"

stubBuildResources :: DailyHome -> FilePath -> IO [(String, String)]
stubBuildResources home steps = do
  let resources = homeDirectory home </> "config-resources"
      templates = resources </> "templates"
  createDirectoryIfMissing True templates
  forM_ ["Main.hs", "seki-user.cabal", "cabal.project"] $ \file -> writeFile (templates </> file) ("-- " <> file <> " for @ARCHIVE@ built by @COMPILER@\n")
  forM_ ["archive", "ghc", "cabal"] $ \file -> writeFile (resources </> file) ""
  writeExecutable (resources </> "env") ("#!/bin/sh\nprintf '%s\\n' \"$3\" >> '" <> steps <> "'\ncase \"$3\" in\n  build) exit 0 ;;\n  list-bin) printf '%s\\n' '" <> homeExecutable home <> "' ;;\n  *) exit 64 ;;\nesac\n")
  pure [("SEKI_CONFIG_" <> variable, resources </> file) | (variable, file) <- [("ARCHIVE", "archive"), ("GHC", "ghc"), ("CABAL", "cabal"), ("ENV", "env"), ("TEMPLATES", "templates")]]

buildHandover :: IO ()
buildHandover = withDailyHome (dailyOptions 2) $ \home -> do
  let steps = homeDirectory home </> "build-steps"
  resources <- stubBuildResources home steps
  runDaily "m3-daily-build" home resources $ \session -> withSubscription session "daily-build-subscription.jsonl" $ \stream -> do
    let probe = sessionProbe session
        stateDirectory = homeStateDirectory home </> "seki"
    termOutput <- placedOutput <$> startedOn session "term"
    emacsOutput <- placedOutput <$> startedOn session "emacs"
    let windows title pads = bar [(termOutput, "term"), (emacsOutput, "emacs")] [workspace "term" [Occupied, Visible, Focused] "Tall", workspace "emacs" [Occupied, Visible] "Tall"] title pads
        restored phase = do
          picture session termOutput (phase <> ": the pad covers term") [Paint Green (Box 1 1 798 598) True]
          picture session emacsOutput (phase <> ": the Emacs window covers its output fullscreen") [Paint Blue (Box 0 0 800 600) False]
          streamShows stream (phase <> ": the bar shows the pad and both workspaces") (windows (Just "daily build pad") [("term", "shown")])
    _ <- openWindow probe Blue
    editor <- openWindowWith probe (windowSpec Blue){specAppId = CustomAppId "Emacs"}
    requestFullscreen probe editor
    modKeys (clientEnvironment session) ["grave"]
    launchedOnce session "Mod-grave launches the scratchpad terminal" "alacritty --class seki-scratchpad"
    pad <- openWindowWith probe (windowSpec Green){specAppId = CustomAppId "seki-scratchpad"}
    waitFocus session "the shown pad has keyboard focus" (Just pad)
    setTitle probe pad "daily build pad"
    restored "before the recompile"
    capture session "daily-build-before"
    oldSession <- snapSession <$> snapshot session
    receipt <- sekictl session ["recompile"]
    revision <- requireRight "recompile receipt" (receiptRevision receipt)
    report "RECOMPILE_RECEIPT" (show revision)
    sessions <- waitUntil "the checked candidate starts a fresh IPC session" assertionTimeoutMicroseconds (nub . map snapSession <$> stream) ((>= 2) . length)
    report "SESSIONS" (show sessions)
    (image, arguments) <- processImage (sessionPid session)
    report "IMAGE" (show (image, arguments))
    assertEqual "the daily process keeps its pid and restores into the new image" True ("--restore" `elem` arguments)
    assertEqual "the new image is a checked candidate in the state directory" (stateDirectory, True) (takeDirectory image, "candidate" `isPrefixOf` takeFileName image)
    readSymbolicLink (stateDirectory </> "profile") >>= assertEqual "the profile names the running candidate" image
    recorded <- stream
    let new = dropWhile ((== oldSession) . snapSession) recorded
    report "NEW_SESSION_BUILD" (show (map snapBuild (take 1 new)))
    assertEqual "the new session runs the candidate with no build error" [Just (BuildView "idle" 0 Nothing (Just image))] (map snapBuild (take 1 new))
    lines <$> readFile steps >>= assertEqual "the session ran one build and one list-bin" ["build", "list-bin"]
    restored "after the handover"
    waitFocus session "the pad keeps keyboard focus after the handover" (Just pad)
    capture session "daily-build"
    modKeys (clientEnvironment session) ["grave"]
    picture session termOutput "the hidden pad uncovers term's own tile" [Paint Blue (Box 1 1 798 598) True]
    streamShows stream "the restored pad hides with Mod-grave" (windows Nothing [("term", "hidden")])
    capture session "daily-build-pad-hidden"
    runInputDriver (clientEnvironment session) ["-k", "XF86AudioRaiseVolume"]
    launchedOnce session "XF86AudioRaiseVolume works in the new image" "amixer -D pulse sset Master 2%+"

barReconnect :: IO ()
barReconnect = withDailySession "m3-daily-bar" (dailyOptions 1) $ \session -> withSubscription session "daily-bar-subscription.jsonl" $ \stream -> do
  let probe = sessionProbe session
      marker = homeDirectory (sessionHome session) </> "spawn-marker"
      markerLines = doesFileExist marker >>= \exists -> if exists then lines <$> readFile marker else pure []
  output <- placedOutput <$> startedOn session "term"
  let onTerm title = bar [(output, "term")] [workspace "term" [Occupied, Visible, Focused] "Tall"] title []
      onEmacs = bar [(output, "emacs")] [workspace "term" [Occupied] "Tall", workspace "emacs" [Visible, Focused] "Tall"] Nothing []
      restartShows phase expected = do
        before <- nub . map snapSession <$> stream
        modKeys (clientEnvironment session) ["q"]
        shots <- waitUntil (phase <> " starts a fresh IPC session") assertionTimeoutMicroseconds stream (\shots -> length (nub (map snapSession shots)) > length before)
        fresh <- maybe (failIntegration (phase <> ": no snapshot of the new session")) pure (find ((`notElem` before) . snapSession) shots)
        report "FRESH_SESSION" (phase <> ": " <> Text.unpack (snapSession fresh) <> " revision " <> show (snapRevision fresh) <> " " <> show (barView fresh))
        assertEqual (phase <> ": the new session's first snapshot matches the windows") expected (barView fresh)
        (_, arguments) <- processImage (sessionPid session)
        assertEqual (phase <> ": the daily process keeps its pid and restores") True ("--restore" `elem` arguments)
        startupRanOnce session (phase <> ": no startup command ran again")
  _ <- openWindow probe Blue
  second <- openWindow probe Green
  waitFocus session "the newest term window has keyboard focus" (Just second)
  setTitle probe second "daily bar first session"
  streamShows stream "the bar shows the titled term window" (onTerm (Just "daily bar first session"))
  capture session "daily-bar-first-session"
  _ <- sekictl session ["spawn", "printf 'once\\n' >> '" <> marker <> "'"]
  _ <- waitUntil "the spawned marker runs once" assertionTimeoutMicroseconds markerLines (== ["once"])
  restartShows "the first Mod-q" (onTerm (Just "daily bar first session"))
  capture session "daily-bar-second-session"
  modKeys (clientEnvironment session) ["2"]
  streamShows stream "the second session shows the empty emacs workspace" onEmacs
  restartShows "the second Mod-q" onEmacs
  capture session "daily-bar-third-session"
  modKeys (clientEnvironment session) ["1"]
  waitFocus session "term's window has keyboard focus in the third session" (Just second)
  setTitle probe second "daily bar third session"
  streamShows stream "the third session shows the new title" (onTerm (Just "daily bar third session"))
  markerLines >>= assertEqual "the spawn was not replayed by either restart" ["once"]
  picture session output "term tiles its two windows in the third session" [Paint Green (Box 1 1 398 598) True, Paint Blue (Box 401 1 398 598) False]
  capture session "daily-bar"

touchpadName :: Text
touchpadName = "ELAN06C7:00 04F3:3193 Touchpad"

touchpadSettings :: [Text]
touchpadSettings = ["tap", "click_method", "natural_scroll"]

backendKeyboards :: [Text]
backendKeyboards = ["seki-private-backend-keyboard", "seki-private-second-keyboard"]

hotplugKeyboard :: Text
hotplugKeyboard = "seki-private-hotplug-keyboard"

namedDevices :: [Text] -> Snapshot -> [DeviceView]
namedDevices names shot = [d | d <- snapDevices shot, maybe False (`elem` names) (deviceName d)]

hotplug :: IO ()
hotplug = do
  backend <- requireSetting "SEKI_INPUT_BACKEND"
  withSystemTempDirectory "seki-daily-hotplug" $ \directory -> do
    let plugged = directory </> "plugged"
        options = (dailyOptions 2){optionRiverEnvironment = [("LD_PRELOAD", backend), ("SEKI_FIXTURE_HOTPLUG", plugged), ("SEKI_FIXTURE_HOTPLUG_POINTER", Text.unpack touchpadName)]}
    withDailySession "m3-daily-hotplug" options $ \session -> do
      hotplugDevices session plugged
      hotplugOutput session

hotplugDevices :: DailySession -> FilePath -> IO ()
hotplugDevices session plugged = do
  ready <- projected (settled backendKeyboards) "the daily keyboard rule's results reach both backend keyboards" (== map dailyKeyboard backendKeyboards)
  report "BACKEND_KEYBOARDS" (show ready)
  pointer <- projected (\shot -> [filter ((`elem` touchpadSettings) . fst) (deviceResults d) | d <- namedDevices ["seki-private-backend-pointer"] shot]) "the backend pointer is in the inventory" (not . null)
  assertEqual "the touchpad rule applies to no backend pointer" [[]] pointer
  repeats <- waitUntil "river applies the rule's 25 Hz repeat after 660 ms" assertionTimeoutMicroseconds (filter ("fixture group repeat observed" `isPrefixOf`) . lines <$> renderManagedProcessLogs (riverProcess (sessionRiver session))) (elem "fixture group repeat observed 25 660")
  report "RIVER_REPEAT" (show repeats)
  writeFile plugged ""
  added <- projected (settled [hotplugKeyboard, touchpadName]) "the plugged keyboard gets the daily keyboard results and the touchpad the touchpad results" (== sort [dailyKeyboard hotplugKeyboard, (Just touchpadName, [("click_method", "unavailable"), ("natural_scroll", "unavailable"), ("tap", "unavailable")], Nothing)])
  report "PLUGGED_DEVICES" (show added)
  removeFile plugged
  gone <- projected (map deviceName . snapDevices) "the unplugged devices leave the inventory" (\names -> all ((`notElem` names) . Just) [hotplugKeyboard, touchpadName])
  report "AFTER_UNPLUG" (show gone)
  projected (settled backendKeyboards) "the backend keyboards keep their results after the unplug" (== map dailyKeyboard backendKeyboards) >>= report "BACKEND_KEYBOARDS_AFTER_UNPLUG" . show
 where
  keyboardResults = [("keymap", "sent"), ("repeat_info", "sent")]
  settled names shot = sort [(deviceName d, sort (deviceResults d), deviceKeymap d) | d <- namedDevices names shot, deviceLifecycle d == "ready"]
  dailyKeyboard name = (Just name, keyboardResults, Just (homeKeymap (sessionHome session)))
  projected project phase = waitUntil phase assertionTimeoutMicroseconds (project <$> snapshot session)

hotplugOutput :: DailySession -> IO ()
hotplugOutput session = do
  let probe = sessionProbe session
      randr arguments = void (runProcessCapture (clientEnvironment session) "wlr-randr" arguments)
  termOutput <- placedOutput <$> startedOn session "term"
  emacs <- startedOn session "emacs"
  let emacsOutput = placedOutput emacs
  tile <- openWindow probe Green
  editor <- openWindowWith probe (windowSpec Green){specAppId = CustomAppId "Emacs"}
  let bothShown :: [(Text, Text)] -> Text -> BarView
      bothShown pads focused = bar [(termOutput, "term"), (emacsOutput, "emacs")] [workspace "term" ([Occupied, Visible] <> [Focused | focused == "term"]) "Tall", workspace "emacs" ([Occupied, Visible] <> [Focused | focused == "emacs"]) "Tall"] Nothing pads
      remaining shown = bar [(termOutput, shown)] [workspace "term" ([Occupied] <> [Visible | shown == "term"] <> [Focused | shown == "term"]) "Tall", workspace "emacs" ([Occupied] <> [Visible | shown == "emacs"] <> [Focused | shown == "emacs"]) "Tall"] Nothing [("term", "shown")]
  click (sessionSocket session) (sessionExtent session) (placedX emacs + 400, 300)
  waitFocus session "a click focuses the Emacs window on the other output" (Just editor)
  modKeys (clientEnvironment session) ["grave"]
  launchedOnce session "Mod-grave launches the scratchpad terminal" "alacritty --class seki-scratchpad"
  pad <- openWindowWith probe (windowSpec Blue){specAppId = CustomAppId "seki-scratchpad"}
  waitFocus session "the shown pad has keyboard focus" (Just pad)
  picture session emacsOutput "the pad shows on emacs's output" [Paint Blue (Box 1 1 798 598) True]
  picture session termOutput "term keeps its tile on the other output" [Paint Green (Box 1 1 798 598) False]
  polledShows session "the pad is shown on emacs" (bothShown [("term", "shown")] "emacs")
  capture session "daily-hotplug-before"
  randr ["--output", Text.unpack emacsOutput, "--off"]
  polledShows session "the removed output's workspace returns to hidden with the pad still shown on it" (remaining "term")
  waitFocus session "focus moves to the surviving output's tile" (Just tile)
  picture session termOutput "the surviving output shows term" [Paint Green (Box 1 1 798 598) True]
  capture session "daily-hotplug-off"
  modKeys (clientEnvironment session) ["2"]
  polledShows session "Mod-2 brings emacs to the remaining output" (remaining "emacs")
  waitFocus session "the pad has keyboard focus on the remaining output" (Just pad)
  picture session termOutput "the pad covers the Emacs window inside the remaining output" [Paint Blue (Box 1 1 798 598) True]
  capture session "daily-hotplug-moved"
  modKeys (clientEnvironment session) ["1"]
  polledShows session "Mod-1 shows term again" (remaining "term")
  randr ["--output", Text.unpack emacsOutput, "--on", "--custom-mode", "800x600", "--pos", show (placedX emacs) <> ",0"]
  polledShows session "the re-added output shows emacs again" (bothShown [("term", "shown")] "term")
  picture session emacsOutput "the pad is back inside the re-added output" [Paint Blue (Box 1 1 798 598) False]
  picture session termOutput "term keeps the focused tile" [Paint Green (Box 1 1 798 598) True]
  capture session "daily-hotplug"

-- | Match argv[0], not comm: a nix wrapProgram daemon runs as .fcitx5-wrapped and keeps its argv[0].
processesWithEnvironment :: String -> (String, String) -> IO [Int]
processesWithEnvironment command (name, value) = do
  entries <- listDirectory "/proc"
  fmap catMaybes . forM [read entry | entry <- entries, not (null entry), all isDigit entry] $ \pid -> do
    found <- try @IOException ((,) <$> BS.readFile ("/proc/" <> show pid <> "/cmdline") <*> BS.readFile ("/proc/" <> show pid <> "/environ"))
    pure $ case found of
      Right (cmdline, environ) | takeFileName (BS8.unpack (BS8.takeWhile (/= '\0') cmdline)) == command && BS8.pack (name <> "=" <> value) `elem` BS.split 0 environ -> Just pid
      _ -> Nothing

imeHandover :: IO ()
imeHandover = withDailyHome (dailyOptions 1) $ \home -> do
  daemon <- findExecutable "fcitx5" >>= maybe (failIntegration "fcitx5 is not installed") pure
  imeBase <- prepareFcitxEnvironment (riverEnvironment (homeRiver home))
  withPrivateDbus imeBase $ \ime -> do
    let carried = [(name, value) | (name, value) <- ime, lookup name (dailyEnvironment home []) /= Just value]
        quote value = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) value <> "'"
    writeExecutable (homeBin home </> "fcitx5") (loggingStub (homeLaunchLog home) <> "exec env " <> unwords [name <> "=" <> quote value | (name, value) <- carried] <> " " <> quote daemon <> " -D -k\n")
    runDaily "m3-daily-ime" home [] $ \session -> do
      let client = clientEnvironment session
      bus <- maybe (failIntegration "the IME environment has no private D-Bus") pure (lookup "DBUS_SESSION_BUS_ADDRESS" ime)
      preflightFootIme client
      _ <- waitUntil "the daily startup hook's fcitx5 serves the private D-Bus" assertionTimeoutMicroseconds (isRight <$> try @IntegrationFailure (runProcessCapture ime "fcitx5-remote" ["--check"])) id
      before <- snapshot session
      modKeys (clientEnvironment session) ["q"]
      after <- waitSnapshot session "Mod-q hands over to a fresh IPC session" ((/= snapSession before) . snapSession)
      report "HANDOVER" (Text.unpack (snapSession before) <> " -> " <> Text.unpack (snapSession after))
      (_, arguments) <- processImage (sessionPid session)
      assertEqual "the daily process keeps its pid and restores" True ("--restore" `elem` arguments)
      capture session "daily-ime-handover"
      withImeFootProbe client $ \terminal -> do
        _ <- waitSnapshot session "foot is managed on term with keyboard focus" (\s -> snapKeyboard s && any (\w -> wsTag w == "term" && wsOccupied w && wsFocused w) (snapWorkspaces s))
        capture session "daily-ime-foot"
        committed <- withFcitxVirtualKeyboardOwner ime $ \owner -> commitMozcText "m3-daily-ime" ime client owner terminal
        report "IME_COMMIT" committed
        assertEqual "foot receives exactly the Mozc commit after the handover" "日本語" committed
        capture session "daily-ime"
      daemons <- processesWithEnvironment "fcitx5" ("DBUS_SESSION_BUS_ADDRESS", bus)
      report "FCITX5_PROCESSES" (show daemons)
      assertEqual "one fcitx5 process from this fixture exists" 1 (length daemons)

xtermTitle :: Text
xtermTitle = "seki-daily-xterm"

xwaylandManagement :: IO ()
xwaylandManagement = withDailySession "m3-daily-xwayland" (dailyOptions 1){optionXwayland = True} $ \session -> do
  let probe = sessionProbe session
      client = clientEnvironment session
      modShift k = runInputDriver client ["-M", "logo", "-M", "shift", "-k", k, "-m", "shift", "-m", "logo"]
      -- xdotool fails while X focus is still on the root window, before any X client maps.
      xFocus phase = void (waitUntil phase assertionTimeoutMicroseconds (either (const Nothing) (Just . Text.pack) <$> try @IntegrationFailure (queryX11FocusedWindowName client)) (== Just xtermTitle))
      xGeometry = do
        found <- runProcessCapture client "xdotool" ["search", "--name", Text.unpack xtermTitle, "getwindowgeometry", "--shell"]
        pure (sort [line | line <- lines (BS8.unpack found), any (`isPrefixOf` line) ["X=", "Y=", "WIDTH=", "HEIGHT="]])
  output <- placedOutput <$> startedOn session "term"
  let onTerm title others = bar [(output, "term")] (workspace "term" [Occupied, Visible, Focused] "Tall" : others) title []
  native <- openWindow probe Green
  waitFocus session "the native window has keyboard focus" (Just native)
  bracket (startManagedProcess "xterm" client (proc "xterm" ["-title", Text.unpack xtermTitle, "-fa", "monospace", "-fs", "12", "-e", "cat"])) stopManagedProcess $ \xterm -> do
    xFocus "the new xterm has X input focus"
    waitFocus session "the native window loses keyboard focus to the xterm" Nothing
    picture session output "the xterm tiles as master beside the native window" [Paint Green (Box 401 1 398 598) False]
    geometry <- waitUntil "the xterm's X window fills the master tile" assertionTimeoutMicroseconds xGeometry (== ["HEIGHT=598", "WIDTH=398", "X=1", "Y=1"])
    report "XTERM_GEOMETRY" (show geometry)
    polledShows session "the bar shows the focused xterm's title" (onTerm (Just xtermTitle) [])
    capture session "daily-xwayland-tiled"
    modKeys (clientEnvironment session) ["j"]
    waitFocus session "Mod-j moves keyboard focus to the native window" (Just native)
    picture session output "the native window takes the focused border" [Paint Green (Box 401 1 398 598) True]
    click (sessionSocket session) (sessionExtent session) (200, 300)
    xFocus "a click gives the xterm X input focus"
    waitFocus session "the click takes keyboard focus from the native window" Nothing
    picture session output "the clicked xterm takes the focused border" [Paint Green (Box 401 1 398 598) False]
    modShift "2"
    waitFocus session "Mod-Shift-2 leaves the native window focused on term" (Just native)
    picture session output "term shows only the native window" [Paint Green (Box 1 1 798 598) True]
    polledShows session "the xterm is on emacs" (onTerm Nothing [workspace "emacs" [Occupied] "Tall"])
    modKeys (clientEnvironment session) ["2"]
    xFocus "viewing emacs gives the xterm X input focus"
    picture session output "emacs shows only the xterm" []
    capture session "daily-xwayland-moved"
    modShift "1"
    modKeys (clientEnvironment session) ["1"]
    xFocus "the xterm moved back to term has X input focus"
    picture session output "the xterm tiles beside the native window again" [Paint Green (Box 401 1 398 598) False]
    polledShows session "the xterm is back on term" (onTerm (Just xtermTitle) [])
    capture session "daily-xwayland"
    modShift "c"
    exit <- waitForProcessWithTimeout "Mod-Shift-c closes the xterm" xterm
    report "XTERM_EXIT" (show exit)
    waitFocus session "the native window has keyboard focus after the close" (Just native)
    picture session output "term shows only the native window after the close" [Paint Green (Box 1 1 798 598) True]
    polledShows session "the bar shows only the native window" (onTerm Nothing [])
    capture session "daily-xwayland-closed"

data Flood = Flood {floodReplies :: IO Int, floodRevision :: IO Integer, unreadFirst :: Int, unreadBacklog :: IO Int}

withFlood :: DailySession -> Int -> (Flood -> IO a) -> IO a
withFlood session workers action = do
  let path = riverRuntimeDirectory (sessionRiver session) </> "seki" </> "ipc.sock"
      open = do
        client <- socket AF_UNIX Stream defaultProtocol
        connect client (SockAddrUnix path) `onException` close client
        pure client
  stop <- newIORef False
  replies <- newIORef (0 :: Int)
  latest <- newIORef (0 :: Integer)
  let request client pending bytes expected = do
        sendAll client bytes
        (reply, rest) <- replyLine client pending
        unless (expected `BS.isInfixOf` reply) (failIntegration ("flood reply lacks " <> show expected <> ": " <> show (BS.take 200 reply)))
        atomicModifyIORef' replies (\count -> (count + 1, ()))
        pure (reply, rest)
      work client pending =
        readIORef stop >>= \stopped -> unless stopped $ do
          (_, afterSnapshot) <- request client pending "{\"version\":1,\"id\":\"flood\",\"op\":\"snapshot\"}\n" "\"snapshot\""
          (receipt, rest) <- request client afterSnapshot "{\"version\":1,\"id\":\"flood\",\"op\":\"workspace\",\"tag\":\"term\"}\n" "\"revision\""
          revision <- requireRight "flood receipt" (receiptRevision receipt)
          atomicModifyIORef' latest (\seen -> (max seen revision, ()))
          work client rest
  bracket open close $ \unread -> do
    sendAll unread "{\"version\":1,\"id\":\"unread\",\"op\":\"subscribe\"}\n"
    first <- waitUntil "the unread subscriber receives its first snapshot" assertionTimeoutMicroseconds (socketQueuedBytes unread) (> 0)
    finished <- forM [1 .. workers] $ \_ -> do
      done <- newEmptyMVar
      _ <- forkFinally (bracket open close (`work` BS.empty)) (putMVar done)
      pure done
    result <- action (Flood (readIORef replies) (readIORef latest) first (socketQueuedBytes unread)) `finally` writeIORef stop True
    outcomes <- forM finished (timeout assertionTimeoutMicroseconds . takeMVar)
    forM_ outcomes $ \outcome -> case outcome of
      Nothing -> failIntegration "an IPC flood worker did not stop"
      Just (Left failure) -> failIntegration ("an IPC flood worker failed: " <> show failure)
      Just (Right ()) -> pure ()
    pure result
 where
  replyLine client pending = case BS.elemIndex 10 pending of
    Just index -> pure (BS.take index pending, BS.drop (index + 1) pending)
    Nothing ->
      timeout assertionTimeoutMicroseconds (recv client 65536) >>= \received -> case received of
        Nothing -> failIntegration "an IPC flood reply timed out"
        Just chunk | BS.null chunk -> failIntegration "IPC closed a flood connection"
        Just chunk -> replyLine client (pending <> chunk)

subscriberStall :: Flood -> IO (Int, Int, Integer, Integer)
subscriberStall flood = do
  before <- unreadBacklog flood
  from <- floodRevision flood
  threadDelay 200000
  (,,,) before <$> unreadBacklog flood <*> pure from <*> floodRevision flood

-- | Polls every 1 ms, because waitUntil's 20 ms poll is too coarse for a key-to-focus sample.
wtypeKeyToFocus :: DailySession -> String -> Maybe ProbeWindow -> IO (Integer, Maybe ProbeWindow)
wtypeKeyToFocus session keyName from = do
  started <- getMonotonicTimeNSec
  bracket (startManagedProcess "wtype" (clientEnvironment session) (proc "wtype" ["-M", "logo", "-k", keyName, "-m", "logo"])) stopManagedProcess $ \_ -> do
    let deadline = started + fromIntegral assertionTimeoutMicroseconds * 1000
        poll = do
          owner <- keyboardOwner <$> keyboardSnapshot (sessionProbe session)
          now <- getMonotonicTimeNSec
          if owner /= from
            then pure (toInteger (now - started), owner)
            else if now >= deadline then failIntegration ("Mod-" <> keyName <> " did not move keyboard focus from " <> show from) else threadDelay 1000 >> poll
    poll

loadResponsive :: IO ()
loadResponsive = withDailySession "m3-daily-load" (dailyOptions 1) $ \session -> do
  let probe = sessionProbe session
      surface = sessionSocket session
  output <- placedOutput <$> startedOn session "term"
  dragged <- openWindow probe Blue
  stacked <- openWindow probe Green
  master <- openWindow probe Red
  waitFocus session "the newest window has keyboard focus as master" (Just master)
  picture session output "three windows tile with the newest as master" [Paint Green (Box 401 1 398 298) False, Paint Blue (Box 401 301 398 298) False]
  capture session "daily-load-tiled"
  (idle, idleOwner) <- wtypeKeyToFocus session "j" (Just master)
  assertEqual "Mod-j without the flood moves focus down the stack" (Just stacked) idleOwner
  report "WTYPE_KEY_TO_FOCUS_IDLE_NS" (show idle)
  modKeys (clientEnvironment session) ["k"]
  waitFocus session "Mod-k returns focus to the master" (Just master)
  started <- getMonotonicTimeNSec
  (count, loaded, frames) <- withFlood session 8 $ \flood -> do
    _ <- waitUntil "the flood is answering requests" assertionTimeoutMicroseconds (floodReplies flood) (>= 100)
    stall <- waitUntil "the never-reading subscriber holds 32 unread snapshots and stops growing while the flood advances the revision" assertionTimeoutMicroseconds (subscriberStall flood) (\(before, after, from, to) -> before >= 32 * unreadFirst flood && before == after && to > from)
    report "SUBSCRIBER_STALL" (show (unreadFirst flood, stall))
    first <- captureOutput surface output
    (moved, resized) <- withHeldLogoIn (clientEnvironment session) probe . withVirtualPointer surface (sessionExtent session) $ \pointer -> do
      let drag button from to = do
            pointerMoveTo pointer from
            _ <- waitUntil ("the pointer is over the dragged window at " <> show from) assertionTimeoutMicroseconds (pointerSnapshot probe) ((== Just dragged) . pointerOwner)
            pointerPress pointer button
            pointerMoveTo pointer to
            pointerRelease pointer button
      drag LeftButton (600, 450) (400, 300)
      picture session output "a logo drag floats the bottom tile moved by the offset during the flood" [Paint Green (Box 401 1 398 598) False, Paint Blue (Box 201 151 398 298) True]
      moved <- captureOutput surface output
      capture session "daily-load-moved"
      drag RightButton (500, 350) (560, 390)
      picture session output "a logo right drag grows the float by the offset during the flood" [Paint Green (Box 401 1 398 598) False, Paint Blue (Box 201 151 458 338) True]
      resized <- captureOutput surface output
      capture session "daily-load-resized"
      pure (moved, resized)
    _ <- waitUntil "the float commits its resized size" assertionTimeoutMicroseconds (Map.lookup dragged <$> committedSizes probe) (== Just (458, 338))
    (latency, owner) <- wtypeKeyToFocus session "j" (Just dragged)
    assertEqual "Mod-j during the flood moves focus from the float to the master" (Just master) owner
    receipt <- sekictl session ["workspace", "term"]
    revision <- requireRight "a fresh sekictl receipt during the flood" (receiptRevision receipt)
    report "FRESH_RECEIPT_REVISION" (show revision)
    count <- floodReplies flood
    pure (count, latency, map imageRgb [first, moved, resized])
  ended <- getMonotonicTimeNSec
  assertEqual "captures keep changing while the subscriber never reads" (length frames) (length (nub frames))
  report "WTYPE_KEY_TO_FOCUS_FLOOD_NS" (show loaded)
  report "FLOOD" (show count <> " replies in " <> show ((ended - started) `div` 1000000) <> " ms")
  picture session output "the float keeps its moved and resized geometry after the flood" [Paint Green (Box 401 1 398 598) False, Paint Blue (Box 201 151 458 338) False]
  capture session "daily-load"
