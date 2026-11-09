{-# LANGUAGE OverloadedStrings #-}

{- | The daily-use configuration on real river (M3-07). Each lane runs the
@seki-daily-config@ executable as a real process with a private HOME whose
PATH stubs log each launch, and drives it through @sekictl@, @wtype@, the
virtual pointer, and the native probe.
-}
module Seki.Integration.Daily (dailyScenarios) where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, bracket, finally, onException, try)
import Control.Monad (forM, forM_, unless, void)
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
import Seki.Integration.Fixture (BorderStyle (..), Box (..), NestedRiver (..), Paint (..), outputPicture, pictureErrorWith, png, withHeldLogoIn, withKeyboard, withNestedRiverEnvironment, withNestedXwaylandRiver)
import Seki.Integration.Harness
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, findExecutable, listDirectory, removeFile)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (readSymbolicLink, setFileMode)
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

-- | What a lane needs before the daily process starts, held for the session's lifetime.
newtype Around = Around (forall r. NestedRiver -> FilePath -> (Additions -> IO r) -> IO r)

data Additions = Additions
  { additionStubs :: [(String, String)]
  -- ^ Programs on the private PATH that replace the logging stub, by name.
  , additionEnvironment :: [(String, String)]
  -- ^ Further environment of the daily process.
  , additionClient :: [(String, String)]
  -- ^ Environment the lane's own clients need, such as a private D-Bus.
  }

data DailyOptions = DailyOptions
  { optionOutputs :: Int
  , optionXwayland :: Bool
  , optionRiverEnvironment :: [(String, String)]
  , optionAround :: Around
  }

dailyOptions :: Int -> DailyOptions
dailyOptions outputs = DailyOptions outputs False [] (Around (\_ _ use -> use (Additions [] [] [])))

data DailySession = DailySession
  { sessionName :: String
  , sessionRiver :: NestedRiver
  , sessionPid :: Int
  , sessionHome :: FilePath
  , sessionLaunchLog :: FilePath
  , sessionKeymap :: FilePath
  , sessionCli :: FilePath
  , sessionAdditions :: Additions
  , sessionProbe :: Probe
  }

{- | Run the daily executable on a fresh nested river with @outputs@ 800 by
600 outputs side by side in name order, and hand the lane a session once
IPC reports every output and each startup command has run once. The lane
ends with the same startup check.
-}
withDailySession :: String -> DailyOptions -> (DailySession -> IO a) -> IO a
withDailySession name options body = do
  executable <- setting "SEKI_DAILY_CONFIG_EXECUTABLE"
  cli <- setting "SEKICTL_EXECUTABLE"
  backend <- setting "SEKI_INPUT_BACKEND"
  (if optionXwayland options then withNestedXwaylandRiver else withNestedRiverEnvironment) (optionRiverEnvironment options) (optionOutputs options) $ \nested -> withSystemTempDirectory "seki-daily-home" $ \home -> do
    let bin = home </> "bin"
        launches = home </> "launches"
        keymap = home </> ".config" </> "seki" </> "keymap.xkb"
        Around around = optionAround options
    createDirectoryIfMissing True bin
    createDirectoryIfMissing True (takeDirectory keymap)
    copyFile (takeDirectory backend </> "daily.xkb") keymap
    around nested home $ \additions -> do
      forM_ ([(program, loggingStub launches) | program <- stubPrograms] <> additionStubs additions) $ \(program, script) -> writeExecutable (bin </> program) script
      let base = riverEnvironment nested
          overrides =
            [ ("HOME", home)
            , ("PATH", bin <> ":" <> fromMaybe "/usr/bin:/bin" (lookup "PATH" base))
            , ("XDG_STATE_HOME", home </> ".local" </> "state")
            , ("XDG_DATA_HOME", home </> ".local" </> "share")
            , ("XDG_CACHE_HOME", home </> ".cache")
            ]
              <> additionEnvironment additions
          -- Without XDG_CONFIG_HOME the daily executable takes the keymap from $HOME/.config, as on the maintainer's desktop.
          environment = overrides <> filter ((`notElem` ("XDG_CONFIG_HOME" : map fst overrides)) . fst) base
          surface = riverSocket nested
          count = optionOutputs options
      bracket (startManagedProcess name environment (proc executable [])) stopManagedProcess $ \process ->
        ( do
            pid <- managedProcessPid process >>= maybe (failIntegration (name <> ": the daily process has no pid")) pure
            outputs <- sort . map snd <$> waitUntil "seki shows every output" assertionTimeoutMicroseconds (outputNames surface) ((== count) . length)
            forM_ (zip [0 :: Int ..] outputs) $ \(index, output) ->
              void (runProcessCapture base "wlr-randr" ["--output", Text.unpack output, "--custom-mode", "800x600", "--pos", show (index * 800) <> ",0", "--scale", "1", "--transform", "normal"])
            withKeyboard base . withProbe surface $ \probe -> do
              let session = DailySession name nested pid home launches keymap cli additions probe
              _ <- waitSnapshot session "IPC shows a workspace on every output" (\s -> length (snapOutputs s) == count && all (isJust . outputWorkspace) (snapOutputs s))
              startupRanOnce session "each startup command runs once after the first output"
              result <- body session
              startupRanOnce session "each startup command still ran exactly once at the end of the lane"
              pure result
        )
          `onException` (renderManagedProcessLogs process >>= announce)

setting :: String -> IO String
setting variable = lookupEnv variable >>= maybe (failIntegration (variable <> " is unset; run through ci/run-integration.sh")) pure

-- | Every program the daily configuration starts or binds.
stubPrograms :: [String]
stubPrograms = ["waybar", "swaybg", "fcitx5", "swayidle", "swaylock", "alacritty", "amixer", "brightnessctl"]

loggingStub :: FilePath -> String
loggingStub launches = "#!/bin/sh\nline=${0##*/}\nfor argument in \"$@\"; do line=\"$line $argument\"; done\nprintf '%s\\n' \"$line\" >> '" <> launches <> "'\n"

writeExecutable :: FilePath -> String -> IO ()
writeExecutable path script = writeFile path script >> setFileMode path 0o755

-- | What the stubs log for the daily startup hook, in any order.
startupCommands :: [String]
startupCommands = ["waybar", "swaybg -i /usr/share/backgrounds/archlinux/wave.png -m fill", "fcitx5", "swayidle -w timeout 600 swaylock -f"]

startupRanOnce :: DailySession -> String -> IO ()
startupRanOnce session phase = do
  _ <- waitUntil phase assertionTimeoutMicroseconds (recordedLaunches session) (\logged -> all (`elem` logged) startupCommands)
  logged <- recordedLaunches session
  assertEqual phase [(command, 1 :: Int) | command <- startupCommands] [(command, length (filter (== command) logged)) | command <- startupCommands]

recordedLaunches :: DailySession -> IO [String]
recordedLaunches session = do
  exists <- doesFileExist (sessionLaunchLog session)
  if exists then lines . BS8.unpack <$> BS8.readFile (sessionLaunchLog session) else pure []

launchedOnce :: DailySession -> String -> String -> IO ()
launchedOnce session phase command = void (waitUntil phase assertionTimeoutMicroseconds (filter (== command) <$> recordedLaunches session) (== [command]))

clientEnvironment :: DailySession -> [(String, String)]
clientEnvironment = riverEnvironment . sessionRiver

sessionSocket :: DailySession -> FilePath
sessionSocket = riverSocket . sessionRiver

-- | Press the keys with the logo modifier held, as Mod4 bindings expect.
key :: DailySession -> [String] -> IO ()
key session keys = runInputDriver (clientEnvironment session) (["-M", "logo"] <> concatMap (\k -> ["-k", k]) keys <> ["-m", "logo"])

sekictl :: DailySession -> [String] -> IO BS.ByteString
sekictl session = runProcessCapture (clientEnvironment session) (sessionCli session)

snapshot :: DailySession -> IO Snapshot
snapshot session = sekictl session ["snapshot"] >>= requireRight "sekictl snapshot reply" . replySnapshot

waitSnapshot :: DailySession -> String -> (Snapshot -> Bool) -> IO Snapshot
waitSnapshot session phase = waitUntil phase assertionTimeoutMicroseconds (snapshot session)

waitFocus :: DailySession -> String -> Maybe ProbeWindow -> IO ()
waitFocus session phase wanted = void (waitUntil phase assertionTimeoutMicroseconds (keyboardSnapshot (sessionProbe session)) ((== wanted) . keyboardOwner))

-- | The applied revision of a command receipt.
receiptRevision :: BS.ByteString -> Either String Integer
receiptRevision bytes = eitherDecodeStrict' bytes >>= parseEither (withObject "reply" (\reply -> reply .: "result" >>= withObject "result" (.: "revision")))

-- | The executable path and command line of a live process.
processImage :: Int -> IO (FilePath, [String])
processImage pid = (,) <$> readSymbolicLink ("/proc/" <> show pid <> "/exe") <*> (map BS8.unpack . filter (not . BS.null) . BS.split 0 <$> BS.readFile ("/proc/" <> show pid <> "/cmdline"))

-- | The output IPC shows the workspace on.
shownOn :: Snapshot -> Text -> IO Text
shownOn shot tag = maybe (failIntegration ("no output shows " <> Text.unpack tag <> ": " <> show (snapOutputs shot))) pure (find ((== Just tag) . outputWorkspace) (snapOutputs shot) >>= outputName)

picture :: DailySession -> Text -> String -> [Paint] -> IO ()
picture session output phase = outputPicture (sessionName session) (sessionSocket session) output phase . pictureErrorWith dailyBorders

-- | The ported 1-pixel red focused and @#0000dd@ unfocused borders.
dailyBorders :: BorderStyle
dailyBorders = BorderStyle 1 (BS.pack [255, 0, 0]) (BS.pack [0, 0, 221])

-- | One key observed value of the lane, so the suite log is its transcript.
report :: String -> String -> IO ()
report label value = announce ("M3_DAILY_" <> label <> " " <> value)

{- | Save every current output side by side, in name order, as one PNG named
by the label, and announce the capture with its monotonic time.
-}
capture :: DailySession -> String -> IO ()
capture session label = do
  outputs <- sort . map snd <$> outputNames (sessionSocket session)
  frames <- mapM (captureOutput (sessionSocket session)) outputs
  stamp <- getMonotonicTimeNSec
  artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
  forM_ artifacts $ \directory -> do
    createDirectoryIfMissing True directory
    BS.writeFile (directory </> label <> ".png") (png (sideBySide frames))
  report "CAPTURE" (label <> " " <> show stamp)

sideBySide :: [Image] -> Image
sideBySide frames = Image width height (BS.concat [BS.concat (map (row y) frames) | y <- [0 .. height - 1]])
 where
  width = sum (map imageWidth frames)
  height = maximum (0 : map imageHeight frames)
  row y frame
    | y < imageHeight frame = BS.take (imageWidth frame * 3) (BS.drop (y * imageWidth frame * 3) (imageRgb frame))
    | otherwise = BS.replicate (imageWidth frame * 3) 0

{- | Run @sekictl subscribe@ into a JSONL file for the action, which reads
the snapshots received so far. The file is kept as an artifact.
-}
withSubscription :: DailySession -> String -> (IO [Snapshot] -> IO a) -> IO a
withSubscription session file action = do
  let path = riverRuntimeDirectory (sessionRiver session) </> file
      received = do
        exists <- doesFileExist path
        complete <- if exists then reverse . drop 1 . reverse . BS.split 10 <$> BS.readFile path else pure []
        forM (filter (not . BS.null) complete) (requireRight "subscription snapshot" . replySnapshot)
      keep = do
        artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
        exists <- doesFileExist path
        forM_ artifacts $ \directory -> if exists then createDirectoryIfMissing True directory >> copyFile path (directory </> file) else pure ()
  bracket (startManagedProcess "sekictl subscribe" (clientEnvironment session) (proc "/bin/sh" ["-c", "exec \"$1\" subscribe > \"$2\"", "seki-subscribe", sessionCli session, path])) stopManagedProcess $ \_ ->
    action received `finally` keep

-- | Wait until the latest snapshot the subscriber received shows the bar state.
barShows :: IO [Snapshot] -> String -> BarView -> IO ()
barShows stream phase expected = do
  _ <- waitUntil phase assertionTimeoutMicroseconds (fmap barView . lastOf <$> stream) (== Just expected)
  report "BAR" (phase <> ": " <> show expected)

lastOf :: [a] -> Maybe a
lastOf = listToMaybe . reverse

-- | The snapshot fields a bar renders: outputs with their workspaces, every workspace, the focused title, and the scratchpads.
data BarView = BarView [(Maybe Text, Maybe Text)] [WorkspaceView] (Maybe Text) [(Text, Text)]
  deriving (Eq)

-- | Outputs, then every workspace that is not empty, hidden, and Tall, as a bar would draw them.
instance Show BarView where
  show (BarView outputs workspaces title pads) =
    unwords
      [ "outputs=" <> show outputs
      , "workspaces=" <> show [(wsTag w, [flag | (flag, True) <- [('O', wsOccupied w), ('V', wsVisible w), ('F', wsFocused w)]], wsLayout w) | w <- workspaces, (wsOccupied w, wsVisible w, wsFocused w, wsLayout w) /= (False, False, False, Just "Tall")]
      , "title=" <> show title
      , "scratchpads=" <> show pads
      ]

barView :: Snapshot -> BarView
barView shot = BarView (sort [(outputName o, outputWorkspace o) | o <- snapOutputs shot]) (snapWorkspaces shot) (snapTitle shot) (snapPads shot)

data Flag = Occupied | Visible | Focused deriving (Eq)

-- | The bar state of the daily workspaces, where every workspace not listed is empty, hidden, and Tall.
bar :: [(Text, Text)] -> [WorkspaceView] -> Maybe Text -> [(Text, Text)] -> BarView
bar outputs listed = BarView (sort [(Just output, Just tag) | (output, tag) <- outputs]) [fromMaybe (workspace tag [] "Tall") (find ((== tag) . wsTag) listed) | tag <- dailyTags]

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

-- | A device's identity, kind, the status of each setting result, and the keymap path it was asked to load.
data DeviceView = DeviceView {deviceGeneration :: Integer, deviceName :: Maybe Text, deviceType :: Maybe Text, deviceLifecycle :: Text, deviceResults :: [(Text, Text)], deviceKeymap :: Maybe FilePath} deriving (Eq, Show)

data BuildView = BuildView {buildState :: Text, buildGeneration :: Integer, buildError :: Maybe Text, buildImage :: Maybe FilePath} deriving (Eq, Show)

-- | The snapshot of a @snapshot@ reply or a subscription line.
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
    generation <- d .: "identity" >>= withObject "identity" (.: "generation")
    results <- d .: "results" >>= mapM (withObject "result" (\r -> (,,) <$> r .: "setting" <*> r .: "status" <*> r .: "requested"))
    keymap <- traverse (withObject "keymap request" (.: "path")) (listToMaybe [requested | (setting', _, requested) <- results, setting' == ("keymap" :: Text)])
    DeviceView generation <$> d .: "name" <*> d .: "type" <*> d .: "lifecycle" <*> pure [(setting', status) | (setting', status, _ :: Value) <- results] <*> pure keymap

{- | Two outputs: term tiles two windows under Mirror Tall with a title set
through the probe, emacs shows an Emacs window on the other output, and the
scratchpad goes from pending to shown to hidden. The subscription's latest
snapshot matches the windows at each step.
-}
twoOutput :: IO ()
twoOutput = withDailySession "m3-daily-two-output" (dailyOptions 2) $ \session -> withSubscription session "daily-two-output-subscription.jsonl" $ \stream -> do
  let probe = sessionProbe session
  initial <- waitSnapshot session "term and emacs are shown on the two outputs" (\s -> all (\tag -> any ((== Just tag) . outputWorkspace) (snapOutputs s)) ["term", "emacs"])
  termOutput <- shownOn initial "term"
  emacsOutput <- shownOn initial "emacs"
  let outputs = [(termOutput, "term"), (emacsOutput, "emacs")]
      shown pads title = bar outputs [workspace "term" [Occupied, Visible, Focused] "Mirror Tall", workspace "emacs" [Occupied, Visible] "Tall"] title pads
  report "OUTPUTS" (show outputs)
  _ <- openWindow probe Blue
  _ <- openWindowWith probe (windowSpec Green){specAppId = CustomAppId "Emacs"}
  second <- openWindow probe Green
  waitFocus session "the newest term window has keyboard focus" (Just second)
  setTitle probe second "daily two-output"
  key session ["space"]
  picture session termOutput "term tiles its two windows under Mirror Tall" [Paint Green (Box 1 1 798 298) True, Paint Blue (Box 1 301 798 298) False]
  picture session emacsOutput "emacs shows the Emacs window on the other output" [Paint Green (Box 1 1 798 598) False]
  barShows stream "the bar shows both outputs, the title, and the layout" (shown [] (Just "daily two-output"))
  capture session "daily-two-output-windows"
  key session ["grave"]
  launchedOnce session "Mod-grave launches the scratchpad terminal" "alacritty --class seki-scratchpad"
  barShows stream "the launched scratchpad is pending" (shown [("term", "pending")] (Just "daily two-output"))
  pad <- openWindowWith probe (windowSpec Blue){specAppId = CustomAppId "seki-scratchpad"}
  waitFocus session "the shown pad has keyboard focus" (Just pad)
  setTitle probe pad "daily scratchpad"
  picture session termOutput "the pad floats over term clamped inside its 800 by 600 output" [Paint Blue (Box 1 1 798 598) True]
  barShows stream "the matching window shows the pad" (shown [("term", "shown")] (Just "daily scratchpad"))
  capture session "daily-two-output"
  key session ["grave"]
  waitFocus session "hiding the pad returns focus to the term window" (Just second)
  picture session termOutput "term tiles its two windows again with the pad hidden" [Paint Green (Box 1 1 798 298) True, Paint Blue (Box 1 301 798 298) False]
  barShows stream "the second Mod-grave hides the pad" (shown [("term", "hidden")] (Just "daily two-output"))
  capture session "daily-two-output-hidden"

{- | Build resources for an installed session whose compiler steps are stubs:
Cabal's build succeeds and @list-bin@ names the daily executable, so the
candidate the session copies and checks is that executable. The env stub
logs each step it runs.
-}
stubBuildResources :: Around
stubBuildResources = Around $ \_ home use -> do
  daily <- setting "SEKI_DAILY_CONFIG_EXECUTABLE"
  let resources = home </> "config-resources"
      templates = resources </> "templates"
      steps = home </> "build-steps"
  createDirectoryIfMissing True templates
  forM_ ["Main.hs", "seki-user.cabal", "cabal.project"] $ \file -> writeFile (templates </> file) ("-- " <> file <> " for @ARCHIVE@ built by @COMPILER@\n")
  forM_ ["archive", "ghc", "cabal"] $ \file -> writeFile (resources </> file) ""
  writeExecutable (resources </> "env") ("#!/bin/sh\nprintf '%s\\n' \"$3\" >> '" <> steps <> "'\ncase \"$3\" in\n  build) exit 0 ;;\n  list-bin) printf '%s\\n' '" <> daily <> "' ;;\n  *) exit 64 ;;\nesac\n")
  use
    (Additions [] [("SEKI_CONFIG_" <> variable, resources </> file) | (variable, file) <- [("ARCHIVE", "archive"), ("GHC", "ghc"), ("CABAL", "cabal"), ("ENV", "env"), ("TEMPLATES", "templates")]] [])

{- | A recompile of the running daily process while the scratchpad is shown
over term and an Emacs window is fullscreen on the other output. The
process keeps its pid and execs the checked candidate, IPC starts a fresh
session, and every window comes back where it was with no startup command
run again.
-}
buildHandover :: IO ()
buildHandover = withDailySession "m3-daily-build" (dailyOptions 2){optionAround = stubBuildResources} $ \session -> withSubscription session "daily-build-subscription.jsonl" $ \stream -> do
  let probe = sessionProbe session
      stateDirectory = sessionHome session </> ".local" </> "state" </> "seki"
  initial <- waitSnapshot session "term and emacs are shown on the two outputs" (\s -> all (\tag -> any ((== Just tag) . outputWorkspace) (snapOutputs s)) ["term", "emacs"])
  termOutput <- shownOn initial "term"
  emacsOutput <- shownOn initial "emacs"
  let windows title pads = bar [(termOutput, "term"), (emacsOutput, "emacs")] [workspace "term" [Occupied, Visible, Focused] "Tall", workspace "emacs" [Occupied, Visible] "Tall"] title pads
      restored phase = do
        picture session termOutput (phase <> ": the pad covers term") [Paint Green (Box 1 1 798 598) True]
        picture session emacsOutput (phase <> ": the Emacs window covers its output fullscreen") [Paint Blue (Box 0 0 800 600) False]
        barShows stream (phase <> ": the bar shows the pad and both workspaces") (windows (Just "daily build pad") [("term", "shown")])
  _ <- openWindow probe Blue
  editor <- openWindowWith probe (windowSpec Blue){specAppId = CustomAppId "Emacs"}
  requestFullscreen probe editor
  key session ["grave"]
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
  let (old, new) = span ((== oldSession) . snapSession) recorded
      oldStates = nub [buildState b | Just b <- map snapBuild old]
  -- A subscriber keeps one replaceable pending snapshot, so the old session's last build state may never arrive before stop.
  report "BUILD_STATES" (show (oldStates, map snapBuild (take 1 new)))
  assertEqual "the new session runs the candidate with no build error" [Just (BuildView "idle" 0 Nothing (Just image))] (map snapBuild (take 1 new))
  lines <$> readFile (sessionHome session </> "build-steps") >>= assertEqual "the session ran one build and one list-bin" ["build", "list-bin"]
  restored "after the handover"
  waitFocus session "the pad keeps keyboard focus after the handover" (Just pad)
  capture session "daily-build"
  key session ["grave"]
  picture session termOutput "the hidden pad uncovers term's own tile" [Paint Blue (Box 1 1 798 598) True]
  barShows stream "the restored pad hides with Mod-grave" (windows Nothing [("term", "hidden")])
  capture session "daily-build-pad-hidden"
  runInputDriver (clientEnvironment session) ["-k", "XF86AudioRaiseVolume"]
  launchedOnce session "XF86AudioRaiseVolume works in the new image" "amixer -D pulse sset Master 2%+"
  startupRanOnce session "no startup command ran again after the handover"

{- | A subscriber stays connected across two Mod-q restarts. Each restart
starts a fresh IPC session whose first snapshot already matches the
windows, and a spawn issued before the restarts runs exactly once.
-}
barReconnect :: IO ()
barReconnect = withDailySession "m3-daily-bar" (dailyOptions 1) $ \session -> withSubscription session "daily-bar-subscription.jsonl" $ \stream -> do
  let probe = sessionProbe session
      marker = sessionHome session </> "spawn-marker"
      markerLines = doesFileExist marker >>= \exists -> if exists then lines <$> readFile marker else pure []
  initial <- waitSnapshot session "term is shown" (any ((== Just "term") . outputWorkspace) . snapOutputs)
  output <- shownOn initial "term"
  let onTerm title = bar [(output, "term")] [workspace "term" [Occupied, Visible, Focused] "Tall"] title []
      onEmacs = bar [(output, "emacs")] [workspace "term" [Occupied] "Tall", workspace "emacs" [Visible, Focused] "Tall"] Nothing []
      restartShows phase expected = do
        before <- nub . map snapSession <$> stream
        key session ["q"]
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
  barShows stream "the bar shows the titled term window" (onTerm (Just "daily bar first session"))
  capture session "daily-bar-first-session"
  _ <- sekictl session ["spawn", "printf 'once\\n' >> '" <> marker <> "'"]
  _ <- waitUntil "the spawned marker runs once" assertionTimeoutMicroseconds markerLines (== ["once"])
  restartShows "the first Mod-q" (onTerm (Just "daily bar first session"))
  capture session "daily-bar-second-session"
  key session ["2"]
  barShows stream "the second session shows the empty emacs workspace" onEmacs
  restartShows "the second Mod-q" onEmacs
  capture session "daily-bar-third-session"
  key session ["1"]
  waitFocus session "term's window has keyboard focus in the third session" (Just second)
  setTitle probe second "daily bar third session"
  barShows stream "the third session shows the new title" (onTerm (Just "daily bar third session"))
  markerLines >>= assertEqual "the spawn was not replayed by either restart" ["once"]
  picture session output "term tiles its two windows in the third session" [Paint Green (Box 1 1 398 598) True, Paint Blue (Box 401 1 398 598) False]
  capture session "daily-bar"

-- | The maintainer's touchpad, the only device the daily touchpad rule names.
touchpadName :: Text
touchpadName = "ELAN06C7:00 04F3:3193 Touchpad"

backendKeyboards :: [Text]
backendKeyboards = ["seki-private-backend-keyboard", "seki-private-second-keyboard"]

hotplugKeyboard :: Text
hotplugKeyboard = "seki-private-hotplug-keyboard"

namedDevices :: [Text] -> Snapshot -> [DeviceView]
namedDevices names shot = [d | d <- snapDevices shot, maybe False (`elem` names) (deviceName d)]

{- | The input backend's keyboards get the daily keyboard rule's results, and
river reports the rule's repeat. A keyboard and a touchpad named as the
maintainer's plugged in later get the same keyboard results and the
touchpad rule's honest unavailable results, and leave the inventory when
unplugged. Turning off the output that shows the scratchpad hides its
workspace as spec 3.2 decides; viewing that workspace brings its window and
the pad, kept inside the remaining output, and turning the output back on
restores both outputs.
-}
hotplug :: IO ()
hotplug = do
  backend <- setting "SEKI_INPUT_BACKEND"
  withSystemTempDirectory "seki-daily-hotplug" $ \directory -> do
    let plugged = directory </> "plugged"
        options = (dailyOptions 2){optionRiverEnvironment = [("LD_PRELOAD", backend), ("SEKI_FIXTURE_HOTPLUG", plugged), ("SEKI_FIXTURE_HOTPLUG_POINTER", Text.unpack touchpadName)]}
    withDailySession "m3-daily-hotplug" options $ \session -> do
      let probe = sessionProbe session
          randr arguments = void (runProcessCapture (clientEnvironment session) "wlr-randr" arguments)
          keyboardResults = [("keymap", "sent"), ("repeat_info", "sent")]
          settled names shot = sort [(deviceName d, sort (deviceResults d), deviceKeymap d) | d <- namedDevices names shot, deviceLifecycle d == "ready"]
          dailyKeyboard name = (Just name, keyboardResults, Just (sessionKeymap session))
          projected project phase = waitUntil phase assertionTimeoutMicroseconds (project <$> snapshot session)
      initial <- waitSnapshot session "term and emacs are shown on the two outputs" (\s -> all (\tag -> any ((== Just tag) . outputWorkspace) (snapOutputs s)) ["term", "emacs"])
      termOutput <- shownOn initial "term"
      emacsOutput <- shownOn initial "emacs"
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
      tile <- openWindow probe Green
      editor <- openWindowWith probe (windowSpec Green){specAppId = CustomAppId "Emacs"}
      let emacsX = if emacsOutput > termOutput then 800 else 0 :: Word32
          bothShown :: [(Text, Text)] -> Text -> BarView
          bothShown pads focused = bar [(termOutput, "term"), (emacsOutput, "emacs")] [workspace "term" ([Occupied, Visible] <> [Focused | focused == "term"]) "Tall", workspace "emacs" ([Occupied, Visible] <> [Focused | focused == "emacs"]) "Tall"] Nothing pads
          remaining shown = bar [(termOutput, shown)] [workspace "term" ([Occupied] <> [Visible | shown == "term"] <> [Focused | shown == "term"]) "Tall", workspace "emacs" ([Occupied] <> [Visible | shown == "emacs"] <> [Focused | shown == "emacs"]) "Tall"] Nothing [("term", "shown")]
      click (sessionSocket session) (1600, 600) (emacsX + 400, 300)
      waitFocus session "a click focuses the Emacs window on the other output" (Just editor)
      key session ["grave"]
      launchedOnce session "Mod-grave launches the scratchpad terminal" "alacritty --class seki-scratchpad"
      pad <- openWindowWith probe (windowSpec Blue){specAppId = CustomAppId "seki-scratchpad"}
      waitFocus session "the shown pad has keyboard focus" (Just pad)
      picture session emacsOutput "the pad shows on emacs's output" [Paint Blue (Box 1 1 798 598) True]
      picture session termOutput "term keeps its tile on the other output" [Paint Green (Box 1 1 798 598) False]
      snapshotShows session "the pad is shown on emacs" (bothShown [("term", "shown")] "emacs")
      capture session "daily-hotplug-before"
      randr ["--output", Text.unpack emacsOutput, "--off"]
      snapshotShows session "the removed output's workspace returns to hidden with the pad still shown on it" (remaining "term")
      waitFocus session "focus moves to the surviving output's tile" (Just tile)
      picture session termOutput "the surviving output shows term" [Paint Green (Box 1 1 798 598) True]
      capture session "daily-hotplug-off"
      key session ["2"]
      snapshotShows session "Mod-2 brings emacs to the remaining output" (remaining "emacs")
      waitFocus session "the pad has keyboard focus on the remaining output" (Just pad)
      picture session termOutput "the pad covers the Emacs window inside the remaining output" [Paint Blue (Box 1 1 798 598) True]
      capture session "daily-hotplug-moved"
      key session ["1"]
      snapshotShows session "Mod-1 shows term again" (remaining "term")
      randr ["--output", Text.unpack emacsOutput, "--on", "--custom-mode", "800x600", "--pos", show emacsX <> ",0"]
      snapshotShows session "the re-added output shows emacs again" (bothShown [("term", "shown")] "term")
      picture session emacsOutput "the pad is back inside the re-added output" [Paint Blue (Box 1 1 798 598) False]
      picture session termOutput "term keeps the focused tile" [Paint Green (Box 1 1 798 598) True]
      capture session "daily-hotplug"

-- | The touchpad rule's settings.
touchpadSettings :: [Text]
touchpadSettings = ["tap", "click_method", "natural_scroll"]

-- | Wait until a fresh snapshot shows the bar state.
snapshotShows :: DailySession -> String -> BarView -> IO ()
snapshotShows session phase expected = do
  _ <- waitSnapshot session phase ((== expected) . barView)
  report "BAR" (phase <> ": " <> show expected)

{- | The daily configuration's own @fcitx5@ starts the real daemon on a
private D-Bus: the PATH entry logs the launch like the stubs and then execs
fcitx5 with the fixture's IME environment.
-}
realFcitx :: Around
realFcitx = Around $ \nested home use -> do
  daemon <- findExecutable "fcitx5" >>= maybe (failIntegration "fcitx5 is not installed") pure
  imeBase <- prepareFcitxEnvironment (riverEnvironment nested)
  withPrivateDbus imeBase $ \ime -> do
    let carried = [(name, value) | (name, value) <- ime, name `elem` imeVariables]
        quote value = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) value <> "'"
        wrapper = loggingStub (home </> "launches") <> "exec env " <> unwords [name <> "=" <> quote value | (name, value) <- carried] <> " " <> quote daemon <> " -D -k\n"
    use (Additions [("fcitx5", wrapper)] [] ime)
 where
  imeVariables = ["HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_CACHE_HOME", "XDG_STATE_HOME", "LANG", "LC_ALL", "XDG_SESSION_TYPE", "XDG_CURRENT_DESKTOP", "GTK_IM_MODULE", "QT_IM_MODULE", "XMODIFIERS", "SDL_IM_MODULE", "DBUS_SESSION_BUS_ADDRESS"]

-- | The pids of processes with the command name whose environment holds the variable, so only this fixture's processes count.
fixtureProcesses :: String -> (String, String) -> IO [Int]
fixtureProcesses command (name, value) = do
  entries <- listDirectory "/proc"
  fmap catMaybes . forM [read entry | entry <- entries, not (null entry), all isDigit entry] $ \pid -> do
    found <- try @IOException ((,) <$> BS.readFile ("/proc/" <> show pid <> "/comm") <*> BS.readFile ("/proc/" <> show pid <> "/environ"))
    pure $ case found of
      Right (comm, environ) | BS8.unpack (BS8.takeWhile (/= '\n') comm) == command && BS8.pack (name <> "=" <> value) `elem` BS.split 0 environ -> Just pid
      _ -> Nothing

{- | After a Mod-q handover, Mozc commits exactly @日本語@ into native foot
through the fcitx5 the daily startup hook launched once.
-}
imeHandover :: IO ()
imeHandover = withDailySession "m3-daily-ime" (dailyOptions 1){optionAround = realFcitx} $ \session -> do
  let ime = additionClient (sessionAdditions session)
      client = clientEnvironment session
  bus <- maybe (failIntegration "the IME environment has no private D-Bus") pure (lookup "DBUS_SESSION_BUS_ADDRESS" ime)
  preflightFootIme client
  _ <- waitUntil "the daily startup hook's fcitx5 serves the private D-Bus" assertionTimeoutMicroseconds (isRight <$> try @SomeException (runProcessCapture ime "fcitx5-remote" ["--check"])) id
  before <- snapshot session
  key session ["q"]
  after <- waitSnapshot session "Mod-q hands over to a fresh IPC session" ((/= snapSession before) . snapSession)
  report "HANDOVER" (Text.unpack (snapSession before) <> " -> " <> Text.unpack (snapSession after))
  (_, arguments) <- processImage (sessionPid session)
  assertEqual "the daily process keeps its pid and restores" True ("--restore" `elem` arguments)
  capture session "daily-ime-handover"
  executable <- getExecutablePath
  withSystemTempDirectory "seki-daily-ime" $ \directory -> do
    let probeSocket = directory </> "probe.sock"
        footConfig = directory </> "foot.ini"
    writeFootConfig footConfig
    withUnixListener probeSocket $ \listener ->
      bracket (startManagedProcess "foot" client (proc "foot" ["-c", footConfig, "-a", "seki-integration-probe", executable, "--ime-terminal-probe", probeSocket])) stopManagedProcess $ \foot -> do
        terminal <- waitForProbe listener foot
        flip finally (hClose terminal) $ do
          _ <- waitSnapshot session "foot is managed on term with keyboard focus" (\s -> snapKeyboard s && any (\w -> wsTag w == "term" && wsOccupied w && wsFocused w) (snapWorkspaces s))
          capture session "daily-ime-foot"
          committed <- withFcitxVirtualKeyboardOwner ime $ \owner -> commitMozcText "m3-daily-ime" ime client owner terminal
          report "IME_COMMIT" committed
          assertEqual "foot receives exactly the Mozc commit after the handover" "日本語" committed
          capture session "daily-ime"
  daemons <- fixtureProcesses "fcitx5" ("DBUS_SESSION_BUS_ADDRESS", bus)
  report "FCITX5_PROCESSES" (show daemons)
  assertEqual "one fcitx5 process from this fixture exists" 1 (length daemons)

xtermTitle :: Text
xtermTitle = "seki-daily-xterm"

{- | An xterm under the daily configuration tiles beside a native window,
loses and regains focus by binding and by click, moves to another
workspace and back, and closes with Mod-Shift-c.
-}
xwaylandManagement :: IO ()
xwaylandManagement = withDailySession "m3-daily-xwayland" (dailyOptions 1){optionXwayland = True} $ \session -> do
  let probe = sessionProbe session
      client = clientEnvironment session
      modShift k = runInputDriver client ["-M", "logo", "-M", "shift", "-k", k, "-m", "shift", "-m", "logo"]
      -- xdotool fails while X focus is still on the root window, before any X client maps.
      xFocus phase = void (waitUntil phase assertionTimeoutMicroseconds (either (const Nothing) (Just . Text.pack) <$> try @SomeException (queryX11FocusedWindowName client)) (== Just xtermTitle))
      xGeometry = do
        found <- runProcessCapture client "xdotool" ["search", "--name", Text.unpack xtermTitle, "getwindowgeometry", "--shell"]
        pure (sort [line | line <- lines (BS8.unpack found), any (`isPrefixOf` line) ["X=", "Y=", "WIDTH=", "HEIGHT="]])
  initial <- waitSnapshot session "term is shown" (any ((== Just "term") . outputWorkspace) . snapOutputs)
  output <- shownOn initial "term"
  let onTerm title others = bar [(output, "term")] (workspace "term" [Occupied, Visible, Focused] "Tall" : others) title []
  native <- openWindow probe Green
  waitFocus session "the native window has keyboard focus" (Just native)
  bracket (startManagedProcess "xterm" client (proc "xterm" ["-title", Text.unpack xtermTitle, "-fa", "monospace", "-fs", "12", "-e", "cat"])) stopManagedProcess $ \xterm -> do
    xFocus "the new xterm has X input focus"
    waitFocus session "the native window loses keyboard focus to the xterm" Nothing
    picture session output "the xterm tiles as master beside the native window" [Paint Green (Box 401 1 398 598) False]
    geometry <- waitUntil "the xterm's X window fills the master tile" assertionTimeoutMicroseconds xGeometry (== ["HEIGHT=598", "WIDTH=398", "X=1", "Y=1"])
    report "XTERM_GEOMETRY" (show geometry)
    snapshotShows session "the bar shows the focused xterm's title" (onTerm (Just xtermTitle) [])
    capture session "daily-xwayland-tiled"
    key session ["j"]
    waitFocus session "Mod-j moves keyboard focus to the native window" (Just native)
    picture session output "the native window takes the focused border" [Paint Green (Box 401 1 398 598) True]
    click (sessionSocket session) (800, 600) (200, 300)
    xFocus "a click gives the xterm X input focus"
    waitFocus session "the click takes keyboard focus from the native window" Nothing
    picture session output "the clicked xterm takes the focused border" [Paint Green (Box 401 1 398 598) False]
    modShift "2"
    waitFocus session "Mod-Shift-2 leaves the native window focused on term" (Just native)
    picture session output "term shows only the native window" [Paint Green (Box 1 1 798 598) True]
    snapshotShows session "the xterm is on emacs" (onTerm Nothing [workspace "emacs" [Occupied] "Tall"])
    key session ["2"]
    xFocus "viewing emacs gives the xterm X input focus"
    picture session output "emacs shows only the xterm" []
    capture session "daily-xwayland-moved"
    modShift "1"
    key session ["1"]
    xFocus "the xterm moved back to term has X input focus"
    picture session output "the xterm tiles beside the native window again" [Paint Green (Box 401 1 398 598) False]
    snapshotShows session "the xterm is back on term" (onTerm (Just xtermTitle) [])
    capture session "daily-xwayland"
    modShift "c"
    exit <- waitForProcessWithTimeout "Mod-Shift-c closes the xterm" xterm
    report "XTERM_EXIT" (show exit)
    waitFocus session "the native window has keyboard focus after the close" (Just native)
    picture session output "term shows only the native window after the close" [Paint Green (Box 1 1 798 598) True]
    snapshotShows session "the bar shows only the native window" (onTerm Nothing [])
    capture session "daily-xwayland-closed"

{- | Workers that each keep one IPC connection and alternate @snapshot@ and
@workspace term@ requests, reading every reply, while one subscriber never
reads. The action gets the count of replies so far; every worker must end
without an error once it returns.
-}
withFlood :: DailySession -> Int -> (IO Int -> IO a) -> IO a
withFlood session workers action = do
  let path = riverRuntimeDirectory (sessionRiver session) </> "seki" </> "ipc.sock"
      open = do
        client <- socket AF_UNIX Stream defaultProtocol
        connect client (SockAddrUnix path) `onException` close client
        pure client
  stop <- newIORef False
  replies <- newIORef (0 :: Int)
  let request client pending bytes expected = do
        sendAll client bytes
        (reply, rest) <- replyLine client pending
        unless (expected `BS.isInfixOf` reply) (failIntegration ("flood reply lacks " <> show expected <> ": " <> show (BS.take 200 reply)))
        atomicModifyIORef' replies (\count -> (count + 1, ()))
        pure rest
      work client pending =
        readIORef stop >>= \stopped -> unless stopped $ do
          afterSnapshot <- request client pending "{\"version\":1,\"id\":\"flood\",\"op\":\"snapshot\"}\n" "\"snapshot\""
          request client afterSnapshot "{\"version\":1,\"id\":\"flood\",\"op\":\"workspace\",\"tag\":\"term\"}\n" "\"revision\"" >>= work client
  bracket open close $ \unread -> do
    sendAll unread "{\"version\":1,\"id\":\"unread\",\"op\":\"subscribe\"}\n"
    finished <- forM [1 .. workers] $ \_ -> do
      done <- newEmptyMVar
      _ <- forkFinally (bracket open close (`work` BS.empty)) (putMVar done)
      pure done
    result <- action (readIORef replies) `finally` writeIORef stop True
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

{- | The time from starting a Mod-key press until the probe's keyboard focus
first differs from @from@, polled every millisecond. It includes wtype's
own startup and Wayland connection.
-}
keyToFocus :: DailySession -> String -> Maybe ProbeWindow -> IO (Integer, Maybe ProbeWindow)
keyToFocus session keyName from = do
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

{- | While eight IPC workers flood snapshot and workspace requests and one
subscriber never reads, a logo drag floats and moves a tile and a logo
button-3 drag resizes it to the exact geometry, captures keep changing, a
key binding still moves keyboard focus, and a fresh sekictl request gets
its receipt.
-}
loadResponsive :: IO ()
loadResponsive = withDailySession "m3-daily-load" (dailyOptions 1) $ \session -> do
  let probe = sessionProbe session
      surface = sessionSocket session
  initial <- waitSnapshot session "term is shown" (any ((== Just "term") . outputWorkspace) . snapOutputs)
  output <- shownOn initial "term"
  dragged <- openWindow probe Blue
  stacked <- openWindow probe Green
  master <- openWindow probe Red
  waitFocus session "the newest window has keyboard focus as master" (Just master)
  picture session output "three windows tile with the newest as master" [Paint Green (Box 401 1 398 298) False, Paint Blue (Box 401 301 398 298) False]
  capture session "daily-load-tiled"
  (idle, idleOwner) <- keyToFocus session "j" (Just master)
  assertEqual "Mod-j without the flood moves focus down the stack" (Just stacked) idleOwner
  report "KEY_TO_FOCUS_IDLE_NS" (show idle)
  key session ["k"]
  waitFocus session "Mod-k returns focus to the master" (Just master)
  started <- getMonotonicTimeNSec
  (count, loaded, frames) <- withFlood session 8 $ \replies -> do
    _ <- waitUntil "the flood is answering requests" assertionTimeoutMicroseconds replies (>= 100)
    first <- captureOutput surface output
    (moved, resized) <- withHeldLogoIn (clientEnvironment session) probe . withVirtualPointer surface (800, 600) $ \pointer -> do
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
    (latency, owner) <- keyToFocus session "j" (Just dragged)
    assertEqual "Mod-j during the flood moves focus from the float to the master" (Just master) owner
    receipt <- sekictl session ["workspace", "term"]
    revision <- requireRight "a fresh sekictl receipt during the flood" (receiptRevision receipt)
    report "FRESH_RECEIPT_REVISION" (show revision)
    count <- replies
    pure (count, latency, map imageRgb [first, moved, resized])
  ended <- getMonotonicTimeNSec
  assertEqual "captures keep changing while the subscriber never reads" (length frames) (length (nub frames))
  report "KEY_TO_FOCUS_FLOOD_NS" (show loaded)
  report "FLOOD" (show count <> " replies in " <> show ((ended - started) `div` 1000000) <> " ms")
  picture session output "the float keeps its moved and resized geometry after the flood" [Paint Green (Box 401 1 398 598) False, Paint Blue (Box 201 151 458 338) False]
  capture session "daily-load"
