{-# LANGUAGE OverloadedStrings #-}

{- | The first start on real river (M4-02). Each scenario runs the installed
launcher against a stub delivery whose build step waits, fails, or resolves a
prebuilt image, so a scenario decides when a build ends and what it yields.
-}
module Seki.Integration.M4 (m4Scenarios, crashingImage) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Exception (bracket, onException)
import Control.Monad (forM_, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, nub, sort, stripPrefix)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Fixture.Probe
import Seki.Integration.Fixture (BorderStyle (..), Box (..), NestedRiver (..), Paint (..), outputPicture, pictureErrorWith, pixel, placeOutputs, portedBorders, rgb, saveOutputsSideBySide, withKeyboard, withNestedRiver)
import Seki.Integration.Harness
import Seki.Internal.Config (Config (..), def)
import Seki.Run.Config (runConfig)
import Seki.Run.ConfigBuild (startupArguments)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, doesPathExist, listDirectory, removeFile)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, readSymbolicLink)
import System.Process (proc)
import System.Timeout (timeout)

m4Scenarios :: [(String, IO ())]
m4Scenarios =
  [ ("m4-first-start", firstStart)
  , ("m4-first-start-failed", firstStartFailed)
  , ("m4-current-profile", currentProfile)
  , ("m4-handover-windows", handoverWindows)
  , ("m4-handover-spawnonce", handoverSpawnOnce)
  , ("m4-upgrade-start", upgradeStart)
  , ("m4-output-lost", outputLost)
  , ("m4-recompile-coalesce", recompileCoalesce)
  , ("m4-check-sync", checkSync)
  , ("m4-crash-recovery", crashRecovery)
  , ("m4-crash-no-loop", crashNoLoop)
  ]

{- | Run this executable as a user image whose startup hook throws. The
crash scenarios build it as the configuration, because no shipped
configuration dies at start.
-}
crashingImage :: [String] -> IO ()
crashingImage arguments =
  runConfig arguments def{startupHook = error "this startup hook throws"} >>= either (\e -> hPutStrLn stderr ("seki: " <> show e) >> exitFailure) pure

data Home = Home
  { homeRiver :: NestedRiver
  , homeDirectory :: FilePath
  , homeLauncher :: FilePath
  , homeCli :: FilePath
  , homeDefault :: FilePath
  }

withHome :: (Home -> IO a) -> IO a
withHome use = do
  launcher <- requireSetting "SEKI_LAUNCHER_EXECUTABLE"
  cli <- requireSetting "SEKICTL_EXECUTABLE"
  defaultRuntime <- requireSetting "SEKI_EXECUTABLE"
  withNestedRiver 1 $ \nested -> withSystemTempDirectory "seki-m4-home" $ \directory -> do
    forM_ ["bin", "build", "images"] (createDirectoryIfMissing True . (directory </>))
    use (Home nested directory launcher cli defaultRuntime)

stateOf :: Home -> FilePath
stateOf home = homeDirectory home </> ".local" </> "state" </> "seki"

control :: Home -> String -> FilePath
control home name = homeDirectory home </> "build" </> name

clientEnvironment :: Home -> [(String, String)]
clientEnvironment = riverEnvironment . homeRiver

socketOf :: Home -> FilePath
socketOf = riverSocket . homeRiver

{- | A stub delivery. Its build step records itself, waits while @hold@
exists, and fails while @fail@ exists. Its list-bin step prints the path in
@built@. A second build step that starts while one runs is recorded in
@overlaps@.
-}
delivery :: Home -> String -> IO [(String, String)]
delivery home name = do
  let resources = homeDirectory home </> name
      templates = resources </> "templates"
      file = control home
  createDirectoryIfMissing True templates
  forM_ ["Main.hs", "seki-user.cabal", "cabal.project"] $ \template -> writeFile (templates </> template) ("-- " <> template <> " for @ARCHIVE@ built by @COMPILER@\n")
  forM_ ["archive", "ghc", "cabal"] $ \resource -> writeFile (resources </> resource) ""
  writeExecutable (resources </> "env") $
    unlines
      [ "#!/bin/sh"
      , "printf '%s\\n' \"$3\" >> '" <> stepsFile home name <> "'"
      , "case \"$3\" in"
      , "  build)"
      , "    [ -e '" <> file "busy" <> "' ] && echo overlap >> '" <> file "overlaps" <> "'"
      , "    : > '" <> file "busy" <> "'"
      , "    while [ -e '" <> file "hold" <> "' ]; do sleep 0.05; done"
      , "    rm -f '" <> file "busy" <> "'"
      , "    if [ -e '" <> file "fail" <> "' ]; then echo 'Main.hs:1:1: error: deliberate compile error'; exit 1; fi"
      , "    exit 0 ;;"
      , "  list-bin) cat '" <> file "built" <> "' ;;"
      , "  *) exit 64 ;;"
      , "esac"
      ]
  pure [("SEKI_CONFIG_" <> variable, resources </> resource) | (variable, resource) <- [("ARCHIVE", "archive"), ("GHC", "ghc"), ("CABAL", "cabal"), ("ENV", "env"), ("TEMPLATES", "templates")]]

stepsFile :: Home -> String -> FilePath
stepsFile home name = control home (name <> "-steps")

steps :: Home -> String -> IO [String]
steps home name = recordedLines (stepsFile home name)

recordedLines :: FilePath -> IO [String]
recordedLines path = doesFileExist path >>= \exists -> if exists then lines . BS8.unpack <$> BS.readFile path else pure []

hold :: Home -> IO ()
hold home = writeFile (control home "hold") ""

release :: Home -> IO ()
release home = removeFile (control home "hold")

-- | What the next build yields.
builds :: Home -> FilePath -> IO ()
builds home image = writeFile (control home "built") (image <> "\n")

-- | The executable with other bytes: a configuration that behaves the same and is not the same image.
variant :: Home -> String -> FilePath -> IO FilePath
variant home name source = do
  let path = homeDirectory home </> "images" </> name
  copyFile source path
  BS.appendFile path (BS8.pack ("\nseki m4 image " <> name <> "\n"))
  pure path

-- | The daily configuration with other bytes, with the keymap and the logging stubs it needs.
dailyVariant :: Home -> String -> IO FilePath
dailyVariant home name = do
  executable <- requireSetting "SEKI_DAILY_CONFIG_EXECUTABLE"
  backend <- requireSetting "SEKI_INPUT_BACKEND"
  let keymap = homeDirectory home </> ".config" </> "seki" </> "keymap.xkb"
  createDirectoryIfMissing True (takeDirectory keymap)
  copyFile (takeDirectory backend </> "daily.xkb") keymap
  forM_ ["waybar", "swaybg", "fcitx5", "swayidle", "swaylock", "alacritty", "amixer", "brightnessctl", "wmenu-run"] $ \program ->
    writeExecutable (homeDirectory home </> "bin" </> program) (loggingStub (launchLog home))
  variant home name executable

launchLog :: Home -> FilePath
launchLog home = homeDirectory home </> "launches"

startupCommands :: [String]
startupCommands = ["waybar", "swaybg -i /usr/share/backgrounds/archlinux/wave.png -m fill", "fcitx5", "swayidle -w timeout 600 swaylock -f"]

-- | How often each startup command has run.
startupRuns :: Home -> IO [Int]
startupRuns home = (\logged -> [length (filter (== command) logged) | command <- startupCommands]) <$> recordedLines (launchLog home)

sekiEnvironment :: Home -> [(String, String)] -> [(String, String)]
sekiEnvironment home resources = overrides <> filter ((`notElem` ("XDG_CONFIG_HOME" : map fst overrides)) . fst) base
 where
  base = clientEnvironment home
  directory = homeDirectory home
  overrides =
    [ ("HOME", directory)
    , ("PATH", directory </> "bin" <> ":" <> fromMaybe "/usr/bin:/bin" (lookup "PATH" base))
    , ("XDG_STATE_HOME", directory </> ".local" </> "state")
    , ("XDG_DATA_HOME", directory </> ".local" </> "share")
    , ("XDG_CACHE_HOME", directory </> ".cache")
    , ("SEKI_CONFIG_DEFAULT", homeDefault home)
    ]
      <> resources

-- | Start @seki@ once, as river's init script does. The body gets its pid and its output so far.
withSeki :: String -> Home -> [(String, String)] -> (Int -> IO String -> IO a) -> IO a
withSeki name home resources body =
  bracket (startManagedProcess name (sekiEnvironment home resources) (proc (homeLauncher home) [])) stopManagedProcess $ \process ->
    (managedProcessPid process >>= maybe (failIntegration (name <> ": seki has no pid")) (\pid -> body pid (renderManagedProcessLogs process))) `onException` (renderManagedProcessLogs process >>= announce)

{- | Start @seki@ in the loop of river's init script. Each exit of @seki@ is
recorded in @exits@. The body gets the loop's output so far.
-}
withInitLoop :: String -> Home -> [(String, String)] -> (IO String -> IO a) -> IO a
withInitLoop name home resources body =
  bracket (startManagedProcess name (sekiEnvironment home resources) (proc "/bin/sh" ["-c", "while :; do " <> shellQuote (homeLauncher home) <> "; echo \"exit $?\" >> " <> shellQuote (control home "exits") <> "; sleep 1; done"])) stopManagedProcess $ \process ->
    body (renderManagedProcessLogs process) `onException` (renderManagedProcessLogs process >>= announce)

exits :: Home -> IO [String]
exits home = recordedLines (control home "exits")

-- | Run @seki --check@ and return the candidate it validated.
checkedCandidate :: Home -> [(String, String)] -> IO FilePath
checkedCandidate home resources = do
  (code, _, diagnostics) <- runProcessOutcome (sekiEnvironment home resources) (homeLauncher home) ["--check"]
  assertEqual ("seki --check succeeds: " <> BS8.unpack diagnostics) ExitSuccess code
  case mapMaybe (stripPrefix "seki: candidate validated: ") (lines (BS8.unpack diagnostics)) of
    [candidate] -> pure candidate
    other -> failIntegration ("seki --check names one candidate, got " <> show other)

installProfile :: Home -> FilePath -> IO ()
installProfile home candidate = createSymbolicLink candidate (stateOf home </> "profile")

-- | The output, placed at 800 by 600 before any window manager runs.
placedOutput :: Home -> IO Text
placedOutput home = do
  found <- waitUntil "river shows its output" assertionTimeoutMicroseconds (outputNames (socketOf home)) ((== 1) . length)
  case found of
    [(_, output)] -> placeOutputs (clientEnvironment home) [output] >> pure output
    other -> failIntegration ("expected one output, got " <> show other)

shot :: Home -> IO (Either String Snapshot)
shot home = do
  (code, reply, failure) <- runProcessOutcome (clientEnvironment home) (homeCli home) ["snapshot"]
  pure $ case code of
    ExitSuccess -> replySnapshot reply
    _ -> Left ("sekictl snapshot ended with " <> show code <> ": " <> BS8.unpack failure)

waitShot :: Home -> String -> (Snapshot -> Bool) -> IO Snapshot
waitShot home phase ready = waitUntil phase assertionTimeoutMicroseconds (shot home) (either (const False) ready) >>= either failIntegration pure

sekictl :: Home -> [String] -> IO ()
sekictl home = void . runProcessCapture (clientEnvironment home) (homeCli home)

buildIs :: Text -> Snapshot -> Bool
buildIs state = maybe False ((== state) . buildState) . snapBuild

-- | The process that owns the IPC session.
sessionPid :: Snapshot -> Int
sessionPid = read . takeWhile isDigit . Text.unpack . snapSession

occupied :: Snapshot -> [Text]
occupied snapshot = [wsTag workspace | workspace <- snapWorkspaces snapshot, wsOccupied workspace]

processImage :: Int -> IO (FilePath, [String])
processImage pid = (,) <$> readSymbolicLink ("/proc/" <> show pid <> "/exe") <*> (drop 1 <$> processArguments pid)

sameContent :: FilePath -> FilePath -> IO Bool
sameContent first second = (==) <$> BS.readFile first <*> BS.readFile second

candidatesOf :: Home -> IO [FilePath]
candidatesOf home = sort . filter (\name -> "candidate" `isPrefixOf` name && not (".resources" `isInfixOf` name)) <$> listDirectory (stateOf home)

onTrial :: Home -> IO Bool
onTrial home = doesFileExist (stateOf home </> "unproven")

-- | The running image is a checked candidate with these bytes, restored by a handover, and the profile names it.
builtProfile :: Home -> String -> Int -> FilePath -> IO FilePath
builtProfile home phase pid expected = do
  (image, arguments) <- processImage pid
  report "IMAGE" (phase <> ": " <> show (image, arguments))
  assertEqual (phase <> ": the image is a candidate in the state directory") (stateOf home, True) (takeDirectory image, "candidate" `isPrefixOf` takeFileName image)
  assertEqual (phase <> ": the image was restored by a handover") ["--restore"] (take 1 arguments)
  sameContent image expected >>= assertEqual (phase <> ": the image has the bytes of the built configuration") True
  readSymbolicLink (stateOf home </> "profile") >>= assertEqual (phase <> ": the profile names the running image") image
  pure image

-- | The launcher has exec'd this executable with the startup arguments.
startedImage :: String -> Int -> FilePath -> IO ()
startedImage phase pid expected = do
  running <- waitUntil phase assertionTimeoutMicroseconds (processImage pid) (== (expected, startupArguments))
  report "IMAGE" (phase <> ": " <> show running)

defaultBorders :: BorderStyle
defaultBorders = BorderStyle 2 (BS.pack [0, 127, 255]) (BS.pack [63, 63, 63])

tiled :: String -> Home -> Text -> BorderStyle -> String -> [Paint] -> IO ()
tiled scenario home output style phase = outputPicture scenario (socketOf home) output phase . pictureErrorWith style

-- | The probe colors an output shows anywhere.
shownColors :: Home -> Text -> IO [Color]
shownColors home output = do
  frame <- captureOutput (socketOf home) output
  pure [color | color <- [Red, Green, Blue], any (\(x, y) -> pixel frame x y == rgb color) [(x, y) | y <- [0, 4 .. 599], x <- [0, 4 .. 799]]]

report :: String -> String -> IO ()
report label value = announce ("M4_" <> label <> " " <> value)

capture :: Home -> String -> IO ()
capture home label = do
  artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
  forM_ artifacts $ \directory -> do
    createDirectoryIfMissing True directory
    saveOutputsSideBySide (socketOf home) (directory </> label <> ".png")
  report "CAPTURE" label

millisecondsSince :: Integer -> IO Integer
millisecondsSince start = (\now -> (toInteger now - start) `div` 1000000) <$> getMonotonicTimeNSec

leftTile, rightTile, fullTile :: Box
leftTile = Box 2 2 396 596
rightTile = Box 402 2 396 596
fullTile = Box 2 2 796 596

portedTile :: Box
portedTile = Box 1 1 798 598

{- | Lane 1. A fresh state directory and a build that is still running: the
default runtime tiles two windows at once, and the built configuration takes
over when the build ends.
-}
firstStart :: IO ()
firstStart = withHome $ \home -> do
  resources <- delivery home "delivery"
  user <- variant home "user" (homeDefault home)
  builds home user
  hold home
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> do
    start <- toInteger <$> getMonotonicTimeNSec
    withSeki "m4-first-start" home resources $ \pid _ -> do
      _ <- openWindow probe Blue
      _ <- openWindow probe Green
      tiled "m4-first-start" home output defaultBorders "the default runtime tiles both windows during the build" [Paint Green leftTile True, Paint Blue rightTile False]
      millisecondsSince start >>= report "FIRST_START_MANAGED_MS" . show
      elapsed <- millisecondsSince start
      when (elapsed < 1000) (threadDelay (fromInteger (1000 - elapsed) * 1000))
      capture home "first-start"
      tiled "m4-first-start" home output defaultBorders "both windows are tiled one second after the start" [Paint Green leftTile True, Paint Blue rightTile False]
      startedImage "the launcher started the default runtime" pid (homeDefault home)
      building <- waitShot home "the snapshot reports the startup build" (buildIs "building")
      assertEqual "the startup build is generation 1" (Just 1) (buildGeneration <$> snapBuild building)
      steps home "delivery" >>= assertEqual "the build began after the session was live" ["build"]
      release home
      _ <- waitShot home "the built configuration starts a new IPC session" ((/= snapSession building) . snapSession)
      _ <- builtProfile home "after the build" pid user
      steps home "delivery" >>= assertEqual "one build and one list-bin ran" ["build", "list-bin"]
      tiled "m4-first-start" home output defaultBorders "the built configuration keeps both windows tiled" [Paint Green leftTile True, Paint Blue rightTile False]
      _ <- waitUntil "the built configuration proves healthy" assertionTimeoutMicroseconds (onTrial home) not
      capture home "first-start-built"

{- | Lane 2. A configuration that does not compile: the default runtime keeps
managing, and the snapshot reports the failed build.
-}
firstStartFailed :: IO ()
firstStartFailed = withHome $ \home -> do
  resources <- delivery home "delivery"
  variant home "user" (homeDefault home) >>= builds home
  writeFile (control home "fail") ""
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withSeki "m4-first-start-failed" home resources $ \pid _ -> do
    _ <- openWindow probe Blue
    failed <- waitShot home "the snapshot reports the failed build" (buildIs "failed")
    report "FAILED_BUILD" (show (snapBuild failed))
    assertEqual "the failure carries the compiler's message" (Just True) (Text.isInfixOf "deliberate compile error" <$> (snapBuild failed >>= buildError))
    startedImage "the default runtime keeps running" pid (homeDefault home)
    doesPathExist (stateOf home </> "profile") >>= assertEqual "a failed build selects no profile" False
    _ <- openWindow probe Green
    tiled "m4-first-start-failed" home output defaultBorders "the default runtime still tiles a new window" [Paint Green leftTile True, Paint Blue rightTile False]
    assertEqual "the session did not restart" (snapSession failed) . snapSession =<< waitShot home "the session answers" (const True)
    capture home "first-start-failed"

{- | Lane 3. A current profile, started twice, the second time after a touch
on a source file: the profile execs directly, its build ends unchanged in the
same session, and its startup hook runs once per start.
-}
currentProfile :: IO ()
currentProfile = withHome $ \home -> do
  resources <- delivery home "delivery"
  dailyVariant home "daily" >>= builds home
  candidate <- checkedCandidate home resources
  installProfile home candidate
  output <- placedOutput home
  forM_ [("the first start", False), ("the start after a touched source", True)] $ \(phase, touched) -> do
    when touched (void (runProcessCapture (clientEnvironment home) "touch" [homeDirectory home </> ".config" </> "seki" </> "Main.hs"]))
    before <- startupRuns home
    withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> do
      start <- toInteger <$> getMonotonicTimeNSec
      withSeki "m4-current-profile" home resources $ \pid _ -> do
        _ <- openWindow probe Blue
        tiled "m4-current-profile" home output portedBorders (phase <> ": the profile tiles a window") [Paint Blue portedTile True]
        millisecondsSince start >>= report "CURRENT_PROFILE_MANAGED_MS" . ((phase <> ": ") <>) . show
        startedImage (phase <> ": the launcher execs the profile's candidate") pid candidate
        first <- waitShot home (phase <> ": the session answers") (const True)
        settled <- waitShot home (phase <> ": the startup build ends unchanged") (buildIs "unchanged")
        report "UNCHANGED_BUILD" (phase <> ": " <> show (snapBuild settled))
        assertEqual (phase <> ": the same IPC session and pid") (snapSession first, pid) (snapSession settled, sessionPid settled)
        _ <- waitUntil (phase <> ": each startup command ran once in this start") assertionTimeoutMicroseconds (startupRuns home) (== map (+ 1) before)
        _ <- waitUntil (phase <> ": the profile proves healthy") assertionTimeoutMicroseconds (onTrial home) not
        candidatesOf home >>= assertEqual (phase <> ": the reproduced copy is deleted") [takeFileName candidate]
        readSymbolicLink (stateOf home </> "profile") >>= assertEqual (phase <> ": the profile is untouched") candidate
        startupRuns home >>= assertEqual (phase <> ": no startup command ran again") (map (+ 1) before)
        capture home (if touched then "current-profile-touched" else "current-profile")

{- | Lane 4. Three windows opened during the first build. A configuration
that keeps tags 1 to 9 keeps every window on its workspace. A configuration
with other tags places every window through its manage hook.
-}
handoverWindows :: IO ()
handoverWindows = keptTags >> otherTags
 where
  opened home probe = do
    _ <- openWindow probe Red
    sekictl home ["workspace", "2"]
    _ <- openWindowWith probe (windowSpec Green){specAppId = CustomAppId "Emacs"}
    sekictl home ["workspace", "3"]
    _ <- openWindow probe Blue
    sekictl home ["workspace", "1"]
    waitShot home "the default runtime holds one window on each of 1, 2 and 3" ((== ["1", "2", "3"]) . occupied)
  keptTags = withHome $ \home -> do
    resources <- delivery home "delivery"
    user <- variant home "user" (homeDefault home)
    builds home user
    hold home
    output <- placedOutput home
    withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withSeki "m4-handover-windows" home resources $ \pid _ -> do
      before <- opened home probe
      release home
      _ <- waitShot home "the built configuration starts a new IPC session" ((/= snapSession before) . snapSession)
      _ <- builtProfile home "tags 1 to 9" pid user
      _ <- waitShot home "every window keeps its workspace" ((== ["1", "2", "3"]) . occupied)
      forM_ [("1", Red), ("2", Green), ("3", Blue)] $ \(tag, color) -> do
        sekictl home ["workspace", tag]
        tiled "m4-handover-windows" home output defaultBorders ("workspace " <> tag <> " shows its own window after the handover") [Paint color fullTile True]
      sekictl home ["workspace", "1"]
      capture home "handover-windows"
  otherTags = withHome $ \home -> do
    resources <- delivery home "delivery"
    daily <- dailyVariant home "daily"
    builds home daily
    hold home
    output <- placedOutput home
    withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withSeki "m4-handover-windows" home resources $ \pid _ -> do
      before <- opened home probe
      release home
      _ <- waitShot home "the configuration with other tags starts a new IPC session" ((/= snapSession before) . snapSession)
      _ <- builtProfile home "other tags" pid daily
      placed <- waitShot home "the manage hook places every window" ((== 2) . length . occupied)
      let shown = mapMaybe outputWorkspace (snapOutputs placed)
      report "OTHER_TAGS" ("occupied " <> show (occupied placed) <> ", shown " <> show shown)
      assertEqual "the Emacs window is on emacs, and the default rules put the others on the shown workspace" (sort ("emacs" : shown)) (sort (occupied placed))
      _ <- waitUntil "the shown workspace holds the two windows the default rules placed" assertionTimeoutMicroseconds (shownColors home output) (== [Red, Blue])
      capture home "handover-windows-other-tags"
      sekictl home ["workspace", "emacs"]
      tiled "m4-handover-windows" home output portedBorders "emacs shows the Emacs window the manage hook shifted" [Paint Green portedTile True]

{- | Lane 5. A current profile and a configuration edited while logged out,
both with the same spawnOnce programs: the session hands over to the new
build, and each program starts exactly once.
-}
handoverSpawnOnce :: IO ()
handoverSpawnOnce = withHome $ \home -> do
  resources <- delivery home "delivery"
  dailyVariant home "daily-before" >>= builds home
  candidate <- checkedCandidate home resources
  installProfile home candidate
  edited <- dailyVariant home "daily-edited"
  builds home edited
  hold home
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withSeki "m4-handover-spawnonce" home resources $ \pid _ -> do
    startedImage "the launcher execs the current profile" pid candidate
    _ <- waitUntil "the current profile's hook starts each program once" assertionTimeoutMicroseconds (startupRuns home) (== map (const 1) startupCommands)
    before <- waitShot home "the snapshot reports the startup build" (buildIs "building")
    release home
    _ <- waitShot home "the edited configuration starts a new IPC session" ((/= snapSession before) . snapSession)
    _ <- builtProfile home "after the handover" pid edited
    _ <- openWindow probe Blue
    tiled "m4-handover-spawnonce" home output portedBorders "the edited configuration tiles a window" [Paint Blue portedTile True]
    _ <- waitUntil "the edited configuration ran its hook and proved healthy" assertionTimeoutMicroseconds (onTrial home) not
    startupRuns home >>= assertEqual "each spawnOnce program started exactly once across the handover" (map (const 1) startupCommands)
    capture home "handover-spawnonce"

{- | Lane 6. A profile built from an earlier delivery: the default runtime
manages during the rebuild, and the session ends on a profile built from the
installed delivery.
-}
upgradeStart :: IO ()
upgradeStart = withHome $ \home -> do
  earlier <- delivery home "earlier-delivery"
  installed <- delivery home "installed-delivery"
  installedArchive <- maybe (failIntegration "the installed delivery names no archive") pure (lookup "SEKI_CONFIG_ARCHIVE" installed)
  variant home "before-upgrade" (homeDefault home) >>= builds home
  checkedCandidate home earlier >>= installProfile home
  rebuilt <- variant home "after-upgrade" (homeDefault home)
  builds home rebuilt
  hold home
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withSeki "m4-upgrade-start" home installed $ \pid diagnostics -> do
    _ <- openWindow probe Blue
    tiled "m4-upgrade-start" home output defaultBorders "the default runtime tiles a window during the rebuild" [Paint Blue fullTile True]
    startedImage "a profile from another delivery starts the default runtime" pid (homeDefault home)
    before <- waitShot home "the snapshot reports the rebuild" (buildIs "building")
    diagnostics >>= assertEqual "the launcher says why it left the profile" True . isInfixOf "not from the installed delivery"
    capture home "upgrade-start"
    release home
    _ <- waitShot home "the rebuilt configuration starts a new IPC session" ((/= snapSession before) . snapSession)
    image <- builtProfile home "after the rebuild" pid rebuilt
    readFile (image <> ".resources") >>= assertEqual "the new profile records the installed delivery" True . isInfixOf installedArchive
    steps home "installed-delivery" >>= assertEqual "the rebuild ran the installed delivery's tools" ["build", "list-bin"]
    tiled "m4-upgrade-start" home output defaultBorders "the rebuilt configuration keeps the window tiled" [Paint Blue fullTile True]
    capture home "upgrade-start-rebuilt"

{- | Lane 7. The only output is removed during the first build and returns
after the exec: the session hands over while headless, and the built profile
manages the returned output and runs its startup hook then.
-}
outputLost :: IO ()
outputLost = withHome $ \home -> do
  resources <- delivery home "delivery"
  daily <- dailyVariant home "daily"
  builds home daily
  hold home
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withSeki "m4-output-lost" home resources $ \pid _ -> do
    _ <- openWindow probe Blue
    tiled "m4-output-lost" home output defaultBorders "the default runtime tiles a window during the build" [Paint Blue fullTile True]
    void (runProcessCapture (clientEnvironment home) "wlr-randr" ["--output", Text.unpack output, "--off"])
    headless <- waitShot home "the default runtime has no output" (null . snapOutputs)
    release home
    restored <- waitShot home "the session hands over while no output exists" ((/= snapSession headless) . snapSession)
    assertEqual "the built profile starts headless" [] (snapOutputs restored)
    _ <- builtProfile home "while headless" pid daily
    startupRuns home >>= assertEqual "the startup hook waits for an output" (map (const 0) startupCommands)
    void (runProcessCapture (clientEnvironment home) "wlr-randr" ["--output", Text.unpack output, "--on", "--custom-mode", "800x600"])
    placeOutputs (clientEnvironment home) [output]
    _ <- waitShot home "the built profile manages the returned output" (\s -> length (snapOutputs s) == 1 && all (isJust . outputWorkspace) (snapOutputs s))
    _ <- waitUntil "the startup hook runs once the output is back" assertionTimeoutMicroseconds (startupRuns home) (== map (const 1) startupCommands)
    tiled "m4-output-lost" home output portedBorders "the built profile tiles the window on the returned output" [Paint Blue portedTile True]
    capture home "output-lost"

{- | Lane 8. A recompile request during the first build: the first result is
discarded, one more build runs after it, and the session execs once, into
the second build.
-}
recompileCoalesce :: IO ()
recompileCoalesce = withHome $ \home -> do
  resources <- delivery home "delivery"
  user <- variant home "user" (homeDefault home)
  builds home user
  hold home
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withSeki "m4-recompile-coalesce" home resources $ \pid _ -> do
    _ <- openWindow probe Blue
    _ <- waitUntil "the startup build runs" assertionTimeoutMicroseconds (steps home "delivery") (== ["build"])
    before <- waitShot home "the snapshot reports the startup build" (buildIs "building")
    sekictl home ["recompile"]
    held <- waitShot home "the request does not start a second compiler" (buildIs "building")
    assertEqual "the first build still runs alone" (Just 1, ["build"]) . (,) (buildGeneration <$> snapBuild held) =<< steps home "delivery"
    sessions <- newEmptyMVar
    stop <- newEmptyMVar
    let watch seen = tryReadMVar stop >>= maybe (shot home >>= \s -> threadDelay 20000 >> watch (either (const seen) ((: seen) . snapSession) s)) (const (putMVar sessions (nub (reverse seen))))
    _ <- forkIO (watch [])
    release home
    _ <- waitShot home "the second build starts a new IPC session" ((/= snapSession before) . snapSession)
    _ <- builtProfile home "after the second build" pid user
    threadDelay 1500000
    putMVar stop ()
    takeMVar sessions >>= assertEqual "the session execs once" 2 . length
    steps home "delivery" >>= assertEqual "two builds ran, one after the other" ["build", "list-bin", "build", "list-bin"]
    doesFileExist (control home "overlaps") >>= assertEqual "two compilers ran at once" False
    length <$> candidatesOf home >>= assertEqual "the discarded first result and the profile" 2
    tiled "m4-recompile-coalesce" home output defaultBorders "the second build keeps the window tiled" [Paint Blue fullTile True]
    capture home "recompile-coalesce"

{- | Lane 9. @seki --check@ with a fresh state directory: it returns only
after the build, prints the validated path, selects no profile, and starts
no window manager.
-}
checkSync :: IO ()
checkSync = withHome $ \home -> do
  resources <- delivery home "delivery"
  variant home "user" (homeDefault home) >>= builds home
  hold home
  finished <- newEmptyMVar
  _ <- forkIO (runProcessOutcome (sekiEnvironment home resources) (homeLauncher home) ["--check"] >>= putMVar finished)
  _ <- waitUntil "the check runs the build" assertionTimeoutMicroseconds (steps home "delivery") (== ["build"])
  threadDelay 300000
  tryReadMVar finished >>= assertEqual "the check waits for its build" True . isNothing
  release home
  (code, _, diagnostics) <- timeout assertionTimeoutMicroseconds (takeMVar finished) >>= maybe (failIntegration "seki --check did not return") pure
  report "CHECK" (show code <> " " <> BS8.unpack diagnostics)
  assertEqual "the check succeeds" ExitSuccess code
  candidate <- case mapMaybe (stripPrefix "seki: candidate validated: ") (lines (BS8.unpack diagnostics)) of
    [path] -> pure path
    other -> failIntegration ("the check prints one validated path, got " <> show other)
  doesFileExist candidate >>= assertEqual "the validated candidate exists" True
  steps home "delivery" >>= assertEqual "one build and one list-bin ran" ["build", "list-bin"]
  doesPathExist (stateOf home </> "profile") >>= assertEqual "the check selects no profile" False
  doesPathExist (riverRuntimeDirectory (homeRiver home) </> "seki" </> "ipc.sock") >>= assertEqual "the check starts no session" False
  onTrial home >>= assertEqual "the check puts no candidate on trial" False
  capture home "check-sync"

{- | Lane 11. A profile whose startup hook throws, and a configuration that
is fixed before river's init loop starts @seki@ again: the second start runs
the default runtime, the fixed configuration takes over, and the broken
profile is not exec'd again.
-}
crashRecovery :: IO ()
crashRecovery = withHome $ \home -> do
  resources <- delivery home "delivery"
  getExecutablePath >>= builds home
  broken <- checkedCandidate home resources
  installProfile home broken
  fixed <- variant home "fixed" (homeDefault home)
  builds home fixed
  hold home
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withInitLoop "m4-crash-recovery" home resources $ \diagnostics -> do
    _ <- openWindow probe Blue
    recovering <- waitShot home "the second start runs the default runtime" (buildIs "building")
    startedImage "the default runtime replaces the profile that died" (sessionPid recovering) (homeDefault home)
    exits home >>= assertEqual "the broken profile died once" ["exit 1"]
    diagnostics >>= assertEqual "the launcher says the profile died before it proved healthy" True . isInfixOf "died before it proved healthy"
    tiled "m4-crash-recovery" home output defaultBorders "the default runtime tiles the window" [Paint Blue fullTile True]
    release home
    recovered <- waitShot home "the fixed configuration takes over" ((/= snapSession recovering) . snapSession)
    _ <- builtProfile home "after the fix" (sessionPid recovered) fixed
    _ <- waitUntil "the fixed configuration proves healthy" assertionTimeoutMicroseconds (onTrial home) not
    tiled "m4-crash-recovery" home output defaultBorders "the fixed configuration keeps the window tiled" [Paint Blue fullTile True]
    threadDelay 2500000
    exits home >>= assertEqual "the broken profile was not exec'd again" ["exit 1"]
    assertEqual "the fixed configuration keeps its session" (snapSession recovered) . snapSession =<< waitShot home "the session answers" (const True)
    capture home "crash-recovery"

{- | A configuration that stays broken. The default runtime does not hand
over to the image that died. A recompile request tries it once more, and the
launcher then starts the default runtime again. The two images never take
turns on their own.
-}
crashNoLoop :: IO ()
crashNoLoop = withHome $ \home -> do
  resources <- delivery home "delivery"
  getExecutablePath >>= builds home
  checkedCandidate home resources >>= installProfile home
  output <- placedOutput home
  withKeyboard (clientEnvironment home) . withProbe (socketOf home) $ \probe -> withInitLoop "m4-crash-no-loop" home resources $ \_ -> do
    _ <- openWindow probe Blue
    refused <- waitShot home "the default runtime refuses the image that died" (buildIs "failed")
    report "REFUSED_BUILD" (show (snapBuild refused))
    assertEqual "the failure names the image that died" (Just True) (Text.isInfixOf "died before it proved healthy" <$> (snapBuild refused >>= buildError))
    startedImage "the default runtime manages" (sessionPid refused) (homeDefault home)
    exits home >>= assertEqual "the broken profile died once" ["exit 1"]
    sekictl home ["recompile"]
    _ <- waitUntil "the requested handover onto the broken configuration dies" assertionTimeoutMicroseconds (exits home) ((== 2) . length)
    again <- waitShot home "the launcher starts the default runtime again, and it refuses again" (\s -> snapSession s /= snapSession refused && buildIs "failed" s)
    startedImage "the default runtime manages again" (sessionPid again) (homeDefault home)
    tiled "m4-crash-no-loop" home output defaultBorders "the default runtime tiles the window" [Paint Blue fullTile True]
    threadDelay 3000000
    exits home >>= assertEqual "nothing was exec'd without a request" ["exit 1", "exit 1"]
    assertEqual "the default runtime keeps its session" (snapSession again) . snapSession =<< waitShot home "the session answers" (const True)
    capture home "crash-no-loop"
