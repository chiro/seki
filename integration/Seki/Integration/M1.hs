{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.M1 (m1Scenarios) where

import Control.Concurrent (forkFinally, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryReadMVar)
import Control.Exception (SomeException, bracket, finally, onException, try)
import Control.Monad (forM, forM_, unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Fixture.LayerSurface qualified as Layer
import Seki.Fixture.Probe
import Seki.Integration.Harness
import Seki.Internal.Core.Action (Action, sendMessage, windows)
import Seki.Internal.Core.Geometry (rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.Layout (ChangeLayout (..), Choose, IncMasterN (..), Mirror (..), Resize (..), (|||))
import Seki.Internal.Core.Scene (BorderColor (..), SceneStyle (..))
import Seki.Internal.Core.StackSet qualified as SS
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..))
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Run.Startup (readWaylandGlobalCatalog)
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Connection.ObjectTable qualified as ObjectTable
import Seki.Wire.Transport (ConnectionTarget (UnixSocketPath))
import Seki.Wire.Types (ObjectId, objectIdValue)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc)

type M1Layout = Choose Tall (Choose (Mirror Tall) Full)
data Fixture = Fixture
  { mSocket :: FilePath
  , mEnvironment :: [(String, String)]
  , mSession :: RiverSession
  , mPolicy :: Policy String M1Layout
  , mOutputs :: [Text]
  , mName :: String
  }
data Key = Down | Up | ShrinkMaster | ExpandMaster | More | Fewer | Swap | Cycle | One | Two | Three | ShiftTwo | SelectTwo deriving (Eq, Enum, Show)
data Box = Box Int Int Int Int deriving (Eq, Show)
data Paint = Paint Color Box Bool deriving (Eq, Show)

m1Scenarios :: [(String, IO ())]
m1Scenarios =
  [ scenario "m1-tall-three" 1 $ \f p -> do
      _ <- three p
      onFirst f "Tall three windows" (baseline Blue)
  , scenario "m1-tall-messages" 1 $ \f p -> do
      _ <- three p
      onFirst f "Tall baseline" (baseline Blue)
      press f ShrinkMaster
      onFirst f "Tall ratio two fifths" [Paint Blue (Box 2 2 316 596) True, Paint Green (Box 322 2 476 296) False, Paint Red (Box 322 302 476 296) False]
      press f ExpandMaster
      onFirst f "Tall ratio restored" (baseline Blue)
      press f More
      onFirst f "two masters" [Paint Blue (Box 2 2 396 296) True, Paint Green (Box 2 302 396 296) False, Paint Red (Box 402 2 396 596) False]
      press f Fewer
      onFirst f "one master restored" (baseline Blue)
  , scenario "m1-focus-borders" 1 $ \f p -> do
      (a, b, c) <- three p
      onFirst f "initial focus" (baseline Blue)
      delivery f p [a, b, c] c
      press f Down
      onFirst f "focus down borders" (baseline Green)
      delivery f p [a, b, c] b
      press f Up
      onFirst f "focus up borders" (baseline Blue)
      delivery f p [a, b, c] c
  , scenario "m1-swap-master" 1 $ \f p -> do
      (a, b, c) <- three p
      onFirst f "before master swap" (baseline Blue)
      press f Down
      onFirst f "selected stack window" (baseline Green)
      press f Swap
      onFirst f "swapped master" [Paint Green (Box 2 2 396 596) True, Paint Blue (Box 402 2 396 296) False, Paint Red (Box 402 302 396 296) False]
      delivery f p [a, b, c] b
  , scenario "m1-layout-cycle" 1 $ \f p -> do
      (a, b, c) <- three p
      onFirst f "Tall before cycle" (baseline Blue)
      press f Cycle
      onFirst f "Mirror Tall" [Paint Blue (Box 2 2 796 296) True, Paint Green (Box 2 302 396 296) False, Paint Red (Box 402 302 396 296) False]
      press f Cycle
      onFirst f "Full hides stack" [full Blue True]
      press f Down
      onFirst f "Full focus changes visible client" [full Green True]
      delivery f p [a, b, c] b
      press f Cycle
      onFirst f "Tall cycle restores geometry" (baseline Green)
  , scenario "m1-workspace-visibility" 1 $ \f p -> do
      (a, b, c) <- three p
      onFirst f "workspace one initial" (baseline Blue)
      press f ShiftTwo
      onFirst f "shift hides blue" [Paint Green (Box 2 2 396 596) True, Paint Red (Box 402 2 396 596) False]
      press f Two
      onFirst f "workspace two shows blue only" [full Blue True]
      delivery f p [a, b, c] c
      press f Three
      onFirst f "empty workspace hides all clients" []
      _ <- waitUntil "empty workspace clears keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot p) ((== Nothing) . keyboardOwner)
      press f One
      onFirst f "workspace one restored" [Paint Green (Box 2 2 396 596) True, Paint Red (Box 402 2 396 596) False]
      delivery f p [a, b, c] b
  , scenario "m1-greedy-view" 2 greedyScenario
  , scenario "m1-layer-exclusive-zone" 1 layerScenario
  , scenario "m1-output-readd" 2 outputScenario
  ]
 where
  scenario name count run = (name, withFixture name count run)

onFirst :: Fixture -> String -> [Paint] -> IO ()
onFirst fixture phase paints = picture fixture phase [(head (mOutputs fixture), paints)]

full :: Color -> Bool -> Paint
full color = Paint color (Box 2 2 796 596)

delivery :: Fixture -> Probe -> [ProbeWindow] -> ProbeWindow -> IO ()
delivery fixture probe clients target = do
  _ <- waitUntil "native keyboard enter" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just target) . keyboardOwner)
  before <- keyboardSnapshot probe
  runInputDriver (mEnvironment fixture) ["q"]
  _ <- waitUntil "only focused native client receives q press" assertionTimeoutMicroseconds (keyboardSnapshot probe) $ \after ->
    keyboardOwner after == Just target && all (\window -> count after window == count before window + if window == target then 1 else 0) clients
  pure ()
 where
  count snapshot window = Map.findWithDefault 0 window (pressedKeys snapshot)

greedyScenario :: Fixture -> Probe -> IO ()
greedyScenario fixture probe = do
  a <- openWindow probe Red
  b <- openWindow probe Green
  press fixture ShrinkMaster
  (first, second) <- twoOutputNames fixture
  let pair focused = [Paint Green (Box 2 2 316 596) focused, Paint Red (Box 322 2 476 596) False]
  picture fixture "first workspace ratio" [(first, pair True), (second, [])]
  press fixture SelectTwo
  c <- openWindow probe Blue
  press fixture Cycle
  press fixture Cycle
  picture fixture "distinct visible workspace layouts" [(first, pair False), (second, [full Blue True])]
  press fixture One
  picture fixture "greedy exchange" [(first, [full Blue False]), (second, pair True)]
  delivery fixture probe [a, b, c] b
  press fixture Two
  picture fixture "greedy exchange back" [(first, pair False), (second, [full Blue True])]
  delivery fixture probe [a, b, c] c

layerScenario :: Fixture -> Probe -> IO ()
layerScenario fixture probe = do
  _ <- openWindow probe Red
  onFirst fixture "layer unmapped" [full Red True]
  withLayer fixture Layer.KeyboardInteractivityNone $ do
    _ <- waitUntil "exclusive area y=32" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (\inventory -> any ((== Just (RiverSize 800 600)) . riverOutputDimensions) (riverInventoryOutputs inventory) && any (maybe False (\r -> rectX r == 0 && rectY r == 32 && rectWidth r == 800 && rectHeight r == 568) . riverOutputUsableArea) (riverInventoryOutputs inventory))
    onFirst fixture "exclusive zone moves client" [Paint Red (Box 2 34 796 564) True]
    frame <- captureOutput (mSocket fixture) (head (mOutputs fixture))
    unless (all (\(x, y) -> pixel frame x y == BS.pack [0x33, 0x66, 0x99]) [(x, y) | x <- [0 .. 799], y <- [0 .. 31]]) (failIntegration "exclusive-zone bar pixel strip differs")
  _ <- waitUntil "exclusive area restored" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (\inventory -> any (maybe False (\r -> rectX r == 0 && rectY r == 0 && rectWidth r == 800 && rectHeight r == 600) . riverOutputUsableArea) (riverInventoryOutputs inventory))
  onFirst fixture "layer teardown restores client" [full Red True]

withLayer :: Fixture -> Layer.KeyboardInteractivity -> IO a -> IO a
withLayer fixture interactivity action = do
  control <- Layer.newFixtureControl
  ready <- newEmptyMVar
  stopped <- newEmptyMVar
  thread <- forkFinally (Layer.runLayerSurfaceFixture control (Layer.FixtureConfig 32 interactivity) (UnixSocketPath (mSocket fixture)) (putMVar ready)) (putMVar stopped)
  let stop = Layer.requestStop control >> waitForMVar "native layer shutdown" stopped >>= requireRight "layer thread" >>= requireRight "layer protocol" >>= assertEqual "layer stop" Layer.StoppedByRequest
  ( do
      size <- waitForMVar "native layer ready" ready
      assertEqual "layer dimensions" (Layer.FixtureReady 800 32) size
      action
    )
    `finally` (stop `finally` killThread thread)

outputScenario :: Fixture -> Probe -> IO ()
outputScenario fixture probe = do
  a <- openWindow probe Red
  b <- openWindow probe Green
  press fixture ShrinkMaster
  (first, second) <- twoOutputNames fixture
  let pair focused = [Paint Green (Box 2 2 316 596) focused, Paint Red (Box 322 2 476 596) False]
  picture fixture "output one nondefault ratio" [(first, pair True), (second, [])]
  press fixture SelectTwo
  c <- openWindow probe Blue
  press fixture Cycle
  picture fixture "output two Mirror layout" [(first, pair False), (second, [full Blue True])]
  press fixture One
  picture fixture "current output before removal" [(first, [full Blue False]), (second, pair True)]
  before <- workspaceSnapshot fixture
  windowIds <- inventoryWindowIds fixture
  disable fixture second 1
  picture fixture "removed current output absent on survivor" [(first, [full Blue True])]
  delivery fixture probe [a, b, c] c
  workspaceSnapshot fixture >>= assertEqual "workspace state after 2 to 1" before
  disable fixture first 0
  _ <- waitUntil "headless core" assertionTimeoutMicroseconds (readPolicyState (mPolicy fixture)) (\case HeadlessState _ -> True; _ -> False)
  _ <- waitUntil "headless keyboard cleared" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Nothing) . keyboardOwner)
  workspaceSnapshot fixture >>= assertEqual "headless workspace state" before
  inventoryWindowIds fixture >>= assertEqual "headless windows remain open" windowIds
  enable fixture first 0 1
  picture fixture "first output restores remembered workspace" [(first, [full Blue True])]
  enable fixture second 1 2
  picture fixture "second output restores preserved ratio" [(first, [full Blue True]), (second, pair False)]
  workspaceSnapshot fixture >>= assertEqual "restored workspace stacks layouts focus" before
  inventoryWindowIds fixture >>= assertEqual "restored river window identities" windowIds
  delivery fixture probe [a, b, c] c
  press fixture One
  picture fixture "explicit greedy view restores tag one to first head" [(first, pair True), (second, [full Blue False])]
  delivery fixture probe [a, b, c] b

twoOutputNames :: Fixture -> IO (Text, Text)
twoOutputNames fixture = case mOutputs fixture of
  [first, second] -> pure (first, second)
  names -> failIntegration ("expected two native output names, got " <> show names)

workspaceSnapshot :: Fixture -> IO [SS.Workspace String M1Layout]
workspaceSnapshot fixture = do
  state <- readPolicyState (mPolicy fixture)
  pure . sortOn SS.tag $ case state of
    AttachedState ss _ -> SS.workspaces ss
    HeadlessState hw -> headlessCurrent hw : headlessHidden hw

inventoryWindowIds :: Fixture -> IO [ObjectId]
inventoryWindowIds fixture = sortOn show . map (ObjectTable.objectId . riverWindowObject) . riverInventoryWindows <$> readRiverInventory (mSession fixture)

disable :: Fixture -> Text -> Int -> IO ()
disable fixture name remaining = do
  old <- outputIdentity fixture name
  void (runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", Text.unpack name, "--off"])
  _ <- waitUntil "actual WM output removed" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (\inventory -> length (riverInventoryOutputs inventory) == remaining && all ((/= old) . ObjectTable.objectId . riverOutputObject) (riverInventoryOutputs inventory))
  pure ()

enable :: Fixture -> Text -> Int -> Int -> IO ()
enable fixture name position count = do
  void (runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", Text.unpack name, "--on", "--custom-mode", "800x600", "--pos", show (position * 800) <> ",0", "--scale", "1", "--transform", "normal"])
  _ <- waitUntil "actual WM output added" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (\inventory -> length (riverInventoryOutputs inventory) == count && all ((/= Nothing) . riverOutputRectangle) (riverInventoryOutputs inventory))
  pure ()

outputIdentity :: Fixture -> Text -> IO ObjectId
outputIdentity fixture name = do
  inventory <- readRiverInventory (mSession fixture)
  let index = if name == head (mOutputs fixture) then 0 else 1
  case find ((== Just (RiverPoint (index * 800) 0)) . riverOutputPosition) (riverInventoryOutputs inventory) of
    Nothing -> failIntegration ("output position unavailable for " <> show name)
    Just output -> pure (ObjectTable.objectId (riverOutputObject output))

withFixture :: String -> Int -> (Fixture -> Probe -> IO ()) -> IO ()
withFixture name outputCount action = do
  requireExecutables ["river", "wtype", "wlr-randr"]
  executable <- getExecutablePath
  host <- getEnvironment
  withSystemTempDirectory "seki-m1" $ \directory -> do
    let runtime = directory </> "runtime"
        config = directory </> "config"
        initSocket = directory </> "init.sock"
    createPrivateDirectory runtime
    createPrivateDirectory config
    let riverEnv = fixtureEnvironment host runtime config <> [("WLR_BACKENDS", "headless"), ("WLR_HEADLESS_OUTPUTS", show outputCount)]
    withUnixListener initSocket $ \listener ->
      bracket (startManagedProcess "river" riverEnv (proc "river" (riverArguments False executable initSocket))) stopManagedProcess $ \river -> do
        names <- waitForRiverInit listener river
        let environment = fixtureEnvironment host runtime config <> [("WAYLAND_DISPLAY", nestedWaylandDisplay names)]
            socketPath = runtime </> nestedWaylandDisplay names
        policy <- newPolicy m1Config >>= requireRight "M1 production policy"
        session <- openRiverSessionAt (UnixSocketPath socketPath) defaultObjectLimit (policyRuntimeHandlers policy) >>= requireRight "M1 runtime"
        result <- newEmptyMVar
        void (forkIO (runRiverSession session >>= putMVar result))
        let cleanup = stopManagedProcess river >> waitForRuntimeExit result
        ( do
            _ <- waitUntil "M1 outputs discovered" assertionTimeoutMicroseconds (readRiverInventory session) (\i -> length (riverInventoryOutputs i) == outputCount && all ((/= Nothing) . riverOutputRectangle) (riverInventoryOutputs i))
            namesByGlobal <- outputNames socketPath
            initialState <- readPolicyState policy
            inventory <- readRiverInventory session
            currentGlobal <- case initialState of
              AttachedState ss _ -> case find ((== SS.output (SS.current ss)) . SS.OutputId . objectIdValue . ObjectTable.objectId . riverOutputObject) (riverInventoryOutputs inventory) of
                Just output -> maybe (failIntegration "current output lacks wl_output global") pure (riverOutputWlOutputName output)
                Nothing -> failIntegration "current core output absent from inventory"
              HeadlessState _ -> failIntegration "M1 initial core unexpectedly headless"
            firstOutput <- maybe (failIntegration "current wl_output has no native name") pure (lookup currentGlobal namesByGlobal)
            let outputs = firstOutput : filter (/= firstOutput) (map snd namesByGlobal)
            forM_ (zip [0 :: Int ..] outputs) $ \(index, output) -> void (runProcessCapture environment "wlr-randr" ["--output", Text.unpack output, "--custom-mode", "800x600", "--pos", show (index * 800) <> ",0", "--scale", "1", "--transform", "normal"])
            let fixture = Fixture socketPath environment session policy outputs name
            bracket (startManagedProcess "virtual keyboard lifetime" environment (proc "wtype" ["-s", "60000"])) stopManagedProcess $ \keeper ->
              withProbe
                socketPath
                ( \probe ->
                    action fixture probe
                      `onException` (try @SomeException (keyboardSnapshot probe) >>= announce . ("native keyboard snapshot: " <>) . show)
                )
                `onException` (renderManagedProcessLogs keeper >>= announce)
            _ <- waitUntil "probe windows closed" assertionTimeoutMicroseconds (readRiverInventory session) (null . riverInventoryWindows)
            _ <- waitUntil "policy windows closed" assertionTimeoutMicroseconds (readPolicyState policy) (\case AttachedState ss _ -> null (SS.allWindows ss); HeadlessState _ -> False)
            stopManagedProcess river
            requireRuntimeExit result >>= assertEqual "M1 runtime shutdown" (Right ())
          )
          `onException` ( do
                            readRiverInventory session >>= announce . ((name <> " inventory: ") <>) . show
                            readPolicyState policy >>= announce . ("core state: " <>) . show
                            readRiverSeatBindings session >>= announce . ("seat bindings: " <>) . show
                            readWaylandGlobalCatalog (riverWaylandSession session) >>= announce . ("Wayland globals: " <>) . show
                            tryReadMVar result >>= announce . ("runtime result: " <>) . show
                            renderManagedProcessLogs river >>= announce
                        )
          `finally` cleanup

m1Config :: PolicyConfig String M1Layout
m1Config =
  PolicyConfig
    { policyLayout = Tall 1 (1 / 10) (1 / 2) ||| Mirror (Tall 1 (1 / 10) (1 / 2)) ||| Full
    , policyTags = ["1", "2", "3"]
    , policyBindings = [(RiverKeyBinding (0xffbe + fromIntegral (fromEnum key)) (River.RiverSeatV1ModifiersFlags 0) (Just 0), action key) | key <- [Down .. SelectTwo]]
    , policySceneStyle = SceneStyle 2 (BorderColor maxBound 0 maxBound maxBound) (BorderColor 0x40404040 0x40404040 0x40404040 maxBound)
    , policyLaunch = \_ -> failIntegration "unexpected spawn in M1 policy"
    }
 where
  action :: Key -> Action String M1Layout ()
  action Down = windows SS.focusDown
  action Up = windows SS.focusUp
  action ShrinkMaster = sendMessage Shrink
  action ExpandMaster = sendMessage Expand
  action More = sendMessage (IncMasterN 1)
  action Fewer = sendMessage (IncMasterN (-1))
  action Swap = windows SS.swapMaster
  action Cycle = sendMessage NextLayout
  action One = windows (SS.greedyView "1")
  action Two = windows (SS.greedyView "2")
  action Three = windows (SS.greedyView "3")
  action ShiftTwo = windows (SS.shift "2")
  action SelectTwo = windows (SS.view "2")

press :: Fixture -> Key -> IO ()
press fixture key = runInputDriver (mEnvironment fixture) ["-k", "F" <> show (fromEnum key + 1)]

three :: Probe -> IO (ProbeWindow, ProbeWindow, ProbeWindow)
three probe = (,,) <$> openWindow probe Red <*> openWindow probe Green <*> openWindow probe Blue

baseline :: Color -> [Paint]
baseline focused = [Paint Blue (Box 2 2 396 596) (focused == Blue), Paint Green (Box 402 2 396 296) (focused == Green), Paint Red (Box 402 302 396 296) (focused == Red)]

picture :: Fixture -> String -> [(Text, [Paint])] -> IO ()
picture fixture phase expected = do
  start <- getMonotonicTimeNSec
  let loop = do
        frames <- forM expected $ \(name, paints) -> do
          frame <- captureOutput (mSocket fixture) name
          pure (name, paints, frame, pictureError paints frame)
        case [(name, errorValue) | (name, _, _, Just errorValue) <- frames] of
          [] -> announce (mName fixture <> ": " <> phase <> " pixels verified")
          errors -> do
            now <- getMonotonicTimeNSec
            if now - start < 10 * 1000 * 1000 * 1000
              then threadDelay pollIntervalMicroseconds >> loop
              else do
                artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
                forM_ artifacts $ \directory -> do
                  createDirectoryIfMissing True directory
                  forM_ frames $ \(name, _, frame, _) -> BS.writeFile (directory </> mName fixture <> "-" <> Text.unpack name <> ".ppm") (ppm frame)
                failIntegration (phase <> ": " <> show errors)
  loop

pictureError :: [Paint] -> Image -> Maybe String
pictureError paints frame
  | (imageWidth frame, imageHeight frame) /= (800, 600) = Just ("capture dimensions " <> show (imageWidth frame, imageHeight frame))
  | BS.length (imageRgb frame) /= 800 * 600 * 3 = Just "capture byte count"
  | otherwise = firstError (map checkColor [Red, Green, Blue] <> map checkBorder paints)
 where
  checkColor color =
    let points = [(x, y) | y <- [0 .. 599], x <- [0 .. 799], pixel frame x y == rgb color]
        actual = case points of [] -> Nothing; _ -> Just (length points, Box (minimum (map fst points)) (minimum (map snd points)) (maximum (map fst points) - minimum (map fst points) + 1) (maximum (map snd points) - minimum (map snd points) + 1))
        expected = case find (\(Paint c _ _) -> c == color) paints of Nothing -> Nothing; Just (Paint _ box@(Box _ _ width height) _) -> Just (width * height, box)
     in if actual == expected then Nothing else Just (show color <> " coverage expected " <> show expected <> ", got " <> show actual)
  checkBorder (Paint color (Box x y width height) focused) =
    let target = if focused then BS.pack [255, 0, 255] else BS.pack [64, 64, 64]
        strips = [(px, py) | py <- [y - 2 .. y + height + 1], px <- [x - 2 .. x + width + 1], px < x || px >= x + width || py < y || py >= y + height]
     in case find (\(px, py) -> pixel frame px py /= target) strips of
          Nothing -> Nothing
          Just point@(px, py) -> Just (show color <> " border mismatch at " <> show point <> ", expected " <> show target <> ", got " <> show (pixel frame px py))

pixel :: Image -> Int -> Int -> BS.ByteString
pixel frame x y = BS.take 3 (BS.drop ((y * imageWidth frame + x) * 3) (imageRgb frame))
rgb :: Color -> BS.ByteString
rgb Red = BS.pack [255, 0, 0]
rgb Green = BS.pack [0, 255, 0]
rgb Blue = BS.pack [0, 0, 255]
firstError :: [Maybe String] -> Maybe String
firstError [] = Nothing
firstError (Just errorValue : _) = Just errorValue
firstError (Nothing : rest) = firstError rest
ppm :: Image -> BS.ByteString
ppm frame = BS8.pack ("P6\n" <> show (imageWidth frame) <> " " <> show (imageHeight frame) <> "\n255\n") <> imageRgb frame
