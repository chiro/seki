{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Fixture (
  NestedRiver (..),
  withNestedRiver,
  Fixture (..),
  Box (..),
  Paint (..),
  BorderStyle (..),
  WindowRequest (..),
  withFixture,
  withFixtureUsing,
  saveCapture,
  withHeldLogo,
  withHeldLogoIn,
  windowRequests,
  picture,
  pictureError,
  pictureErrorWith,
  outputPicture,
  withKeyboard,
  pixel,
  rgb,
) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryReadMVar)
import Control.Exception (SomeException, bracket, finally, onException, try)
import Control.Monad (forM, forM_, void)
import Data.Bits (complement, shiftR, xor, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Fixture.Probe
import Seki.Integration.Harness
import Seki.Internal.Core.Layout (LayoutClass)
import Seki.Internal.Core.StackSet qualified as SS
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory
import Seki.Run.Policy
import Seki.Run.River (RiverWindowManagerHandlers (..))
import Seki.Run.Runtime
import Seki.Run.Startup (readWaylandGlobalCatalog)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Connection.ObjectTable qualified as ObjectTable
import Seki.Wire.Transport (ConnectionTarget (UnixSocketPath))
import Seki.Wire.Types (ObjectId, objectIdValue)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc)

data Fixture l = Fixture
  { mSocket :: FilePath
  , mDispatcher :: ThreadId
  , mEnvironment :: [(String, String)]
  , mSession :: RiverSession
  , mPolicy :: Policy String l
  , mOutputs :: [Text]
  , mName :: String
  , mRequests :: IORef (Map ObjectId [WindowRequest])
  }

data WindowRequest = FullscreenRequested | ExitFullscreenRequested | PointerMoveRequested | PointerResizeRequested deriving (Eq, Show)

data Box = Box Int Int Int Int deriving (Eq, Show)

-- | A window's content box and whether its border is focused. A picture lists paints bottom to top, so a later paint's content and border hide what lies under them.
data Paint = Paint Color Box Bool deriving (Eq, Show)

{- | The border width and the focused and unfocused border pixels a picture
expects. A probe color equal to a border pixel is not checked for coverage.
-}
data BorderStyle = BorderStyle Int BS.ByteString BS.ByteString deriving (Eq, Show)

-- | A nested headless river with no window manager yet.
data NestedRiver = NestedRiver
  { riverSocket :: FilePath
  , riverEnvironment :: [(String, String)]
  -- ^ What a client of the nested river runs with.
  , riverRuntimeDirectory :: FilePath
  , riverProcess :: ManagedProcess
  }

withNestedRiver :: Int -> (NestedRiver -> IO a) -> IO a
withNestedRiver outputCount action = do
  requireExecutables ["river", "wtype", "wlr-randr"]
  executable <- getExecutablePath
  host <- getEnvironment
  withSystemTempDirectory "seki-fixture" $ \directory -> do
    let runtime = directory </> "runtime"
        config = directory </> "config"
        initSocket = directory </> "init.sock"
    createPrivateDirectory runtime
    createPrivateDirectory config
    let riverEnv = fixtureEnvironment host runtime config <> [("WLR_BACKENDS", "headless"), ("WLR_HEADLESS_OUTPUTS", show outputCount)]
    withUnixListener initSocket $ \listener ->
      bracket (startManagedProcess "river" riverEnv (proc "river" (riverArguments False executable initSocket))) stopManagedProcess $ \river -> do
        names <- waitForRiverInit listener river
        action
          NestedRiver
            { riverSocket = runtime </> nestedWaylandDisplay names
            , riverEnvironment = fixtureEnvironment host runtime config <> [("WAYLAND_DISPLAY", nestedWaylandDisplay names)]
            , riverRuntimeDirectory = runtime
            , riverProcess = river
            }

withFixture :: (LayoutClass l) => PolicyConfig String l -> String -> Int -> (Fixture l -> Probe -> IO ()) -> IO ()
withFixture = withFixtureUsing id

withFixtureUsing :: (LayoutClass l) => (RiverRuntimeHandlers -> RiverRuntimeHandlers) -> PolicyConfig String l -> String -> Int -> (Fixture l -> Probe -> IO ()) -> IO ()
withFixtureUsing transform policyConfig name outputCount action =
  withNestedRiver outputCount $ \(NestedRiver socketPath environment _ river) -> do
    policy <- newPolicy policyConfig >>= requireRight "production policy"
    requests <- newIORef Map.empty
    session <- openRiverSessionAt (UnixSocketPath socketPath) defaultObjectLimit (transform (recordWindowRequests requests (policyRuntimeHandlers policy))) >>= requireRight "runtime"
    result <- newEmptyMVar
    dispatcher <- forkIO (runRiverSession session >>= putMVar result)
    let cleanup = stopManagedProcess river >> waitForRuntimeExit result
    ( do
        _ <- waitUntil "fixture outputs discovered" assertionTimeoutMicroseconds (readRiverInventory session) (\i -> length (riverInventoryOutputs i) == outputCount && all ((/= Nothing) . riverOutputRectangle) (riverInventoryOutputs i))
        namesByGlobal <- outputNames socketPath
        initialState <- readPolicyState policy
        inventory <- readRiverInventory session
        currentGlobal <- case coreWorkspaces initialState of
          AttachedState ss _ -> case find ((== SS.output (SS.current ss)) . SS.OutputId . objectIdValue . ObjectTable.objectId . riverOutputObject) (riverInventoryOutputs inventory) of
            Just output -> maybe (failIntegration "current output lacks wl_output global") pure (riverOutputWlOutputName output)
            Nothing -> failIntegration "current core output absent from inventory"
          HeadlessState _ -> failIntegration "initial core unexpectedly headless"
        firstOutput <- maybe (failIntegration "current wl_output has no native name") pure (lookup currentGlobal namesByGlobal)
        let outputs = firstOutput : filter (/= firstOutput) (map snd namesByGlobal)
        forM_ (zip [0 :: Int ..] outputs) $ \(index, output) -> void (runProcessCapture environment "wlr-randr" ["--output", Text.unpack output, "--custom-mode", "800x600", "--pos", show (index * 800) <> ",0", "--scale", "1", "--transform", "normal"])
        let fixture = Fixture socketPath dispatcher environment session policy outputs name requests
        bracket (startManagedProcess "virtual keyboard lifetime" environment (proc "wtype" ["-s", "60000"])) stopManagedProcess $ \keeper ->
          withProbe
            socketPath
            ( \probe ->
                action fixture probe
                  `onException` (try @SomeException (keyboardSnapshot probe) >>= announce . ("native keyboard snapshot: " <>) . show)
            )
            `onException` (renderManagedProcessLogs keeper >>= announce)
        _ <- waitUntil "probe windows closed" assertionTimeoutMicroseconds (readRiverInventory session) (null . riverInventoryWindows)
        _ <- waitUntil "policy windows closed" assertionTimeoutMicroseconds (coreWorkspaces <$> readPolicyState policy) (\case AttachedState ss _ -> null (SS.allWindows ss); HeadlessState _ -> False)
        stopManagedProcess river
        requireRuntimeExit result >>= assertEqual "runtime shutdown" (Right RiverEnded)
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

withHeldLogo :: Fixture l -> Probe -> IO a -> IO a
withHeldLogo fixture = withHeldLogoIn (mEnvironment fixture)

-- | Hold the logo modifier from a client of this environment while the action runs.
withHeldLogoIn :: [(String, String)] -> Probe -> IO a -> IO a
withHeldLogoIn environment probe action = do
  result <-
    bracket (startManagedProcess "held logo modifier" environment (proc "wtype" ["-M", "logo", "-s", "60000"])) stopManagedProcess $ \_ -> do
      _ <- waitUntil "logo modifier held on the focused window" assertionTimeoutMicroseconds (keyboardSnapshot probe) (logoHeld . depressedModifiers)
      action
  _ <- waitUntil "logo modifier released after its keyboard stops" assertionTimeoutMicroseconds (keyboardSnapshot probe) (not . logoHeld . depressedModifiers)
  pure result
 where
  logoHeld modifiers = modifiers .&. 0x40 /= 0

windowRequests :: Fixture l -> ObjectId -> IO [WindowRequest]
windowRequests fixture window = Map.findWithDefault [] window <$> readIORef (mRequests fixture)

recordWindowRequests :: IORef (Map ObjectId [WindowRequest]) -> RiverRuntimeHandlers -> RiverRuntimeHandlers
recordWindowRequests requests handlers = handlers{riverProtocolHandlers = protocol{handleRiverWindowEvent = record}}
 where
  protocol = riverProtocolHandlers handlers
  record window event = do
    forM_ (windowRequest event) $ \value -> atomicModifyIORef' requests (\logged -> (Map.insertWith (flip (<>)) (ObjectTable.objectId window) [value] logged, ()))
    handleRiverWindowEvent protocol window event
  windowRequest = \case
    River.RiverWindowV1FullscreenRequested _ -> Just FullscreenRequested
    River.RiverWindowV1ExitFullscreenRequested -> Just ExitFullscreenRequested
    River.RiverWindowV1PointerMoveRequested _ -> Just PointerMoveRequested
    River.RiverWindowV1PointerResizeRequested _ _ -> Just PointerResizeRequested
    _ -> Nothing

picture :: Fixture l -> String -> [(Text, [Paint])] -> IO ()
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
pictureError = pictureErrorWith (BorderStyle 2 (BS.pack [255, 0, 255]) (BS.pack [64, 64, 64]))

pictureErrorWith :: BorderStyle -> [Paint] -> Image -> Maybe String
pictureErrorWith (BorderStyle border focusedPixel unfocusedPixel) paints frame
  | (imageWidth frame, imageHeight frame) /= (800, 600) = Just ("capture dimensions " <> show (imageWidth frame, imageHeight frame))
  | BS.length (imageRgb frame) /= 800 * 600 * 3 = Just "capture byte count"
  | any (\(Paint color _ _) -> rgb color `elem` borderPixels) paints = Just "a painted window has a border color"
  | otherwise = firstError (map checkColor (filter ((`notElem` borderPixels) . rgb) [Red, Green, Blue]) <> zipWith checkBorder [1 ..] paints)
 where
  borderPixels = [focusedPixel, unfocusedPixel]
  covered index (px, py) = any (\(Paint _ (Box x y width height) _) -> px >= x - border && px < x + width + border && py >= y - border && py < y + height + border) (drop index paints)
  bounds points = case points of [] -> Nothing; _ -> Just (length points, Box (minimum (map fst points)) (minimum (map snd points)) (maximum (map fst points) - minimum (map fst points) + 1) (maximum (map snd points) - minimum (map snd points) + 1))
  checkColor color =
    let actual = bounds [(x, y) | y <- [0 .. 599], x <- [0 .. 799], pixel frame x y == rgb color]
        expected = case break (\(Paint c _ _) -> c == color) paints of
          (_, []) -> Nothing
          (below, Paint _ (Box x y width height) _ : _) -> bounds [(px, py) | py <- [y .. y + height - 1], px <- [x .. x + width - 1], not (covered (length below + 1) (px, py))]
     in if actual == expected then Nothing else Just (show color <> " coverage expected " <> show expected <> ", got " <> show actual)
  checkBorder index (Paint color (Box x y width height) focused) =
    let target = if focused then focusedPixel else unfocusedPixel
        strips = [(px, py) | py <- [max 0 (y - border) .. min 599 (y + height + border - 1)], px <- [max 0 (x - border) .. min 799 (x + width + border - 1)], px < x || px >= x + width || py < y || py >= y + height, not (covered index (px, py))]
     in case find (\(px, py) -> pixel frame px py /= target) strips of
          Nothing -> Nothing
          Just point@(px, py) -> Just (show color <> " border mismatch at " <> show point <> ", expected " <> show target <> ", got " <> show (pixel frame px py))

{- | Poll one output until the check passes, for up to ten seconds, and
announce the phase under the scenario's name.
-}
outputPicture :: String -> FilePath -> Text -> String -> (Image -> Maybe String) -> IO ()
outputPicture scenario socket output phase check = do
  start <- getMonotonicTimeNSec
  let loop = do
        frame <- captureOutput socket output
        case check frame of
          Nothing -> announce (scenario <> ": " <> phase <> " pixels verified")
          Just e -> do
            now <- getMonotonicTimeNSec
            if now - start < 10 * 1000 * 1000 * 1000 then threadDelay pollIntervalMicroseconds >> loop else failIntegration (phase <> ": " <> e)
  loop

-- | A virtual keyboard that outlives each wtype call, so the seat keeps a keyboard for focus.
withKeyboard :: [(String, String)] -> IO a -> IO a
withKeyboard environment action = bracket (startManagedProcess "virtual keyboard lifetime" environment (proc "wtype" ["-s", "60000"])) stopManagedProcess (const action)

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

saveCapture :: Fixture l -> String -> IO ()
saveCapture fixture label = do
  artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
  forM_ artifacts $ \directory -> do
    createDirectoryIfMissing True directory
    forM_ (mOutputs fixture) $ \output -> do
      frame <- captureOutput (mSocket fixture) output
      BS.writeFile (directory </> label <> "-" <> Text.unpack output <> ".png") (png frame)

png :: Image -> BS.ByteString
png frame = BS.pack [137, 80, 78, 71, 13, 10, 26, 10] <> chunk "IHDR" header <> chunk "IDAT" compressed <> chunk "IEND" BS.empty
 where
  header = word (fromIntegral (imageWidth frame)) <> word (fromIntegral (imageHeight frame)) <> BS.pack [8, 2, 0, 0, 0]
  raw = BS.concat [BS.singleton 0 <> BS.take (imageWidth frame * 3) (BS.drop (y * imageWidth frame * 3) (imageRgb frame)) | y <- [0 .. imageHeight frame - 1]]
  compressed = BS.pack [0x78, 0x01] <> blocks raw <> word adler
  adler = let (a, b) = BS.foldl' (\(accA, accB) x -> let next = (accA + fromIntegral x) `mod` 65521 in (next, (accB + next) `mod` 65521)) (1, 0) raw in b * 65536 + a
  blocks bytes =
    let (part, rest) = BS.splitAt 65535 bytes
        size = BS.length part
        little n = BS.pack [fromIntegral n, fromIntegral (n `div` 256)]
     in BS.singleton (if BS.null rest then 1 else 0) <> little size <> little (65535 - size) <> part <> if BS.null rest then BS.empty else blocks rest
  chunk name bytes = word (fromIntegral (BS.length bytes)) <> name <> bytes <> word (crc (name <> bytes))
  crc = complement . BS.foldl' (\c byte -> foldl' (\n _ -> if n .&. 1 == 1 then shiftR n 1 `xor` 0xedb88320 else shiftR n 1) (c `xor` fromIntegral byte) [1 .. 8 :: Int]) 0xffffffff
  word :: Word32 -> BS.ByteString
  word n = BS.pack [fromIntegral (shiftR n amount) | amount <- [24, 16, 8, 0]]
