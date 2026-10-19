{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Fixture (
  Fixture (..),
  Box (..),
  Paint (..),
  WindowRequest (..),
  withFixture,
  withHeldLogo,
  windowRequests,
  picture,
  pictureError,
  pixel,
  rgb,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryReadMVar)
import Control.Exception (SomeException, bracket, finally, onException, try)
import Control.Monad (forM, forM_, void)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
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

withFixture :: (LayoutClass l) => PolicyConfig String l -> String -> Int -> (Fixture l -> Probe -> IO ()) -> IO ()
withFixture policyConfig name outputCount action = do
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
        let environment = fixtureEnvironment host runtime config <> [("WAYLAND_DISPLAY", nestedWaylandDisplay names)]
            socketPath = runtime </> nestedWaylandDisplay names
        policy <- newPolicy policyConfig >>= requireRight "production policy"
        requests <- newIORef Map.empty
        session <- openRiverSessionAt (UnixSocketPath socketPath) defaultObjectLimit (recordWindowRequests requests (policyRuntimeHandlers policy)) >>= requireRight "runtime"
        result <- newEmptyMVar
        void (forkIO (runRiverSession session >>= putMVar result))
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
            let fixture = Fixture socketPath environment session policy outputs name requests
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
            requireRuntimeExit result >>= assertEqual "runtime shutdown" (Right ())
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
withHeldLogo fixture probe action = do
  result <-
    bracket (startManagedProcess "held logo modifier" (mEnvironment fixture) (proc "wtype" ["-M", "logo", "-s", "60000"])) stopManagedProcess $ \_ -> do
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
pictureError paints frame
  | (imageWidth frame, imageHeight frame) /= (800, 600) = Just ("capture dimensions " <> show (imageWidth frame, imageHeight frame))
  | BS.length (imageRgb frame) /= 800 * 600 * 3 = Just "capture byte count"
  | otherwise = firstError (map checkColor [Red, Green, Blue] <> zipWith checkBorder [1 ..] paints)
 where
  covered index (px, py) = any (\(Paint _ (Box x y width height) _) -> px >= x - 2 && px < x + width + 2 && py >= y - 2 && py < y + height + 2) (drop index paints)
  bounds points = case points of [] -> Nothing; _ -> Just (length points, Box (minimum (map fst points)) (minimum (map snd points)) (maximum (map fst points) - minimum (map fst points) + 1) (maximum (map snd points) - minimum (map snd points) + 1))
  checkColor color =
    let actual = bounds [(x, y) | y <- [0 .. 599], x <- [0 .. 799], pixel frame x y == rgb color]
        expected = case break (\(Paint c _ _) -> c == color) paints of
          (_, []) -> Nothing
          (below, Paint _ (Box x y width height) _ : _) -> bounds [(px, py) | py <- [y .. y + height - 1], px <- [x .. x + width - 1], not (covered (length below + 1) (px, py))]
     in if actual == expected then Nothing else Just (show color <> " coverage expected " <> show expected <> ", got " <> show actual)
  checkBorder index (Paint color (Box x y width height) focused) =
    let target = if focused then BS.pack [255, 0, 255] else BS.pack [64, 64, 64]
        strips = [(px, py) | py <- [max 0 (y - 2) .. min 599 (y + height + 1)], px <- [max 0 (x - 2) .. min 799 (x + width + 1)], px < x || px >= x + width || py < y || py >= y + height, not (covered index (px, py))]
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
