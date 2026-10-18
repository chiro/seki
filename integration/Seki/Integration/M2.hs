{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.M2 (m2Scenarios) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Seki.Fixture.Probe
import Seki.Integration.Fixture hiding (Fixture)
import Seki.Integration.Fixture qualified as Shared
import Seki.Integration.Harness
import Seki.Internal.Config (Config (..), KeyBinding (..), Keysym (..), Mods (..), additionalKeys, def)
import Seki.Internal.Core.Action (spawn, spawnOnce, windows)
import Seki.Internal.Core.ManageHook (appId, doShift, idHook, (-->), (=?))
import Seki.Internal.Core.Scene (BorderColor (..), SceneStyle (..))
import Seki.Internal.Core.StackSet qualified as SS
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Config (policyConfigOf)
import Seki.Run.Inventory
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable qualified as ObjectTable
import Seki.Wire.Types (ObjectId, objectIdValue)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

type Fixture = Shared.Fixture Tall

m2Scenarios :: [(String, IO ())]
m2Scenarios =
  [ ("m2-probe-self-test", withFixture m2Config "m2-probe-self-test" 1 selfTest)
  , ("m2-manage-shift", withFixture manageShiftConfig "m2-manage-shift" 1 manageShift)
  , ("m2-startup-keys", startupKeys)
  ]

selfTest :: Fixture -> Probe -> IO ()
selfTest fixture probe = do
  parent <- openWindowWith probe (windowSpec Red){specAppId = CustomAppId "seki-m2-custom", specMinSize = Just (120, 90), specMaxSize = Just (700, 500)}
  _ <- openWindowWith probe (windowSpec Green){specParent = Just parent, specCommit = CommitFixed 300 200}
  anonymous <- openWindowWith probe (windowSpec Blue){specAppId = NoAppId}
  custom <- riverWindow fixture "custom app_id reaches river" ((== Just "seki-m2-custom") . riverWindowAppId)
  _ <- riverWindow fixture "absent app_id stays absent in river" ((== Nothing) . riverWindowAppId)
  child <- riverWindow fixture "child window by numbered app_id" ((== Just "seki-color-probe-2") . riverWindowAppId)
  assertEqual "set_parent reaches river as the parent window" (Just (riverId custom)) (riverWindowParent child)
  assertEqual "min and max size reach river as the dimensions hint" (Just (RiverDimensionsHint 120 90 700 500)) (riverWindowDimensionsHint custom)
  _ <- riverWindow fixture "configured window commits the proposed stack tile" (\w -> riverWindowAppId w == Just "seki-m2-custom" && riverWindowDimensions w == Just (RiverSize 396 296))
  _ <- riverWindow fixture "fixed commit ignores the same-sized stack tile" (\w -> riverWindowAppId w == Just "seki-color-probe-2" && riverWindowDimensions w == Just (RiverSize 300 200))
  requestFullscreen probe parent
  requested fixture "set_fullscreen reaches the window manager" custom FullscreenRequested
  requestExitFullscreen probe parent
  requested fixture "unset_fullscreen reaches the window manager" custom ExitFullscreenRequested
  try @SomeException (requestMove probe anonymous) >>= either (const (pure ())) (const (failIntegration "move without a pointer press was sent"))
  withVirtualPointer (mSocket fixture) (800, 600) $ \pointer -> do
    pointerMoveTo pointer (600, 450)
    _ <- waitUntil "pointer enters the custom window" assertionTimeoutMicroseconds (pointerSnapshot probe) ((== Just parent) . pointerOwner)
    pointerPress pointer LeftButton
    _ <- waitUntil "held left press reaches the custom window" assertionTimeoutMicroseconds (pointerSnapshot probe) ((== 1) . Map.findWithDefault 0 parent . pointerPresses)
    requestMove probe parent
    requested fixture "move with the held press serial reaches the window manager" custom PointerMoveRequested
    pointerRelease pointer LeftButton
    _ <- waitUntil "left release reaches the custom window" assertionTimeoutMicroseconds (pointerSnapshot probe) ((== 1) . Map.findWithDefault 0 parent . pointerReleases)
    pure ()
  withHeldLogo fixture probe . withVirtualPointer (mSocket fixture) (800, 600) $ \pointer -> do
    pointerMoveTo pointer (600, 450)
    before <- waitUntil "pointer over the custom window with logo held" assertionTimeoutMicroseconds (pointerSnapshot probe) ((== Just parent) . pointerOwner)
    pointerPress pointer LeftButton
    pointerMoveTo pointer (650, 480)
    pointerRelease pointer LeftButton
    let count field = Map.findWithDefault 0 parent . field
        moved after = (\(x0, y0) (x1, y1) -> (round (x1 - x0), round (y1 - y0))) <$> surfacePosition before <*> surfacePosition after
    _ <- waitUntil "logo press, absolute motion, and release reach the custom window" assertionTimeoutMicroseconds (pointerSnapshot probe) $ \after ->
      count pointerPresses after == count pointerPresses before + 1 && count pointerReleases after == count pointerReleases before + 1 && moved after == Just (50 :: Int, 30 :: Int)
    pure ()

{- | Seki proposes no dimensions for a window on a hidden workspace, so river
sends its initial configure only once the workspace is shown; until then the
probe's open stays pending.
-}
manageShift :: Fixture -> Probe -> IO ()
manageShift fixture probe = do
  red <- openWindow probe Red
  onFirst "first window tiles alone" [Paint Red fullBox True]
  _ <- waitUntil "first window has keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just red) . keyboardOwner)
  redCore <- coreId <$> riverWindow fixture "first window reaches river" ((== Just "seki-color-probe-1") . riverWindowAppId)
  pending <- newEmptyMVar
  _ <- forkIO (try @SomeException (openWindowWith probe (windowSpec Green){specAppId = CustomAppId shiftedAppId}) >>= putMVar pending)
  green <- coreId <$> riverWindow fixture "matching window reaches river" ((== Just shiftedAppId) . riverWindowAppId)
  _ <-
    waitUntil "doShift puts the matching window on hidden workspace 2 and keeps focus" assertionTimeoutMicroseconds (coreWorkspaces <$> readPolicyState (mPolicy fixture)) $ \case
      AttachedState ss _ -> SS.findTag green ss == Just "2" && SS.currentTag ss == "1" && SS.peek ss == Just redCore
      HeadlessState _ -> False
  onFirst "matching window is not shown" [Paint Red fullBox True]
  keyboardSnapshot probe >>= assertEqual "keyboard focus stays on the first window" (Just red) . keyboardOwner
  press ViewTwo
  onFirst "workspace 2 shows the shifted window" [Paint Green fullBox True]
  shifted <- takeMVar pending >>= either throwIO pure
  _ <- waitUntil "shifted window takes keyboard focus on its workspace" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just shifted) . keyboardOwner)
  press ViewOne
  onFirst "workspace 1 restored" [Paint Red fullBox True]
  blue <- openWindow probe Blue
  onFirst "non-matching window tiles as before" [Paint Blue (Box 2 2 396 596) True, Paint Red (Box 402 2 396 596) False]
  _ <- waitUntil "non-matching window takes keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just blue) . keyboardOwner)
  pure ()
 where
  coreId = SS.WindowId . objectIdValue . riverId
  onFirst phase paints = picture fixture phase [(head (mOutputs fixture), paints)]
  fullBox = Box 2 2 796 596
  press key = runInputDriver (mEnvironment fixture) ["-k", "F" <> show (fromEnum key + 1)]

shiftedAppId :: Text
shiftedAppId = "seki-m2-shift"

data ManageShiftKey = ViewOne | ViewTwo deriving (Enum, Show)

manageShiftConfig :: PolicyConfig String Tall
manageShiftConfig =
  m2Config
    { policyBindings = [(RiverKeyBinding (0xffbe + fromIntegral (fromEnum key)) (River.RiverSeatV1ModifiersFlags 0) (Just 0), windows (SS.view tag)) | (key, tag) <- [(ViewOne, "1"), (ViewTwo, "2")]]
    , policyManageHook = appId =? "seki-m2-shift" --> doShift "2"
    }

{- | A public configuration whose startup hook and modifier-less media key
each append a line to a file, launched through the production spawn path.
-}
startupKeys :: IO ()
startupKeys = withSystemTempDirectory "seki-m2-startup-keys" $ \directory -> do
  let launches = directory </> "launches"
      append line = "printf '%s\\n' " <> line <> " >> '" <> launches <> "'"
      config =
        additionalKeys
          def{startupHook = spawnOnce (append "startup")}
          [(KeyBinding (Mods 0) (Keysym 0x1008ff13), spawn (append "raise-volume"))]
      recorded = doesFileExist launches >>= \exists -> if exists then lines . BS8.unpack <$> BS8.readFile launches else pure []
  withFixture (policyConfigOf config) "m2-startup-keys" 1 $ \fixture _ -> do
    _ <- waitUntil "the startup hook spawns once the first output arrives" assertionTimeoutMicroseconds recorded (== ["startup"])
    runInputDriver (mEnvironment fixture) ["-k", "XF86AudioRaiseVolume"]
    _ <- waitUntil "the unmodified key spawns its command and the startup hook does not repeat" assertionTimeoutMicroseconds recorded (== ["startup", "raise-volume"])
    pure ()

riverWindow :: Fixture -> String -> (RiverWindowState -> Bool) -> IO RiverWindowState
riverWindow fixture phase predicate =
  waitUntil phase assertionTimeoutMicroseconds (filter predicate . riverInventoryWindows <$> readRiverInventory (mSession fixture)) ((== 1) . length) >>= \case
    [window] -> pure window
    found -> failIntegration (phase <> ": expected one window, got " <> show found)

requested :: Fixture -> String -> RiverWindowState -> WindowRequest -> IO ()
requested fixture phase window request = void (waitUntil phase assertionTimeoutMicroseconds (windowRequests fixture (riverId window)) (elem request))

riverId :: RiverWindowState -> ObjectId
riverId = ObjectTable.objectId . riverWindowObject

m2Config :: PolicyConfig String Tall
m2Config =
  PolicyConfig
    { policyLayout = Tall 1 (1 / 10) (1 / 2)
    , policyTags = ["1", "2", "3"]
    , policyBindings = []
    , policySceneStyle = SceneStyle 2 (BorderColor maxBound 0 maxBound maxBound) (BorderColor 0x40404040 0x40404040 0x40404040 maxBound)
    , policyManageHook = idHook
    , policyStartupHook = pure ()
    , policyLaunch = \_ -> failIntegration "unexpected spawn in M2 policy"
    }
