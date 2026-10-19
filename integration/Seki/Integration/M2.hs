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
import Seki.Internal.Config (Config (..), KeyBinding (..), Keysym (..), Mods (..), additionalKeys, def, defaultManageHook)
import Seki.Internal.Core.Action (Action, spawn, spawnOnce, windows)
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
  , ("m2-float-dialog", withFixture m2Config{policyManageHook = defaultManageHook} "m2-float-dialog" 1 floatDialog)
  , ("m2-fullscreen", withFixture m2Config{policyBindings = viewBindings} "m2-fullscreen" 1 fullscreen)
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
  pressView fixture ViewTwo
  onFirst "workspace 2 shows the shifted window" [Paint Green fullBox True]
  shifted <- takeMVar pending >>= either throwIO pure
  _ <- waitUntil "shifted window takes keyboard focus on its workspace" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just shifted) . keyboardOwner)
  pressView fixture ViewOne
  onFirst "workspace 1 restored" [Paint Red fullBox True]
  blue <- openWindow probe Blue
  onFirst "non-matching window tiles as before" [Paint Blue (Box 2 2 396 596) True, Paint Red (Box 402 2 396 596) False]
  _ <- waitUntil "non-matching window takes keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just blue) . keyboardOwner)
  pure ()
 where
  coreId = SS.WindowId . objectIdValue . riverId
  onFirst phase paints = picture fixture phase [(head (mOutputs fixture), paints)]
  fullBox = Box 2 2 796 596

shiftedAppId :: Text
shiftedAppId = "seki-m2-shift"

data ViewKey = ViewOne | ViewTwo deriving (Enum, Show)

-- | F1 and F2 with no modifier view workspaces 1 and 2.
viewBindings :: [(RiverKeyBinding, Action String Tall ())]
viewBindings = [(RiverKeyBinding (0xffbe + fromIntegral (fromEnum key)) (River.RiverSeatV1ModifiersFlags 0) (Just 0), windows (SS.view tag)) | (key, tag) <- [(ViewOne, "1"), (ViewTwo, "2")]]

pressView :: Fixture -> ViewKey -> IO ()
pressView fixture key = runInputDriver (mEnvironment fixture) ["-k", "F" <> show (fromEnum key + 1)]

manageShiftConfig :: PolicyConfig String Tall
manageShiftConfig =
  m2Config
    { policyBindings = viewBindings
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

{- | Under the default manage hook a window with a parent floats over its tiled
parent, at the size its client commits, centered on the 800 by 600 output.
-}
floatDialog :: Fixture -> Probe -> IO ()
floatDialog fixture probe = do
  parent <- openWindow probe Red
  onFirst "the parent tiles alone" [Paint Red fullBox True]
  dialog <- openWindowWith probe (windowSpec Green){specParent = Just parent, specCommit = CommitFixed 300 200}
  onFirst "the dialog floats centered over its parent at its own size" [Paint Red fullBox False, Paint Green dialogBox True]
  _ <- waitUntil "the dialog has keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just dialog) . keyboardOwner)
  dialogCore <- riverWindow fixture "the dialog reaches river" ((== Just "seki-color-probe-2") . riverWindowAppId)
  state <- coreWorkspaces <$> readPolicyState (mPolicy fixture)
  case state of
    AttachedState ss _ -> assertEqual "the dialog floats with no stored box" (Map.singleton (SS.WindowId (objectIdValue (riverId dialogCore))) SS.ClientSized) (SS.floating ss)
    HeadlessState _ -> failIntegration "core unexpectedly headless"
  click (mSocket fixture) (800, 600) (100, 100)
  _ <- waitUntil "a click on the parent takes keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just parent) . keyboardOwner)
  onFirst "refocusing the parent keeps the dialog above it" [Paint Red fullBox True, Paint Green dialogBox False]
 where
  onFirst phase paints = picture fixture phase [(head (mOutputs fixture), paints)]
  fullBox = Box 2 2 796 596
  dialogBox = Box 250 200 300 200

{- | The focused probe window covers the whole output when it requests
fullscreen, and returns to its tile with its border when it requests exit.
While its workspace is hidden it covers nothing, so another workspace's
window shows, and it covers the output again when its workspace returns.
-}
fullscreen :: Fixture -> Probe -> IO ()
fullscreen fixture probe = do
  pressView fixture ViewTwo
  _ <- openWindow probe Blue
  onFirst "workspace 2 tiles its window alone" otherWorkspace
  pressView fixture ViewOne
  _ <- openWindow probe Red
  onFirst "the first window tiles alone" [Paint Red (Box 2 2 796 596) True]
  green <- openWindow probe Green
  onFirst "the second window tiles as master" tiled
  greenCore <- riverWindow fixture "the second window reaches river" ((== Just "seki-color-probe-3") . riverWindowAppId)
  requestFullscreen probe green
  requested fixture "the fullscreen request reaches seki" greenCore FullscreenRequested
  onFirst "the fullscreen window covers the output" covering
  pressView fixture ViewTwo
  onFirst "workspace 2's window shows while the fullscreen window's workspace is hidden" otherWorkspace
  pressView fixture ViewOne
  onFirst "the fullscreen window covers the output again" covering
  _ <- waitUntil "the fullscreen window has keyboard focus again" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just green) . keyboardOwner)
  requestExitFullscreen probe green
  requested fixture "the exit request reaches seki" greenCore ExitFullscreenRequested
  onFirst "exit returns the window to its tile" tiled
  _ <- waitUntil "the window keeps keyboard focus after exit" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just green) . keyboardOwner)
  pure ()
 where
  onFirst phase paints = picture fixture phase [(head (mOutputs fixture), paints)]
  tiled = [Paint Green (Box 2 2 396 596) True, Paint Red (Box 402 2 396 596) False]
  covering = [Paint Green (Box 0 0 800 600) True]
  otherWorkspace = [Paint Blue (Box 2 2 796 596) True]

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
