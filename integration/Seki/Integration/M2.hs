{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.M2 (m2Scenarios) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Seki.Fixture.Probe
import Seki.Integration.Fixture hiding (Fixture)
import Seki.Integration.Fixture qualified as Shared
import Seki.Integration.Harness
import Seki.Internal.Core.Scene (BorderColor (..), SceneStyle (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Run.Inventory
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Wire.Connection.ObjectTable qualified as ObjectTable
import Seki.Wire.Types (ObjectId)

type Fixture = Shared.Fixture Tall

m2Scenarios :: [(String, IO ())]
m2Scenarios =
  [ ("m2-probe-self-test", withFixture m2Config "m2-probe-self-test" 1 selfTest)
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

riverWindow :: Fixture -> String -> (RiverWindowState -> Bool) -> IO RiverWindowState
riverWindow fixture phase predicate =
  waitUntil phase assertionTimeoutMicroseconds (filter predicate . riverInventoryWindows <$> readRiverInventory (mSession fixture)) ((== 1) . length) >>= \case
    [window] -> pure window
    windows -> failIntegration (phase <> ": expected one window, got " <> show windows)

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
    , policyLaunch = \_ -> failIntegration "unexpected spawn in M2 policy"
    }
