{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.InputTest (tests) where

import Data.Map.Strict qualified as Map
import Seki.Internal.Input
import Seki.Protocol.RiverInputManagementV1 qualified as Input
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Input
import Seki.Wire.Connection.ObjectTable
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "runtime input"
    [ testCase "v1 captured callback excludes later hotplug and removed children" $ do
        a <- object 1
        b <- object 1
        let first = metadata (InputGeneration 0) (discoverInput a emptyInputInventory)
            cohort = discoveryCohort first
            second = metadata (InputGeneration 1) (discoverInput b first)
            removed = updateInput (InputGeneration 0) Input.RiverInputDeviceV1Removed second
            finished = finishDiscovery cohort removed
        assertEqual "removed remains retired" Removed (lifecycle 0 finished)
        assertEqual "later child still discovering" Discovering (lifecycle 1 finished)
        assertEqual "later callback readies its own child" Ready (lifecycle 1 (finishDiscovery (discoveryCohort finished) finished))
    , testCase "v2 stages metadata until done; later events do not repeat requests" $ do
        a <- object 2
        rules <- rulesFor [RepeatInfo 37 410]
        let initial = metadata (InputGeneration 0) (discoverInput a emptyInputInventory)
        assertEqual "no sends before done" [] (snd (planGeneric rules Map.empty initial))
        let ready = updateInput (InputGeneration 0) Input.RiverInputDeviceV1Done initial
            (sent, requests) = planGeneric rules Map.empty ready
        assertEqual "one request" [GenericRepeat (InputGeneration 0) 37 410] requests
        assertEqual "no feedback resend" [] (snd (planGeneric rules Map.empty (updateInput (InputGeneration 0) Input.RiverInputDeviceV1Done sent)))
        assertEqual "no fabricated application" (Just SettingPending) (settingOutcome <$> (Map.lookup (InputGeneration 0) (inputDevices sent) >>= Map.lookup RepeatInfoKey . inputResults))
    , testCase "wire ID reuse cannot revive a removed generation or old callback" $ do
        a <- object 1
        let first = metadata (InputGeneration 0) (discoverInput a emptyInputInventory)
            removed = updateInput (InputGeneration 0) Input.RiverInputDeviceV1Removed first
            replaced = metadata (InputGeneration 1) (discoverInput a removed)
            stale = finishDiscovery [InputGeneration 0] replaced
        assertEqual "old identity removed" Removed (lifecycle 0 stale)
        assertEqual "replacement not readied" Discovering (lifecycle 1 stale)
    , testCase "output absence and readd retry exactly once against fresh target" $ do
        a <- object 2
        out1 <- objectId <$> object 1
        out2 <- objectId <$> object 2
        rules <- rulesFor [MapOutput (Just "DP-1")]
        let gen = InputGeneration 0
            ready = updateInput gen Input.RiverInputDeviceV1Done (updateInput gen (Input.RiverInputDeviceV1Type Input.RiverInputDeviceV1TypeEnumPointer) (metadata gen (discoverInput a emptyInputInventory)))
            (missing, none) = planGeneric rules Map.empty ready
            (mapped, requests) = planGeneric rules (Map.singleton "DP-1" out1) missing
            (absent, removedRequests) = planGeneric rules Map.empty mapped
            (_, readded) = planGeneric rules (Map.singleton "DP-1" out2) absent
        assertEqual "absent" [] none
        assertEqual "map current handle" [GenericMapOutput gen (Just out1)] requests
        assertEqual "no request on removed output" [] removedRequests
        assertEqual "readded handle" [GenericMapOutput gen (Just out2)] readded
        assertEqual "same output no duplicate" [] (snd (planGeneric rules (Map.singleton "DP-1" out1) mapped))
    , testCase "generic precedence preserves explicit clears and deferred outcomes" $ do
        a <- object 2
        patternValue <- either (assertFailure . show) pure (compileDevicePattern "*")
        let gen = InputGeneration 0
            rules = [InputRule patternValue Nothing [ScrollFactor 1, MapOutput (Just "DP-1"), MapRectangle (Just (InputRectangle 1 2 3 4))], InputRule patternValue (Just Pointer) [ScrollFactor 2, MapOutput Nothing, MapRectangle Nothing, Tap True]]
            ready = updateInput gen Input.RiverInputDeviceV1Done (updateInput gen (Input.RiverInputDeviceV1Type Input.RiverInputDeviceV1TypeEnumPointer) (metadata gen (discoverInput a emptyInputInventory)))
            (planned, requests) = planGeneric rules Map.empty ready
        assertEqual "last fields and explicit clears" [GenericScroll gen 512, GenericMapOutput gen Nothing, GenericMapRectangle gen (InputRectangle 0 0 0 0)] requests
        assertEqual "deferred libinput unavailable" (Just SettingUnavailable) (settingOutcome <$> (Map.lookup gen (inputDevices planned) >>= Map.lookup TapKey . inputResults))
        let removed = updateInput gen Input.RiverInputDeviceV1Removed planned
        assertEqual "removed device never sends" [] (snd (planGeneric rules Map.empty removed))
        assertEqual "results retired" (Just [SettingRemoved, SettingRemoved, SettingRemoved, SettingRemoved]) (fmap (map settingOutcome . Map.elems . inputResults) (Map.lookup gen (inputDevices removed)))
    , testCase "positive sub-precision scroll is invalid; zero remains explicit" $ do
        assertEqual "rounding to zero" (Left InvalidScrollFactor) (validateInputSetting (ScrollFactor (1 / 1024)))
        assertEqual "tie rounds to zero" (Left InvalidScrollFactor) (validateInputSetting (ScrollFactor (1 / 512)))
        assertEqual "smallest exact positive" (Right ()) (validateInputSetting (ScrollFactor (1 / 256)))
        assertEqual "explicit zero" (Right ()) (validateInputSetting (ScrollFactor 0))
    ]
 where
  object version = do
    (table, _) <- either (assertFailure . show) pure (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
    (_, obj) <- either (assertFailure . show) pure (allocateClientObject Input.riverInputDeviceV1Interface version table)
    pure obj
  metadata gen = updateInput gen (Input.RiverInputDeviceV1Name "fixture") . updateInput gen (Input.RiverInputDeviceV1Type Input.RiverInputDeviceV1TypeEnumKeyboard)
  lifecycle n inventory = maybe (error "missing device") inputLifecycle (Map.lookup (InputGeneration n) (inputDevices inventory))
  rulesFor settings = do
    patternValue <- either (assertFailure . show) pure (compileDevicePattern "*")
    pure [InputRule patternValue Nothing settings]
