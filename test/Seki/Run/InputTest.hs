{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.InputTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Seki.Internal.Input
import Seki.Protocol.RiverInputManagementV1 qualified as Input
import Seki.Protocol.RiverLibinputConfigV1 qualified as L
import Seki.Protocol.RiverXkbConfigV1 qualified as X
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.External
import Seki.Run.Input
import Seki.Run.Keymap
import Seki.Run.Libinput
import Seki.Run.XkbConfig
import Seki.Wire.Connection.ObjectTable
import Seki.Wire.Transport (closeOwnedFd, withOwnedFd)
import System.Directory (removeFile)
import System.IO (hClose, hPutStr, openTempFile)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "runtime input"
    [ testCase "XKB batches layout names/index and locks independently" $ do
        let initial = emptyXkbFacet FacetDiscovering
            staged = stageXkb (X.RiverXkbKeyboardV1Layout 1 (Just "Japanese")) (stageXkb X.RiverXkbKeyboardV1CapslockEnabled initial)
        assertEqual "initial current unavailable" Nothing (xkbLayout (xkbCurrent staged))
        assertEqual "generic discovery cannot commit XKB" FacetDiscovering (xkbLifecycle staged)
        let ready = commitXkb staged
        assertEqual "reported name and index" (Just (1, Just "Japanese")) (xkbLayout (xkbCurrent ready))
        assertEqual "caps observed" (Just True) (xkbCapsLock (xkbCurrent ready))
        assertEqual "unreported lock stays unknown" Nothing (xkbNumLock (xkbCurrent ready))
    , testCase "XKB planner gates scrolllock by negotiated version and preserves other facets" $ do
        generic <- object 2
        (table, _) <- either (assertFailure . show) pure (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
        (_, keyboard) <- either (assertFailure . show) pure (allocateClientObject X.riverXkbKeyboardV1Interface 2 table)
        rules <- rulesFor [ActiveLayout (LayoutByName "Japanese"), CapsLock True, NumLock False, ScrollLock True, RepeatInfo 37 410]
        let gen = InputGeneration 0
            base = updateInput gen Input.RiverInputDeviceV1Done (metadata gen (discoverInput generic emptyInputInventory))
            ready = base{inputDevices = Map.adjust (\d -> d{inputXkb = (emptyXkbFacet FacetReady){xkbObject = Just keyboard, xkbGeneration = Just (FacetGeneration 0)}}) gen (inputDevices base)}
            (planned, requests) = planInput rules Map.empty ready
            outcome key = settingOutcome <$> (Map.lookup gen (inputDevices planned) >>= Map.lookup key . inputResults)
        assertEqual "older scrolllock unsupported" (Just SettingUnsupported) (outcome ScrollLockKey)
        assertEqual "layout planned" (Just SettingPending) (outcome ActiveLayoutKey)
        assertEqual "generic still planned" [GenericRepeat gen 37 410] requests
        assertEqual "no invented defaults" (Just Map.empty) (inputDefaults <$> Map.lookup gen (inputDevices planned))
    , testCase "keymap helper snapshots immutable bounded contents and closes idempotently" $
        bracket (openTempFile "/tmp" "seki-keymap-test") (\(path, _) -> removeFile path) $ \(path, handle) -> do
          hPutStr handle "xkb_keymap {};"
          hClose handle
          bracket newKeymapWorker closeKeymapWorker $ \worker -> do
            _ <- bindExternalWake (keymapWake worker) (pure True)
            admitted <- enqueueKeymap worker (1 :: Int) path
            assertBool "admitted" admitted
            let await = drainKeymaps worker >>= \caseValue -> case caseValue of [] -> threadDelay 1000 >> await; value -> pure value
            completed <- timeout 3000000 await
            case completed of
              Just [(1, Just fd)] -> do
                writeFile path "changed"
                bytes <- withOwnedFd fd (\raw -> fdRead raw 100)
                assertEqual "source mutation cannot change sealed snapshot" (Right "xkb_keymap {};\0") bytes
                rejected <- withOwnedFd fd (\raw -> try @IOException (fdWrite raw "mutation"))
                assertBool "snapshot sealed against writes" (case rejected of Right (Left _) -> True; _ -> False)
                assertBool "close once" =<< closeOwnedFd fd
                assertBool "second close no ownership" . not =<< closeOwnedFd fd
              other -> assertFailure ("missing helper result: " <> show (fmap (map fst) other))
    , testCase "keymap loader rejects missing, directory, empty, malformed UTF8 and embedded NUL" $
        bracket (openTempFile "/tmp" "seki-keymap-invalid") (\(path, _) -> removeFile path) $ \(path, handle) -> do
          hClose handle
          bracket newKeymapWorker closeKeymapWorker $ \worker -> do
            _ <- bindExternalWake (keymapWake worker) (pure True)
            let check file = do
                  admitted <- enqueueKeymap worker () file
                  assertBool "admitted" admitted
                  let await = drainKeymaps worker >>= \items -> case items of [] -> threadDelay 1000 >> await; value -> pure value
                  result <- timeout 3000000 await
                  case result of Just [((), Nothing)] -> pure (); _ -> assertFailure "invalid input returned a resource or hung"
            check (path <> ".missing")
            check "/tmp"
            mapM_ (\bytes -> BS.writeFile path bytes >> check path) [BS.empty, BS.pack [0xc0, 0x80], BS.pack [0xed, 0xa0, 0x80], BS.pack [0, 65], BS.replicate (4 * 1024 * 1024 + 1) 65]
            closeKeymapWorker worker
            assertEqual "closed admission" False =<< enqueueKeymap worker () path
            assertBool "closed completions empty" . null =<< drainKeymaps worker
    , testCase "libinput native float/double payloads reject malformed lengths and values" $ do
        assertEqual "six native floats" (Right [1, 0, 0, 0, 1, 0]) (decodeFloats (encodeFloats [1, 0, 0, 0, 1, 0]))
        assertEqual "one native double" (Right 0.25) (decodeDouble (encodeDoubles [0.25]))
        mapM_ (\n -> assertBool "wrong matrix length" (either (const True) (const False) (decodeFloats (BS.replicate n 0)))) [0, 8, 23, 25, 48]
        mapM_ (\n -> assertBool "wrong speed length" (either (const True) (const False) (decodeDouble (BS.replicate n 0)))) [0, 4, 7, 9, 16]
        assertBool "NaN rejected" (either (const True) (const False) (decodeDouble (encodeDoubles [0 / 0])))
        assertBool "Infinity rejected" (either (const True) (const False) (decodeFloats (encodeFloats [1, 0, 0, 0, 1, 1 / 0])))
        assertEqual "oversize curve" (Left InvalidCurve) (validateInputSetting (Acceleration (AccelerationCurves [AccelCurve CurveMotion 1 (replicate 4097 0)])))
        assertEqual "duplicate curve" (Left InvalidCurve) (validateInputSetting (Acceleration (AccelerationCurves [AccelCurve CurveMotion 1 [0, 1], AccelCurve CurveMotion 2 [1, 2]])))
    , testCase "libinput support and unknown wire values never invent defaults" $ do
        let initial = emptyFacet FacetDiscovering
            supported = updateFacet (L.RiverLibinputDeviceV1TapSupport 0) initial
            unknown = updateFacet (L.RiverLibinputDeviceV1TapCurrent (L.RiverLibinputDeviceV1TapStateEnum 17)) initial
            mask = updateFacet (L.RiverLibinputDeviceV1ScrollMethodSupport (L.RiverLibinputDeviceV1ScrollMethodsFlags 128)) initial
        assertEqual "no support event yet" Nothing (supportsSetting initial (Tap True))
        assertEqual "explicit unsupported" (Just False) (supportsSetting supported (Tap True))
        assertEqual "not readied by support" FacetDiscovering (facetLifecycle supported)
        assertBool "unknown enum diagnostic retained" (not (null (facetDiagnostics unknown)))
        assertEqual "unknown current not converted to disabled" Map.empty (facetCurrent unknown)
        assertEqual "unknown mask not supported" Nothing (supportsSetting mask (ScrollMethod ScrollOnButton))
    , testCase "libinput acknowledgements reject superseded and replaced generations" $ do
        generic <- object 2
        (table, _) <- either (assertFailure . show) pure (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
        (_, lib) <- either (assertFailure . show) pure (allocateClientObject L.riverLibinputDeviceV1Interface 2 table)
        let gen = InputGeneration 0
            old = SettingResult (Tap True) 0 Nothing (Just (objectId lib)) SettingAwaiting
            current = old{settingDesired = Tap False, settingRevision = 1}
            base = metadata gen (discoverInput generic emptyInputInventory)
            ready = updateInput gen Input.RiverInputDeviceV1Done base
            configured = ready{inputDevices = Map.adjust (\d -> d{inputLibinput = (emptyFacet FacetReady){facetObject = Just lib, facetSupport = Map.singleton TapKey (Right 3)}, inputResults = Map.singleton TapKey current}) gen (inputDevices ready)}
            stale = acknowledgeLibinput gen lib old SettingApplied configured
            applied = acknowledgeLibinput gen lib current SettingApplied stale
        let discovering = configured{inputDevices = Map.adjust (\d -> d{inputLibinput = (inputLibinput d){facetLifecycle = FacetDiscovering, facetGeneration = Just (FacetGeneration 1)}}) gen (inputDevices configured)}
        assertEqual "old v1 cohort cannot ready replacement facet" (Just FacetDiscovering) (facetLifecycle . inputLibinput <$> Map.lookup gen (inputDevices (finishFacetDiscovery lib (FacetGeneration 0) discovering)))
        assertEqual "own v1 cohort readies facet" (Just FacetReady) (facetLifecycle . inputLibinput <$> Map.lookup gen (inputDevices (finishFacetDiscovery lib (FacetGeneration 1) discovering)))
        assertEqual "removed child cannot revive at barrier" (Just Removed) (inputLifecycle <$> Map.lookup gen (inputDevices (finishFacetDiscovery lib (FacetGeneration 1) (updateInput gen Input.RiverInputDeviceV1Removed discovering))))
        assertEqual "old ack cannot overwrite replacement desired" (Just current) (Map.lookup gen (inputDevices stale) >>= Map.lookup TapKey . inputResults)
        assertEqual "real matching success applies" (Just SettingApplied) (settingOutcome <$> (Map.lookup gen (inputDevices applied) >>= Map.lookup TapKey . inputResults))
        assertEqual "generic refresh preserves applied" (Just SettingApplied) (settingOutcome <$> (Map.lookup gen (inputDevices (fst (planInput [InputRule (either (error . show) id (compileDevicePattern "*")) Nothing [Tap False]] Map.empty applied))) >>= Map.lookup TapKey . inputResults))
    , testCase "v1 captured callback excludes later hotplug and removed children" $ do
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
        assertEqual "no sends before done" [] (snd (planInput rules Map.empty initial))
        let ready = updateInput (InputGeneration 0) Input.RiverInputDeviceV1Done initial
            (sent, requests) = planInput rules Map.empty ready
        assertEqual "one request" [GenericRepeat (InputGeneration 0) 37 410] requests
        assertEqual "no feedback resend" [] (snd (planInput rules Map.empty (updateInput (InputGeneration 0) Input.RiverInputDeviceV1Done sent)))
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
            (missing, none) = planInput rules Map.empty ready
            (mapped, requests) = planInput rules (Map.singleton "DP-1" out1) missing
            (absent, removedRequests) = planInput rules Map.empty mapped
            (_, readded) = planInput rules (Map.singleton "DP-1" out2) absent
        assertEqual "absent" [] none
        assertEqual "map current handle" [GenericMapOutput gen (Just out1)] requests
        assertEqual "no request on removed output" [] removedRequests
        assertEqual "readded handle" [GenericMapOutput gen (Just out2)] readded
        assertEqual "same output no duplicate" [] (snd (planInput rules (Map.singleton "DP-1" out1) mapped))
    , testCase "generic precedence preserves explicit clears and deferred outcomes" $ do
        a <- object 2
        patternValue <- either (assertFailure . show) pure (compileDevicePattern "*")
        let gen = InputGeneration 0
            rules = [InputRule patternValue Nothing [ScrollFactor 1, MapOutput (Just "DP-1"), MapRectangle (Just (InputRectangle 1 2 3 4))], InputRule patternValue (Just Pointer) [ScrollFactor 2, MapOutput Nothing, MapRectangle Nothing, Tap True]]
            ready = updateInput gen Input.RiverInputDeviceV1Done (updateInput gen (Input.RiverInputDeviceV1Type Input.RiverInputDeviceV1TypeEnumPointer) (metadata gen (discoverInput a emptyInputInventory)))
            (planned, requests) = planInput rules Map.empty ready
        assertEqual "last fields and explicit clears" [GenericScroll gen 512, GenericMapOutput gen Nothing, GenericMapRectangle gen (InputRectangle 0 0 0 0)] requests
        assertEqual "deferred libinput unavailable" (Just SettingUnavailable) (settingOutcome <$> (Map.lookup gen (inputDevices planned) >>= Map.lookup TapKey . inputResults))
        let removed = updateInput gen Input.RiverInputDeviceV1Removed planned
        assertEqual "removed device never sends" [] (snd (planInput rules Map.empty removed))
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
