{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.LatencyPureTest (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Aeson (eitherDecodeFileStrict', withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.Bits (shiftR)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Seki.Internal.Core.Action (Intent (..))
import Seki.Internal.Core.Event (drainCore)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize)
import Seki.Internal.Core.Layout (SizePolicy (..))
import Seki.Internal.Core.ManageHook (idHook)
import Seki.Internal.Core.PointerOp (OpKind (..), PointerOp (..), SeatId (..), SizeBounds (..), firstOpId)
import Seki.Internal.Core.Scene
import Seki.Internal.Core.Scratchpad.Types (ScratchpadState (..))
import Seki.Internal.Core.StackSet qualified as S
import Seki.Internal.Core.State
import Seki.Latency.Pure
import Seki.Latency.Workload (WorkloadName (..), padName)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "pure latency forcing"
    [ testCase "consumes lazy intent payload" $ throws (forceResult initial [Spawn (error "unforced command")] emptyScene)
    , testCase "consumes scene placement values" $ throws (forceResult initial [] emptyScene{sceneWindows = Map.singleton (S.WindowId 1) (Placed Crop (error "unforced rectangle"))})
    , testCase "consumes complete scene ordering" $ throws (forceResult initial [] emptyScene{sceneStacking = [S.WindowId 1, error "unforced tail"]})
    , testCase "consumes hidden workspace tags" $ case coreWorkspaces initial of
        AttachedState ss os -> throws (forceResult (fromWorkspaces (AttachedState ss{S.hidden = [S.Workspace (error "unforced tag") layout Nothing]} os)) [] emptyScene)
        _ -> assertFailure "unexpected headless fixture"
    , testCase "consumes a placed tile's size policy" $ throws (forceResult initial [] emptyScene{sceneWindows = Map.singleton (S.WindowId 1) (Placed (error "unforced policy") rect)})
    , testCase "consumes the fullscreen set" $ throws (forceResult initial{coreFullscreen = Set.fromList [S.WindowId 1, error "unforced fullscreen window"]} [] emptyScene)
    , testCase "consumes every pointer op field" $ throws (forceResult initial{corePointerOp = Just op{opBounds = (opBounds op){boundsMaxHeight = error "unforced bound"}}} [] emptyScene)
    , testCase "consumes the next op id" $ throws (forceResult initial{coreNextOpId = error "unforced op id"} [] emptyScene)
    , testCase "consumes the scene's pointer op" $ throws (forceResult initial [] emptyScene{scenePointerOp = Just (SeatId 1, error "unforced scene op")})
    , testCase "rejects missing work and missing scene membership" $ do
        assertBool "empty result cannot claim 32 windows" (not (validateResult 32 (initialState M1 2) emptyScene))
    , testCase "rejects a fullscreen window that does not focus its workspace" $ do
        let st = initialState M2 8
        assertBool "the generated state is valid" (validateResult 8 st (planFor M2 8 st))
        let unfocused = st{coreFullscreen = Set.singleton (S.WindowId 1)}
        assertBool "window 1 is not focused" (not (validateResult 8 unfocused (planFor M2 8 unfocused)))
    , testCase "checksum changes when scene focus changes" $
        assertBool "focus contributes to retained digest" (forceResult initial [] emptyScene /= forceResult initial [] emptyScene{sceneFocus = Just (S.WindowId 1)})
    , testCase "m2 starts with its two newest windows as client-sized floats" $ case coreWorkspaces (initialState M2 8) of
        AttachedState ss _ -> S.floating ss @?= Map.fromList [(S.WindowId 7, S.ClientSized), (S.WindowId 8, S.ClientSized)]
        _ -> assertFailure "unexpected headless fixture"
    , testCase "m2 dialogs stack over their parent" $ do
        let st = initialState M2 8
        planParents M2 8 @?= Map.fromList [(S.WindowId 7, Just (S.WindowId 1)), (S.WindowId 8, Just (S.WindowId 1))]
        take 2 (reverse (sceneStacking (planFor M2 8 st))) @?= [S.WindowId 8, S.WindowId 7]
    , testCase "m2 operations toggle and request fullscreen, raise a dialog, and float a tile" $ do
        let run codes = fst (drainCore idHook (map (eventFor M2 8) codes) (initialState M2 8))
            lowerDialog = run [18, 16]
        coreFullscreen (run [16]) @?= Set.singleton (S.WindowId 8)
        coreRaiseOrder lowerDialog @?= [S.WindowId 8, S.WindowId 7]
        coreFullscreen lowerDialog @?= Set.singleton (S.WindowId 7)
        coreFullscreen (run [17]) @?= Set.empty
        case coreWorkspaces (run [19]) of
          AttachedState ss _ -> Map.lookup (S.WindowId 3) (S.floating ss) @?= Just S.ClientSized
          _ -> assertFailure "unexpected headless fixture"
    , testCase "m1 keeps its sixteen operation codes, m2 adds four, and m3 two more" $ do
        operationCodes M1 1909 200 @?= [fromIntegral ((x `shiftR` 32) `mod` 16) | x <- take 200 (tail (iterate lcg 1909))]
        operationCodes M2 1909 200 @?= [fromIntegral ((x `shiftR` 32) `mod` 20) | x <- take 200 (tail (iterate lcg 1909))]
        operationCodes M3 1909 200 @?= [fromIntegral ((x `shiftR` 32) `mod` 22) | x <- take 200 (drop 1 (iterate lcg 1909))]
    , testCase "m3 starts as m2 with a hidden pad outside every workspace" $ do
        let st = initialState M3 8
        Map.keys (coreScratchpads st) @?= [padName]
        fmap padWindow (Map.lookup padName (coreScratchpads st)) @?= Just (S.WindowId 9, False)
        coreWorkspaces st @?= coreWorkspaces (initialState M2 8)
        assertBool "the hidden pad leaves the m2 invariants" (validateResult 8 st (planFor M3 8 st))
    , testCase "m3 toggles the pad into the workspace and out, and requests a restart" $ do
        let run codes = drainCore idHook (map (eventFor M3 8) codes) (initialState M3 8)
            shown = fst (run [20])
        fmap padWindow (Map.lookup padName (coreScratchpads shown)) @?= Just (S.WindowId 9, True)
        assertBool "a shown pad is a managed window" (validateResult 8 shown (planFor M3 8 shown))
        fmap padWindow (Map.lookup padName (coreScratchpads (fst (run [20, 20])))) @?= Just (S.WindowId 9, False)
        snd (run [21]) @?= [Restart]
        fst (run [16]) @?= fst (drainCore idHook [eventFor M2 8 16] (initialState M3 8))
    , testCase "a pad missing from the workspaces fails validation" $ do
        let shown = fst (drainCore idHook [eventFor M3 8 20] (initialState M3 8))
            lost = shown{coreScratchpads = Map.insert padName (ShownPad (S.WindowId 10) Nothing) (coreScratchpads shown)}
        assertBool "the pad's window is not managed" (not (validateResult 8 lost (planFor M3 8 lost)))
    , testCase "m2 keeps trunk c2b7851's pure checksum over 512 steps of seed 1909 on 32 windows" $
        withSystemTempDirectory "seki-pure" $ \directory -> do
          runPure (directory </> "pure.json") M2 1909 32 512
          decoded <- eitherDecodeFileStrict' (directory </> "pure.json")
          checksum <- either fail pure (decoded >>= parseEither (withObject "pure" (.: "checksum")))
          checksum @?= (16867668497154443398 :: Word64)
    , testCase "the m3 digest consumes the pad state" $
        throws (forceResult (initialState M3 8){coreScratchpads = Map.singleton padName (ShownPad (error "unforced pad") Nothing)} [] emptyScene)
    ]
 where
  initial = initialState M1 8
  emptyScene = DesiredScene Map.empty [] [] [] Nothing Nothing Nothing
  rect = either (error . show) id (mkSize 10 10 >>= mkRect (Point 0 0))
  lcg :: Word64 -> Word64
  lcg x = x * 6364136223846793005 + 1442695040888963407
  op = PointerOp firstOpId (SeatId 1) (S.WindowId 1) Move (OutputRect rect) (SizeBounds 1 1 100 100) True
  padWindow (ShownPad w _) = (w, True)
  padWindow (HiddenPad w _) = (w, False)
  padWindow (LaunchPending _) = (S.WindowId 0, False)
  throws value = do
    result <- try @ErrorCall (evaluate value)
    case result of Left _ -> pure (); Right _ -> assertFailure "deep value was not forced"
