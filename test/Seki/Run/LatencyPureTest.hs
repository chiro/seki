module Seki.Run.LatencyPureTest (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Map.Strict qualified as Map
import Seki.Internal.Core.Action (Intent (..))
import Seki.Internal.Core.Layout (SizePolicy (..))
import Seki.Internal.Core.Scene
import Seki.Internal.Core.StackSet qualified as S
import Seki.Internal.Core.State
import Seki.Latency.Pure
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
    , testCase "rejects missing work and missing scene membership" $ do
        assertBool "empty result cannot claim 32 windows" (not (validateResult 32 (initialState 2) emptyScene))
    , testCase "checksum changes when scene focus changes" $
        assertBool "focus contributes to retained digest" (forceResult initial [] emptyScene /= forceResult initial [] emptyScene{sceneFocus = Just (S.WindowId 1)})
    ]
 where
  initial = initialState 8
  emptyScene = DesiredScene Map.empty [] [] [] Nothing Nothing Nothing
  throws value = do
    result <- try @ErrorCall (evaluate value)
    case result of Left _ -> pure (); Right _ -> assertFailure "deep value was not forced"
