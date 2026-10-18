{-# LANGUAGE ScopedTypeVariables #-}

module Seki.Internal.Core.SceneTest (tests) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Seki.Internal.Core.Geometry (Point (..), Rect, mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.GeometryGen (genRatio, genRect)
import Seki.Internal.Core.Layout (CLR (..), Choose (..), LayoutClass (..), Mirror (..), runLayout)
import Seki.Internal.Core.Scene (DesiredScene (..), Placement (..), SceneStyle (..), defaultSceneStyle, planScene, stackWindows)
import Seki.Internal.Core.StackSet (OutputId (..), Screen (..), Stack (..), StackSet (..), WindowId (..), Workspace (..), integrate)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..))
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (NonNegative (..), chooseInt, conjoin, forAll, property, sublistOf, testProperty, (===))

rectangle :: Int -> Int -> Rect
rectangle w h = either (error . show) id (mkSize (fromIntegral w) (fromIntegral h) >>= mkRect (Point (-100) 20))
w1, w2, w3, w4 :: WindowId
w1 = WindowId 1
w2 = WindowId 2
w3 = WindowId 3
w4 = WindowId 4
state :: l -> Stack WindowId -> CoreState Int l
state l s = AttachedState (StackSet (Screen (Workspace 1 l (Just s)) (OutputId 1)) [] [Workspace 2 l (Just (Stack w4 [] []))] Map.empty) []
scene :: (LayoutClass l) => l -> Stack WindowId -> DesiredScene
scene l s = planScene defaultSceneStyle (state l s) (Map.singleton (OutputId 1) (rectangle 101 81)) Map.empty

tests :: TestTree
tests =
  testGroup
    "desired scene"
    [ testCase "Full selects only focus and insets outside borders" $ do
        let result = scene Full (Stack w2 [w1] [w3])
        sceneWindows result @?= Map.fromList [(w1, Omitted), (w2, Placed content), (w3, Omitted), (w4, Omitted)]
        sceneFocus result @?= Just w2
        sceneStacking result @?= [w1, w3, w4, w2]
    , testCase "missing usable area and headless retain all omitted windows" $ do
        let st = state Full (Stack w1 [] [w2])
            result = planScene defaultSceneStyle st Map.empty Map.empty
        sceneWindows result @?= Map.fromList [(w1, Omitted), (w2, Omitted), (w4, Omitted)]
        sceneFocus result @?= Nothing
        let hw = HeadlessWorkspaces (Workspace (1 :: Int) Full Nothing) [] Map.empty
        sceneWindows (planScene defaultSceneStyle (HeadlessState hw{headlessCurrent = Workspace 1 Full (Just (Stack w1 [] []))}) Map.empty Map.empty) @?= Map.singleton w1 Omitted
    , testCase "parents and focused sibling branch are complete and stable" $ do
        let w5 = WindowId 5
            st = state (Tall 1 0.03 0.5) (Stack w2 [w1] [w5, w3])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) (rectangle 100 100)) (Map.fromList [(w2, Just w1), (w3, Just w1)])
        sceneStacking result @?= [w4, w5, w1, w3, w2]
    , testProperty "every parent graph, including cycles, stacks every window exactly once" $ \(a :: NonNegative Int) (b :: NonNegative Int) (c :: NonNegative Int) ->
        let parentChoice (NonNegative n) = [Nothing, Just w1, Just w2, Just w3] !! (n `mod` 4)
            result = planScene defaultSceneStyle (state (Tall 1 0.03 0.5) (Stack w2 [w1] [w3])) (Map.singleton (OutputId 1) (rectangle 100 100)) (Map.fromList (zip [w1, w2, w3] (map parentChoice [a, b, c])))
         in sort (sceneStacking result) === [w1, w2, w3, w4]
    , testCase "a pending parent does not group its shown descendants" $
        stackWindows [w1, w2, w3, w4] [w2, w3, w4] (Just w3) (Map.fromList [(w2, Just w1), (w3, Just w2)])
          @?= [w1, w4, w2, w3]
    , testProperty "actual shown subsets keep a complete order with every hidden node below" $ \(a :: NonNegative Int) (b :: NonNegative Int) (c :: NonNegative Int) ->
        forAll (sublistOf [w1, w2, w3, w4]) $ \shown ->
          let parentChoice (NonNegative n) = [Nothing, Just w1, Just w2, Just w3] !! (n `mod` 4)
              parents = Map.fromList (zip [w1, w2, w3] (map parentChoice [a, b, c]))
              ordered = stackWindows [w1, w2, w3, w4] shown (Just w2) parents
              hidden = [w | w <- [w1, w2, w3, w4], w `notElem` shown]
           in conjoin [sort ordered === [w1, w2, w3, w4], take (length hidden) ordered === hidden, sort (drop (length hidden) ordered) === sort shown]
    , testProperty "zero or border-sized tiles are omitted without losing windows" $ \(NonNegative n) ->
        let extent = n `mod` 5
            result = planScene defaultSceneStyle (state Full (Stack w1 [] [w2])) (Map.singleton (OutputId 1) (rectangle extent 100)) Map.empty
         in sceneWindows result === Map.fromList [(w1, Omitted), (w2, Omitted), (w4, Omitted)]
    , testProperty "multi-output scenes keep positive content and borders inside tiles with a placed focus" $ \(NonNegative masters) ->
        forAll ((,,,) <$> genRect <*> genRect <*> chooseInt (0, 20) <*> chooseInt (0, 2)) $ \(area1, area2, border, choice) ->
          forAll genRatio $ \ratio ->
            forAll (sublistOf [OutputId 1, OutputId 2]) $ \available ->
              let tall = Tall (masters `mod` 5) (1 / 10) ratio
                  selected = Choose (if choice == 0 then CL else CR) tall (Choose (if choice == 1 then CL else CR) (Mirror tall) Full)
                  w5 = WindowId 5
                  w6 = WindowId 6
                  first = Screen (Workspace (1 :: Int) selected (Just (Stack w2 [w1] [w3]))) (OutputId 1)
                  second = Screen (Workspace 2 selected (Just (Stack w4 [] [w5]))) (OutputId 2)
                  st = AttachedState (StackSet first [second] [Workspace 3 selected (Just (Stack w6 [] []))] Map.empty) []
                  areas = Map.fromList [(o, area) | (o, area) <- [(OutputId 1, area1), (OutputId 2, area2)], o `elem` available]
                  style = defaultSceneStyle{sceneBorderWidth = fromIntegral border}
                  result = planScene style st areas Map.empty
                  tiles = Map.fromList [(w, tile) | s <- [first, second], Just area <- [Map.lookup (output s) areas], (w, tile) <- runLayout (workspace s) area]
                  placed = [(w, r) | (w, Placed r) <- Map.toList (sceneWindows result)]
                  containsBorders (w, r) = case Map.lookup w tiles of
                    Nothing -> False
                    Just tile ->
                      rectWidth r > 0
                        && rectHeight r > 0
                        && toInteger (rectX r) - toInteger border == toInteger (rectX tile)
                        && toInteger (rectY r) - toInteger border == toInteger (rectY tile)
                        && toInteger (rectWidth r) + 2 * toInteger border == toInteger (rectWidth tile)
                        && toInteger (rectHeight r) + 2 * toInteger border == toInteger (rectHeight tile)
                  focusPlaced = case sceneFocus result of
                    Nothing -> True
                    Just w -> case Map.lookup w (sceneWindows result) of
                      Just Placed{} -> True
                      _ -> False
               in conjoin
                    [ property (all containsBorders placed)
                    , property focusPlaced
                    , sort (map fst placed) === [w | (w, tile) <- Map.toList tiles, toInteger (rectWidth tile) > 2 * toInteger border, toInteger (rectHeight tile) > 2 * toInteger border]
                    , Map.lookup w6 (sceneWindows result) === Just Omitted
                    , Map.keys (sceneWindows result) === [w1, w2, w3, w4, w5, w6]
                    , sort (sceneStacking result) === [w1, w2, w3, w4, w5, w6]
                    ]
    , testCase "a custom layout omitting windows hides them" $ do
        let result = scene MasterOnly (Stack w2 [w1] [w3])
        sceneWindows result @?= Map.fromList [(w1, Placed content), (w2, Omitted), (w3, Omitted), (w4, Omitted)]
        sceneFocus result @?= Nothing
        sceneStacking result @?= [w2, w3, w4, w1]
    , testCase "focusing a parent raises its child and grandchild above unrelated windows" $ do
        let w5 = WindowId 5
            w6 = WindowId 6
            st = state (Tall 1 0.03 0.5) (Stack w1 [w5] [w2, w6, w3])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) (rectangle 100 100)) (Map.fromList [(w2, Just w1), (w3, Just w2)])
        sceneStacking result @?= [w4, w5, w6, w1, w2, w3]
    , testCase "a parent on another workspace leaves its child a family of its own" $ do
        let st = state (Tall 1 0.03 0.5) (Stack w1 [] [w2, w3])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) (rectangle 100 100)) (Map.fromList [(w2, Just w4), (w3, Just w2)])
        sceneStacking result @?= [w4, w2, w3, w1]
    ]
 where
  content = either (error . show) id (mkSize 97 77 >>= mkRect (Point (-98) 22))

data MasterOnly = MasterOnly deriving (Show)

instance LayoutClass MasterOnly where
  pureLayout _ r s = [(w, r) | w <- take 1 (integrate s)]
