{-# LANGUAGE ScopedTypeVariables #-}

module Seki.Internal.Core.SceneTest (tests) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Seki.Internal.Core.Event (CoreEvent (..), drainCore)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), Rect, mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.GeometryGen (genRatio, genRect)
import Seki.Internal.Core.Layout (CLR (..), Choose (..), LayoutClass (..), Mirror (..), SizePolicy (..), runLayout, tolerant, (|||))
import Seki.Internal.Core.ManageHook (idHook)
import Seki.Internal.Core.Scene (DesiredScene (..), OutputArea (..), Placement (..), SceneStyle (..), defaultSceneStyle, planScene, stackWindows)
import Seki.Internal.Core.StackSet (FloatGeom (..), OutputId (..), Screen (..), Stack (..), StackSet (..), WindowId (..), Workspace (..), integrate)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..), fromWorkspaces)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (NonNegative (..), chooseInt, conjoin, counterexample, forAll, property, sublistOf, testProperty, vectorOf, (===))

rectangle :: Int -> Int -> Rect
rectangle w h = either (error . show) id (mkSize (fromIntegral w) (fromIntegral h) >>= mkRect (Point (-100) 20))
w1, w2, w3, w4 :: WindowId
w1 = WindowId 1
w2 = WindowId 2
w3 = WindowId 3
w4 = WindowId 4
state :: l -> Stack WindowId -> CoreState Int l
state l s = fromWorkspaces (AttachedState (StackSet (Screen (Workspace 1 l (Just s)) (OutputId 1)) [] [Workspace 2 l (Just (Stack w4 [] []))] Map.empty) [])
scene :: (LayoutClass l) => l -> Stack WindowId -> DesiredScene
scene l s = planScene defaultSceneStyle (state l s) (Map.singleton (OutputId 1) (whole (rectangle 101 81))) Map.empty

tests :: TestTree
tests =
  testGroup
    "desired scene"
    [ testCase "Full selects only focus and insets outside borders" $ do
        let result = scene Full (Stack w2 [w1] [w3])
        sceneWindows result @?= Map.fromList [(w1, Omitted), (w2, Placed Crop content), (w3, Omitted), (w4, Omitted)]
        sceneFocus result @?= Just w2
        sceneStacking result @?= [w1, w3, w4, w2]
    , testCase "missing usable area and headless retain all omitted windows" $ do
        let st = state Full (Stack w1 [] [w2])
            result = planScene defaultSceneStyle st Map.empty Map.empty
        sceneWindows result @?= Map.fromList [(w1, Omitted), (w2, Omitted), (w4, Omitted)]
        sceneFocus result @?= Nothing
        let hw = HeadlessWorkspaces (Workspace (1 :: Int) Full Nothing) [] Map.empty
        sceneWindows (planScene defaultSceneStyle (fromWorkspaces (HeadlessState hw{headlessCurrent = Workspace 1 Full (Just (Stack w1 [] []))})) Map.empty Map.empty) @?= Map.singleton w1 Omitted
    , testCase "parents and focused sibling branch are complete and stable" $ do
        let w5 = WindowId 5
            st = state (Tall 1 0.03 0.5) (Stack w2 [w1] [w5, w3])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) (whole (rectangle 100 100))) (Map.fromList [(w2, Just w1), (w3, Just w1)])
        sceneStacking result @?= [w4, w5, w1, w3, w2]
    , testProperty "every parent graph, including cycles, stacks every window exactly once" $ \(a :: NonNegative Int) (b :: NonNegative Int) (c :: NonNegative Int) ->
        let parentChoice (NonNegative n) = [Nothing, Just w1, Just w2, Just w3] !! (n `mod` 4)
            result = planScene defaultSceneStyle (state (Tall 1 0.03 0.5) (Stack w2 [w1] [w3])) (Map.singleton (OutputId 1) (whole (rectangle 100 100))) (Map.fromList (zip [w1, w2, w3] (map parentChoice [a, b, c])))
         in sort (sceneStacking result) === [w1, w2, w3, w4]
    , testCase "a pending parent does not group its shown descendants" $
        stackWindows [w1, w2, w3, w4] [] [] [w2, w3, w4] (Just w3) (Map.fromList [(w2, Just w1), (w3, Just w2)])
          @?= [w1, w4, w2, w3]
    , testProperty "actual shown subsets keep a complete order with every hidden node below" $ \(a :: NonNegative Int) (b :: NonNegative Int) (c :: NonNegative Int) ->
        forAll (sublistOf [w1, w2, w3, w4]) $ \shown ->
          let parentChoice (NonNegative n) = [Nothing, Just w1, Just w2, Just w3] !! (n `mod` 4)
              parents = Map.fromList (zip [w1, w2, w3] (map parentChoice [a, b, c]))
              ordered = stackWindows [w1, w2, w3, w4] [] [] shown (Just w2) parents
              hidden = [w | w <- [w1, w2, w3, w4], w `notElem` shown]
           in conjoin [sort ordered === [w1, w2, w3, w4], take (length hidden) ordered === hidden, sort (drop (length hidden) ordered) === sort shown]
    , testProperty "zero or border-sized tiles are omitted without losing windows" $ \(NonNegative n) ->
        let extent = n `mod` 5
            result = planScene defaultSceneStyle (state Full (Stack w1 [] [w2])) (Map.singleton (OutputId 1) (whole (rectangle extent 100))) Map.empty
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
                  st = fromWorkspaces (AttachedState (StackSet first [second] [Workspace 3 selected (Just (Stack w6 [] []))] Map.empty) [])
                  areas = Map.fromList [(o, whole area) | (o, area) <- [(OutputId 1, area1), (OutputId 2, area2)], o `elem` available]
                  style = defaultSceneStyle{sceneBorderWidth = fromIntegral border}
                  result = planScene style st areas Map.empty
                  tiles = Map.fromList [(w, tile) | s <- [first, second], Just area <- [Map.lookup (output s) areas], (w, tile) <- runLayout (workspace s) (outputUsable area)]
                  placed = [(w, r) | (w, Placed _ r) <- Map.toList (sceneWindows result)]
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
        sceneWindows result @?= Map.fromList [(w1, Placed Crop content), (w2, Omitted), (w3, Omitted), (w4, Omitted)]
        sceneFocus result @?= Nothing
        sceneStacking result @?= [w2, w3, w4, w1]
    , testCase "focusing a parent raises its child and grandchild above unrelated windows" $ do
        let w5 = WindowId 5
            w6 = WindowId 6
            st = state (Tall 1 0.03 0.5) (Stack w1 [w5] [w2, w6, w3])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) (whole (rectangle 100 100))) (Map.fromList [(w2, Just w1), (w3, Just w2)])
        sceneStacking result @?= [w4, w5, w6, w1, w2, w3]
    , testCase "a parent on another workspace leaves its child a family of its own" $ do
        let st = state (Tall 1 0.03 0.5) (Stack w1 [] [w2, w3])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) (whole (rectangle 100 100))) (Map.fromList [(w2, Just w4), (w3, Just w2)])
        sceneStacking result @?= [w4, w2, w3, w1]
    , testCase "a child of a closed parent starts its own family" $ do
        let st = state (Tall 1 0.03 0.5) (Stack w2 [w1] [w3])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) (whole (rectangle 100 100))) (Map.singleton w2 (Just (WindowId 9)))
        sceneStacking result @?= [w4, w1, w3, w2]
    , testCase "one-pixel Tall and Mirror Tall scenes omit every zero tile" $
        forM_ [CL, CR] $ \side -> do
          let tall = Tall 1 (1 / 10) (1 / 2)
              st = state (Choose side tall (Mirror tall)) (Stack w1 [] [w2, w3])
              result = planScene defaultSceneStyle{sceneBorderWidth = 0} st (Map.singleton (OutputId 1) (whole (rectangle 1 1))) Map.empty
          sceneWindows result @?= Map.fromList [(w1, Omitted), (w2, Omitted), (w3, Placed Crop (rectangle 1 1)), (w4, Omitted)]
          sceneFocus result @?= Nothing
    , floatTests
    , fullscreenTests
    , testCase "M2-D04: each workspace's tiles take its layout's size policy, and floats and fullscreen windows take none" $ do
        let layouts = Full ||| tolerant Full
            w5 = WindowId 5
            st =
              (fromWorkspaces (AttachedState (StackSet (Screen (Workspace (1 :: Int) layouts (Just (Stack w1 [] [w2, w3]))) (OutputId 1)) [Screen (Workspace 2 (Choose CR Full (tolerant Full)) (Just (Stack w4 [] [w5]))) (OutputId 2)] [] (Map.singleton w2 (FloatAt (OutputRect (rectAt 10 10 100 100))))) []))
                { coreFullscreen = Set.singleton w5
                }
            areas = Map.fromList [(OutputId 1, whole (rectAt 0 0 800 600)), (OutputId 2, secondOutput)]
        sceneWindows (planScene defaultSceneStyle st areas Map.empty)
          @?= Map.fromList [(w1, Placed Crop (rectAt 2 2 796 596)), (w2, Floating (rectAt 10 10 100 100)), (w3, Omitted), (w4, Placed Tolerate (rectAt 802 32 796 566)), (w5, Fullscreen (OutputId 2))]
    ]
 where
  content = either (error . show) id (mkSize 97 77 >>= mkRect (Point (-98) 22))

whole :: Rect -> OutputArea
whole r = OutputArea r r

rectAt :: Int -> Int -> Int -> Int -> Rect
rectAt x y w h = either (error . show) id (mkSize (fromIntegral w) (fromIntegral h) >>= mkRect (Point (fromIntegral x) (fromIntegral y)))

secondOutput :: OutputArea
secondOutput = OutputArea (rectAt 800 0 800 600) (rectAt 800 30 800 570)

floatState :: l -> Stack WindowId -> [(WindowId, FloatGeom)] -> [WindowId] -> CoreState Int l
floatState l s fl order = (fromWorkspaces (AttachedState (StackSet (Screen (Workspace 1 l (Just s)) (OutputId 1)) [] [Workspace 2 l Nothing] (Map.fromList fl)) [])){coreRaiseOrder = order}

floatScene :: (LayoutClass l) => l -> Stack WindowId -> [(WindowId, FloatGeom)] -> [WindowId] -> Map.Map WindowId (Maybe WindowId) -> DesiredScene
floatScene l s fl order = planScene defaultSceneStyle (floatState l s fl order) (Map.singleton (OutputId 1) secondOutput)

floatTests :: TestTree
floatTests =
  testGroup
    "floating windows"
    [ testCase "M2-D01: a float's content rectangle is its stored geometry plus the output origin" $
        Map.lookup w2 (sceneWindows (floatScene (Tall 1 0.03 0.5) (Stack w2 [w1] []) [(w2, FloatAt (OutputRect (rectAt 30 40 200 100)))] [w2] Map.empty))
          @?= Just (Floating (rectAt 830 40 200 100))
    , testCase "a float leaves the layout, which tiles the rest of the stack in the usable area" $
        sceneWindows (floatScene Full (Stack w2 [w1] [w3]) [(w2, FloatAt (OutputRect (rectAt 30 40 200 100)))] [w2] Map.empty)
          @?= Map.fromList [(w1, Omitted), (w2, Floating (rectAt 830 40 200 100)), (w3, Placed Crop (rectAt 802 32 796 566))]
    , testCase "a float with no stored box is centered on the whole output inside its border" $
        Map.lookup w2 (sceneWindows (floatScene Full (Stack w2 [w1] []) [(w2, ClientSized)] [w2] Map.empty))
          @?= Just (Centered (rectAt 802 2 796 596))
    , testCase "a float on a hidden workspace is omitted" $
        let st = floatState Full (Stack w1 [] []) [(w4, FloatAt (OutputRect (rectAt 0 0 10 10)))] [w4]
            hiddenFloat = st{coreWorkspaces = AttachedState (StackSet (Screen (Workspace (1 :: Int) Full (Just (Stack w1 [] []))) (OutputId 1)) [] [Workspace 2 Full (Just (Stack w4 [] []))] (Map.singleton w4 (FloatAt (OutputRect (rectAt 0 0 10 10))))) []}
         in Map.lookup w4 (sceneWindows (planScene defaultSceneStyle hiddenFloat (Map.singleton (OutputId 1) secondOutput) Map.empty)) @?= Just Omitted
    , testCase "clamping moves a float back onto its output and shrinks only one larger than it" $
        [ Map.lookup w2 (sceneWindows (floatScene Full (Stack w2 [w1] []) [(w2, FloatAt (OutputRect stored))] [w2] Map.empty))
        | stored <- [rectAt 700 550 200 100, rectAt (-50) (-50) 200 100, rectAt 10 10 2000 100]
        ]
          @?= map (Just . Floating) [rectAt 1398 498 200 100, rectAt 802 2 200 100, rectAt 802 10 796 100]
    , testCase "clamping leaves the stored geometry unchanged" $
        let st = floatState Full (Stack w2 [w1] []) [(w2, FloatAt (OutputRect (rectAt 700 550 200 100)))] [w2]
         in case coreWorkspaces st of
              AttachedState ss _ -> Map.lookup w2 (floating ss) @?= Just (FloatAt (OutputRect (rectAt 700 550 200 100)))
              HeadlessState{} -> fail "attached"
    , testCase "M2-D02: a floating dialog stacks over its tiled parent, focused or not" $ do
        let parents = Map.singleton w2 (Just w1)
            dialogFocused = floatScene (Tall 1 0.03 0.5) (Stack w2 [] [w1, w3]) [(w2, ClientSized)] [w2] parents
            parentFocused = floatScene (Tall 1 0.03 0.5) (Stack w1 [w2] [w3]) [(w2, ClientSized)] [w2] parents
        sceneStacking dialogFocused @?= [w1, w3, w2]
        sceneStacking parentFocused @?= [w3, w1, w2]
    , testCase "M2-D02: focus moving to a tile leaves two overlapping floats in their order" $ do
        let overlapping = [(w2, FloatAt (OutputRect (rectAt 10 10 300 300))), (w3, FloatAt (OutputRect (rectAt 100 100 300 300)))]
            (st, _) = drainCore idHook [WindowInteraction w3, WindowInteraction w2, WindowInteraction w1] (floatState Full (Stack w1 [] [w2, w3]) overlapping [])
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) secondOutput) Map.empty
        sceneFocus result @?= Just w1
        sceneStacking result @?= [w1, w3, w2]
    , testCase "floating families follow their most recently raised member, the focused family last" $
        stackWindows [w1, w2, w3, w4] [w1, w4, w3] [] [w1, w2, w3, w4] (Just w2) (Map.singleton w3 (Just w1))
          @?= [w2, w4, w1, w3]
    , testProperty "every float lies inside its output with its border, and no float reaches a layout" $
        forAll ((,,) <$> genRect <*> vectorOf 4 genRect <*> sublistOf [w1, w2, w3, w4]) $ \(bounds, stored, floated) ->
          forAll (chooseInt (0, 20)) $ \border ->
            let geoms = Map.fromList [(w, g) | (w, r, sized) <- zip3 [w1, w2, w3, w4] stored (cycle [True, False, True]), w `elem` floated, let g = if sized then FloatAt (OutputRect r) else ClientSized]
                st = (fromWorkspaces (AttachedState (StackSet (Screen (Workspace (1 :: Int) Everything (Just (Stack w1 [] [w2, w3, w4]))) (OutputId 1)) [] [] geoms) [])){coreRaiseOrder = Map.keys geoms}
                result = planScene defaultSceneStyle{sceneBorderWidth = fromIntegral border} st (Map.singleton (OutputId 1) (OutputArea bounds bounds)) Map.empty
                b = fromIntegral border
                outer r = toInteger (rectX r) - b >= toInteger (rectX bounds) && toInteger (rectY r) - b >= toInteger (rectY bounds) && toInteger (rectX r) + toInteger (rectWidth r) + b <= toInteger (rectX bounds) + toInteger (rectWidth bounds) && toInteger (rectY r) + toInteger (rectHeight r) + b <= toInteger (rectY bounds) + toInteger (rectHeight bounds)
                check (w, p) = case p of
                  Placed _ r -> counterexample (show w ++ " floats, or the layout saw a float") (property (w `notElem` floated && toInteger (rectHeight r) + 2 * b == 100 * toInteger (4 - length floated)))
                  Floating r -> property (w `elem` floated && outer r)
                  Centered r -> property (w `elem` floated && outer r)
                  Fullscreen _ -> counterexample (show w ++ " is fullscreen") (property False)
                  HiddenFullscreen -> counterexample (show w ++ " is fullscreen") (property False)
                  Omitted -> property True
             in conjoin (map check (Map.toList (sceneWindows result)))
    ]

fullscreenTests :: TestTree
fullscreenTests =
  testGroup
    "fullscreen windows"
    [ testCase "M2-D03: a fullscreen tile keeps its slot and covers its workspace's output above both tiers" $ do
        let st = (floatState (Tall 1 0.03 0.5) (Stack w2 [w1] [w3]) [(w3, FloatAt (OutputRect (rectAt 10 10 100 100)))] [w3]){coreFullscreen = Set.singleton w2}
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) secondOutput) Map.empty
        sceneWindows result @?= Map.fromList [(w1, Placed Crop (rectAt 802 32 396 566)), (w2, Fullscreen (OutputId 1)), (w3, Floating (rectAt 810 10 100 100))]
        sceneFocus result @?= Just w2
        sceneStacking result @?= [w1, w3, w2]
    , testCase "a fullscreen float keeps its float state and covers its output" $ do
        let st = (floatState Full (Stack w3 [w1] []) [(w3, FloatAt (OutputRect (rectAt 10 10 100 100)))] [w3]){coreFullscreen = Set.singleton w3}
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) secondOutput) Map.empty
        sceneWindows result @?= Map.fromList [(w1, Placed Crop (rectAt 802 32 796 566)), (w3, Fullscreen (OutputId 1))]
        sceneStacking result @?= [w1, w3]
    , testCase "a hidden workspace's fullscreen window stays fullscreen and covers no output" $ do
        let st = (state Full (Stack w1 [] [])){coreFullscreen = Set.singleton w4}
            result = planScene defaultSceneStyle st (Map.singleton (OutputId 1) secondOutput) Map.empty
        Map.lookup w4 (sceneWindows result) @?= Just HiddenFullscreen
        Map.lookup w1 (sceneWindows result) @?= Just (Placed Crop (rectAt 802 32 796 566))
        sceneStacking result @?= [w4, w1]
    , testCase "fullscreen windows stack above the tiled and floating tiers" $
        stackWindows [w1, w2, w3, w4] [w3] [w1] [w1, w2, w3, w4] (Just w2) Map.empty
          @?= [w4, w2, w3, w1]
    ]

-- | Places every window it is given at the area's top-left, 100 pixels tall per window it is given.
data Everything = Everything deriving (Show)

instance LayoutClass Everything where
  pureLayout _ r s = [(w, tile) | let n = length (integrate s), Right tile <- [mkSize (rectWidth r) (fromIntegral (100 * n)) >>= mkRect (Point (rectX r) (rectY r))], w <- integrate s]

data MasterOnly = MasterOnly deriving (Show)

instance LayoutClass MasterOnly where
  pureLayout _ r s = [(w, r) | w <- take 1 (integrate s)]
