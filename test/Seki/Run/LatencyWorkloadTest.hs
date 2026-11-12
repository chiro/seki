module Seki.Run.LatencyWorkloadTest (tests) where

import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Word (Word64)
import Seki.Fixture.Probe (Color (..), Image (..))
import Seki.Internal.Core.Geometry (Point (..), mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Layout.Tall (tile)
import Seki.Latency.Workload
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "latency workload"
    [ testCase "m1 opens eight tiles in the retained color cycle" $
        windowRoles M1 @?= map TileRole (take 8 (cycle [Red, Green, Blue]))
    , testCase "m1 layers are the retained hard-coded Tall boxes and colors" $
        [(layerColor l, layerContent l, layerBorder l) | l <- expectedLayers (initialModel M1)] @?= zip3 m1Colors m1Boxes (repeat 2)
    , testCase "m1 plan keeps the retained action codes" $
        map stepCode (take 300 (plan M1 1910 (initialModel M1))) @?= take 300 (m1Codes 1910)
    , testCase "every m1 step changes what the probe can observe" $
        everyStepObservable M1
    , testCase "an m1 picture of its own layers matches, and one wrong pixel is reported" $ do
        let layers = expectedLayers (initialModel M1)
            image = paint layers
        pictureMismatch layers image @?= Nothing
        assertBool "a recolored content pixel" (pictureMismatch layers (setPixel 100 100 (0, 0, 255) image) /= Nothing)
        assertBool "a probe color in a border band" (pictureMismatch layers (setPixel 0 0 (0, 255, 0) image) /= Nothing)
    , testCase "a hidden workspace shows no probe pixel and has no keyboard owner" $ do
        let hidden = stepModel (initialModel M1) ViewEmpty
        expectedLayers hidden @?= []
        observedOwner (expectedObservation hidden) @?= Nothing
        pictureMismatch [] (paint []) @?= Nothing
    , testCase "a hidden workspace keeps every window's last committed size, fullscreen included" $ do
        let full = stepModel (stepModel (initialModel M2) FocusUp) EnterFullscreen
            sizes = observedSizes . expectedObservation
        sequence_ [sizes (stepModel m ViewEmpty) @?= sizes m | m <- [initialModel M1, initialModel M2, full, initialModel M3]]
    , testCase "workload names parse from their text" $
        map (parseWorkloadName . workloadText) [minBound .. maxBound] @?= map Just [M1, M2, M3]
    , testCase "the tile table agrees with the production Tall layout" $
        sequence_ [tileBoxes r n @?= production r n | r <- [Half, TwoFifths], n <- [1, 6, 8]]
    , testCase "m2 opens six tiles, then two dialogs of the first tile" $
        windowRoles M2 @?= map TileRole [Red, Green, Blue, Red, Green, Blue] ++ [DialogRole Red (300, 200) 0, DialogRole Green (200, 300) 0]
    , testCase "m2 layers are the tiles, then the centered dialogs in raise order" $
        [(layerColor l, layerContent l) | l <- expectedLayers (initialModel M2)]
          @?= zip [Blue, Green, Red, Blue, Green, Red] (tileBoxes Half 6) ++ [(Red, (250, 200, 300, 200)), (Green, (300, 150, 200, 300))]
    , testCase "focusing a dialog raises it, and focusing a tile keeps the raise order" $ do
        let m0 = initialModel M2
            dialog = stepModel m0 FocusDown
            tile' = stepModel dialog FocusDown
        (modelRaise m0, modelRaise dialog, modelRaise tile') @?= ([1, 0], [0, 1], [0, 1])
        map layerColor (drop 6 (expectedLayers tile')) @?= [Green, Red]
    , testCase "fullscreen covers the output, a view keeps it, and a focus change exits it" $ do
        let tiled = stepModel (initialModel M2) FocusUp
            full = stepModel tiled EnterFullscreen
        modelFullscreen full @?= Just 7
        last (expectedLayers full) @?= Layer Red (0, 0, 800, 600) 0
        Map.lookup 7 (observedSizes (expectedObservation full)) @?= Just (800, 600)
        modelFullscreen (stepModel (stepModel full ViewEmpty) ViewPopulated) @?= Just 7
        modelFullscreen (stepModel full FocusUp) @?= Nothing
        modelFullscreen (stepModel full ExitFullscreen) @?= Nothing
    , testCase "a resize changes every tile's committed size" $ do
        let m0 = initialModel M2
            sizes = observedSizes . expectedObservation
            changed = Map.keys (Map.filter id (Map.intersectionWith (/=) (sizes m0) (sizes (stepModel m0 ShrinkMaster))))
        changed @?= [2 .. 7]
    , testCase "every m2 step changes what the probe can observe" $
        everyStepObservable M2
    , testCase "the m2 plan enters fullscreen only on a focused tile" $ do
        let m0 = initialModel M2
            steps = take 3000 (plan M2 1910 m0)
        sequence_ [assertBool (show m) (modelFocus m >= 2) | (m, EnterFullscreen) <- zip (scanl stepModel m0 steps) steps]
    , testCase "seed 1909 exercises every m2 feature in 1023 measured steps" $ do
        let m0 = initialModel M2
            warmed = foldl stepModel m0 (take 24 (plan M2 1909 m0))
            counts = exerciseCounts (exercised warmed (take 1023 (plan M2 1910 warmed)))
        assertBool (show counts) (all ((> 0) . snd) counts)
        map fst counts @?= ["clip_resizes", "fullscreen_enters", "fullscreen_exits_by_focus", "fullscreen_exits_by_request", "hidden_fullscreen_views", "float_raises"]
    , testCase "m1 exercises none of the m2 features" $
        exerciseCounts (exercised (initialModel M1) (take 300 (plan M1 1910 (initialModel M1)))) @?= map (\(n, _) -> (n, 0)) (exerciseCounts (exercised (initialModel M2) []))
    , testCase "the restore steps leave workspace 1 shown with no fullscreen window" $ do
        let full = stepModel (stepModel (stepModel (initialModel M2) FocusUp) EnterFullscreen) ViewEmpty
            restored = foldl stepModel full (restoreSteps full)
        (modelShown restored, modelFullscreen restored) @?= (True, Nothing)
    , testCase "a picture with overlapping dialogs and a fullscreen window matches its own layers" $ do
        let full = stepModel (stepModel (initialModel M2) FocusUp) EnterFullscreen
            dialogs = expectedLayers (initialModel M2)
        pictureMismatch (expectedLayers full) (paint (expectedLayers full)) @?= Nothing
        pictureMismatch dialogs (paint dialogs) @?= Nothing
        assertBool "the lower dialog showing through the upper" (pictureMismatch dialogs (paint (reverse (drop 6 dialogs) ++ take 6 dialogs)) /= Nothing)
    , testCase "m3 opens two tiles, then the pad, which the WM adopts and shows" $ do
        windowRoles M3 @?= [TileRole Red, TileRole Blue, PadRole Green]
        let m0 = initialModel M3
        (modelFocus m0, modelPadShown m0, observedOwner (expectedObservation m0)) @?= (1, True, Just 0)
        expectedLayers m0 @?= zipWith (\c b -> Layer c b 2) [Blue, Red] (tileBoxes Half 2) ++ [Layer Green padContentBox 2]
    , testCase "hiding the pad returns the keyboard to the focused tile and keeps every size" $ do
        let m0 = initialModel M3
            hidden = stepModel m0 TogglePad
        observedOwner (expectedObservation hidden) @?= Just 1
        observedSizes (expectedObservation hidden) @?= observedSizes (expectedObservation m0)
        Map.lookup 0 (observedSizes (expectedObservation m0)) @?= Just (400, 300)
        map layerColor (expectedLayers hidden) @?= [Blue, Red]
    , testCase "the m3 plan hides the adopted pad, then cycles two views, two toggles, and a recompile" $
        take 11 (plan M3 1909 (initialModel M3))
          @?= [TogglePad, ViewEmpty, ViewPopulated, TogglePad, TogglePad, Recompile, ViewEmpty, ViewPopulated, TogglePad, TogglePad, Recompile]
    , testCase "the m3 plan ignores the seed that m1 and m2 draw their steps from" $ do
        let m0 = initialModel M3
        sequence_ [take 300 (plan M3 seed m0) @?= take 300 (plan M3 1909 m0) | seed <- [0, 1, 1910, maxBound]]
        assertBool "m2 plans differ by seed" (take 300 (plan M2 1909 (initialModel M2)) /= take 300 (plan M2 1910 (initialModel M2)))
    , testCase "every m3 step changes what the probe or the subscriber observes" $ do
        let m0 = initialModel M3
            models = scanl stepModel m0 (take 3000 (plan M3 1910 m0))
            seen m = (expectedObservation m, expectedPublication m)
        sequence_ [assertBool (show m) (seen next /= seen m) | (m, next) <- zip models (drop 1 models)]
    , testCase "a recompile publishes one more finished build and leaves the probe's view alone" $ do
        let m0 = stepModel (initialModel M3) TogglePad
            built = stepModel m0 Recompile
        (expectedFocusedTag (expectedPublication built), expectedFinishedBuilds (expectedPublication built)) @?= ("1", 1)
        expectedObservation built @?= expectedObservation m0
        expectedFocusedTag (expectedPublication (stepModel m0 ViewEmpty)) @?= "2"
    , testCase "24 warmup steps leave the pad shown, so 2500 measured m3 steps time 1001 toggles and 499 recompiles" $ do
        let m0 = initialModel M3
            warmed = foldl stepModel m0 (take 24 (plan M3 1909 m0))
            measured = take 2500 (plan M3 1910 warmed)
            counts = Map.fromListWith (+) [(endpointText e, 1 :: Int) | s <- measured, e <- stepEndpoints M3 s]
        modelPadShown warmed @?= True
        counts @?= Map.fromList [("external_wake", 1000), ("ipc_update", 1000), ("scratchpad_toggle", 1001), ("recompile_completion", 499)]
    , testCase "the m3 restore steps show workspace 1 with the pad hidden" $ do
        let shownAway = stepModel (initialModel M3) ViewEmpty
            restored = foldl stepModel shownAway (restoreSteps shownAway)
        restoreSteps shownAway @?= [ViewPopulated, TogglePad]
        (modelShown restored, modelPadShown restored) @?= (True, False)
    , testCase "every m3 plan step and restore step has an IPC command" $ do
        let m0 = initialModel M3
            warmup = take 24 (plan M3 1909 m0)
            steps = warmup ++ take 3000 (plan M3 1910 (foldl stepModel m0 warmup))
        sequence_ [assertBool (show s) (isJust (stepCommand s)) | s <- steps ++ concatMap restoreSteps (scanl stepModel m0 steps)]
    , testCase "m1 and m2 steps time no m3 endpoint" $
        [e | name <- [M1, M2], s <- [minBound .. maxBound], e <- stepEndpoints name s] @?= []
    , testCase "the shown pad's picture matches its own layers, and a missing pad is reported" $ do
        let layers = expectedLayers (initialModel M3)
        pictureMismatch layers (paint layers) @?= Nothing
        assertBool "pad absent" (pictureMismatch layers (paint (take 2 layers)) /= Nothing)
    ]
 where
  m1Colors = [Green, Red, Blue, Green, Red, Blue, Green, Red]
  edges = [0, 85, 170, 256, 342, 428, 514, 600]
  m1Boxes = (2, 2, 396, 596) : [(402, top + 2, 396, bottom - top - 4) | (top, bottom) <- zip edges (tail edges)]

production :: Ratio -> Int -> [(Int, Int, Int, Int)]
production r n = [(fromIntegral (rectX t) + 2, fromIntegral (rectY t) + 2, fromIntegral (rectWidth t) - 4, fromIntegral (rectHeight t) - 4) | t <- tile frac screen 1 n]
 where
  frac = case r of Half -> 1 / 2; TwoFifths -> 2 / 5
  screen = either (error . show) id (mkSize 800 600 >>= mkRect (Point 0 0))

m1Codes :: Word64 -> [Int]
m1Codes n = concatMap (\x -> [fromIntegral ((x `shiftR` 32) `mod` 2), 2, 3]) (tail (iterate (\x -> x * 6364136223846793005 + 1442695040888963407) n))

everyStepObservable :: WorkloadName -> Assertion
everyStepObservable name = do
  let m0 = initialModel name
      steps = take 3000 (plan name 1910 m0)
      models = scanl stepModel m0 steps
  sequence_
    [ assertBool (show (m, s)) (expectedObservation next /= expectedObservation m)
    | (m, s, next) <- zip3 models steps (drop 1 models)
    ]

{- | The image a compositor draws for these layers over a black background,
with a gray border band.
-}
paint :: [Layer] -> Image
paint layers = Image 800 600 (BS.pack (concat [pixel x y | y <- [0 .. 599], x <- [0 .. 799]]))
 where
  pixel x y = case [l | l <- reverse layers, inside (outer l) x y] of
    l : _ | inside (layerContent l) x y -> rgb (layerColor l)
    _ : _ -> [128, 128, 128]
    [] -> [0, 0, 0]
  outer l = let (x, y, w, h) = layerContent l; b = layerBorder l in (x - b, y - b, w + 2 * b, h + 2 * b)
  inside (bx, by, bw, bh) x y = x >= bx && x < bx + bw && y >= by && y < by + bh
  rgb Red = [255, 0, 0]
  rgb Green = [0, 255, 0]
  rgb Blue = [0, 0, 255]

setPixel :: Int -> Int -> (Int, Int, Int) -> Image -> Image
setPixel x y (r, g, b) image =
  let offset = (y * imageWidth image + x) * 3
      (front, back) = BS.splitAt offset (imageRgb image)
   in image{imageRgb = front <> BS.pack (map fromIntegral [r, g, b]) <> BS.drop 3 back}
