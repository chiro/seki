module Seki.Run.LatencyWorkloadTest (tests) where

import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.Word (Word64)
import Seki.Fixture.Probe (Color (..), Image (..))
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
    ]
 where
  m1Colors = [Green, Red, Blue, Green, Red, Blue, Green, Red]
  edges = [0, 85, 170, 256, 342, 428, 514, 600]
  m1Boxes = (2, 2, 396, 596) : [(402, top + 2, 396, bottom - top - 4) | (top, bottom) <- zip edges (tail edges)]

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
