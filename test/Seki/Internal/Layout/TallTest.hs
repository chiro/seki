{- | The Tall layout, with the properties of xmonad 0.18.1's
@tests/Properties/Layout/Tall.hs@ and their @tests/Properties.hs@ entries under
their xmonad names.

Adapted properties:

* @noOverlaps@ compares every pair of tiles by overlapping area, where xmonad
  compares only unequal rectangles by inclusive corners. A zero-width or
  zero-height tile has no area, so it overlaps nothing.
* @split_horizontal@ drops xmonad's overflow precondition, since every 'Rect'
  keeps its far edges inside 'Data.Int.Int32'.
-}
module Seki.Internal.Layout.TallTest (tests) where

import Data.Int (Int32)
import Data.List (nub, sort)
import Data.Maybe (fromJust, isJust)
import Data.Ratio ((%))
import Data.Word (Word32)
import Seki.Internal.Core.Geometry (Rect, rectHeight, rectWidth, rectX, splitHorizontally, splitVerticallyBy)
import Seki.Internal.Core.GeometryGen (fields, genRatio, genRect, overlapArea, rectAt, within)
import Seki.Internal.Core.Layout (IncMasterN (..), LayoutClass (..), Resize (..), SomeMessage (..))
import Seki.Internal.Core.StackSet (Screen (..), Stack (..), StackSet (..), WindowId (..), Workspace (..), index, peek)
import Seki.Internal.Core.StackSetGen (T)
import Seki.Internal.Layout.Tall (Tall (..), tile)
import Test.QuickCheck (
  Gen,
  NonNegative (..),
  Positive (..),
  Property,
  arbitrary,
  chooseInt,
  conjoin,
  counterexample,
  forAll,
  suchThat,
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Tall layout"
    [ testGroup
        "xmonad properties"
        [ testProperty "tile_fullscreen" prop_tile_fullscreen
        , testProperty "tile_non_overlap" prop_tile_non_overlap
        , testProperty "tile_max_ratio" prop_tile_max_ratio
        , testProperty "tile_min_ratio" prop_tile_min_ratio
        , testProperty "split_horizontal" prop_split_horizontal
        , testProperty "split_vertical" prop_split_vertical
        , testProperty "purelayout_tall" (\n d r -> forAll genRect (prop_purelayout_tall n d r))
        , testProperty "purelayout_tall: ratio = 0" (\n d -> forAll genRect (prop_purelayout_tall n d 0))
        , testProperty "purelayout_tall: ratio = 1" (\n d -> forAll genRect (prop_purelayout_tall n d 1))
        , testProperty "shrink_tall" prop_shrink_tall
        , testProperty "expand_tall" prop_expand_tall
        , testProperty "incmaster_tall" prop_incmaster_tall
        ]
    , testGroup
        "placement"
        [ testProperty "tiles stay inside the rectangle, apart, and once per window" propTallPlacement
        , testCase "ratio 0 omits the masters" testRatioZero
        , testCase "ratio 1 omits the stack" testRatioOne
        , testCase "zero masters tile one column" testZeroMasters
        , testCase "zero masters at ratio 1 place nothing" testZeroMastersRatioOne
        , testCase "more masters than windows tile one column" testMoreMasters
        , testCase "a ratio that floors to zero keeps a zero-width master" testZeroWidthMaster
        , testCase "a one-pixel rectangle keeps zero-sized tiles" testOnePixel
        ]
    , testGroup
        "default Tall golden examples"
        [ testCase "1920x1080 at the origin" testGoldenOrigin
        , testCase "odd 1921x1081" testGoldenOdd
        , testCase "negative origin at (-1920, 0)" testGoldenNegative
        ]
    , testCase "description" (assertEqual "description" "Tall" (description (Tall 1 (3 / 100) (1 / 2))))
    ]

noOverlaps :: [Rect] -> Bool
noOverlaps rs = and [overlapArea a b == 0 | (i, a) <- indexed, (j, b) <- indexed, i < j]
 where
  indexed = zip [0 :: Int ..] rs

prop_tile_fullscreen :: Property
prop_tile_fullscreen = forAll genRect $ \rect -> tile (1 / 2) rect 1 1 === [rect]

prop_tile_non_overlap :: Int -> Int -> Property
prop_tile_non_overlap windows nmaster =
  forAll genRect $ \rect ->
    let tiled = tile (3 % 100) rect nmaster windows
     in counterexample (show tiled) (noOverlaps tiled)

prop_tile_max_ratio :: Property
prop_tile_max_ratio = extremeRatio 1 drop

prop_tile_min_ratio :: Property
prop_tile_min_ratio = extremeRatio 0 take

extremeRatio :: Rational -> (Int -> [Rect] -> [Rect]) -> Property
extremeRatio amount getRects =
  forAll genRect $ \rect ->
    forAll (chooseInt (1, 100)) $ \windows ->
      forAll (chooseInt (0, windows - 1)) $ \nmaster ->
        let tiled = tile amount rect nmaster windows
         in counterexample (show tiled) $
              if nmaster == 0
                then noOverlaps tiled
                else all ((== 0) . rectWidth) (getRects nmaster tiled)

prop_split_horizontal :: NonNegative Int -> Property
prop_split_horizontal (NonNegative n) =
  forAll genRect $ \x ->
    let xs = splitHorizontally n x
     in conjoin
          [ sum (map (toInteger . rectWidth) xs) === toInteger (rectWidth x)
          , counterexample "heights" (all (\s -> rectHeight s == rectHeight x) xs)
          , map rectX xs === sort (map rectX xs)
          ]

prop_split_vertical :: Rational -> Property
prop_split_vertical r =
  forAll genRect $ \x ->
    let (a, b) = splitVerticallyBy r x
     in counterexample (show (a, b)) $
          rectX x == rectX a
            && rectX x == rectX b
            && rectWidth x == rectWidth a
            && rectWidth x == rectWidth b

prop_purelayout_tall :: Int -> Rational -> Rational -> Rect -> Property
prop_purelayout_tall n d r rect =
  forAll ((arbitrary :: Gen T) `suchThat` (isJust . peek)) $ \x ->
    let layout = Tall n d r
        st = fromJust (stack (workspace (current x)))
        ts = pureLayout layout rect st
        ntotal = length (index x)
     in counterexample (show ts) $
          ( if r == 0
              then (0 <= n && n <= ntotal) <= (length ts == ntotal - n)
              else
                if r == 1
                  then (0 <= n && n <= ntotal) <= (length ts == n)
                  else length ts == ntotal
          )
            && noOverlaps (map snd ts)
            && description layout == "Tall"

prop_shrink_tall :: NonNegative Int -> Positive Rational -> NonNegative Rational -> Bool
prop_shrink_tall (NonNegative n) (Positive delta) (NonNegative frac) =
  n == n'
    && delta == delta'
    && frac' <= frac
    && (if frac' < frac then frac' == 0 || frac' == frac - delta else frac == 0)
 where
  Tall n' delta' frac' = fromJust (pureMessage (Tall n delta frac) (SomeMessage Shrink))

prop_expand_tall :: NonNegative Int -> Positive Rational -> NonNegative Integer -> Positive Integer -> Bool
prop_expand_tall (NonNegative n) (Positive delta) (NonNegative n1) (Positive d1) =
  n == n'
    && delta == delta'
    && frac' >= frac
    && (if frac' > frac then frac' == 1 || frac' == frac + delta else frac == 1)
 where
  frac = min 1 (n1 % d1)
  Tall n' delta' frac' = fromJust (pureMessage (Tall n delta frac) (SomeMessage Expand))

prop_incmaster_tall :: NonNegative Int -> Positive Rational -> NonNegative Rational -> NonNegative Int -> Bool
prop_incmaster_tall (NonNegative n) (Positive delta) (NonNegative frac) (NonNegative k) =
  delta == delta' && frac == frac' && n' == n + k
 where
  Tall n' delta' frac' = fromJust (pureMessage (Tall n delta frac) (SomeMessage (IncMasterN k)))

propTallPlacement :: Property
propTallPlacement =
  forAll genRect $ \rect ->
    forAll (chooseInt (0, 6)) $ \n ->
      forAll genRatio $ \r ->
        forAll ((arbitrary :: Gen T) `suchThat` (isJust . peek)) $ \x ->
          let placed = pureLayout (Tall n (3 / 100) r) rect (fromJust (stack (workspace (current x))))
              ws = map fst placed
           in counterexample (show placed) $
                conjoin
                  [ counterexample "inside" (all ((`within` rect) . snd) placed)
                  , counterexample "apart" (noOverlaps (map snd placed))
                  , counterexample "once" (nub ws == ws)
                  , counterexample "members" (all (`elem` index x) ws)
                  ]

stackOf :: Int -> Stack WindowId
stackOf k = Stack (WindowId 1) [] (map WindowId [2 .. fromIntegral k :: Word32])

placements :: Tall -> Rect -> Int -> [(Word32, (Int32, Int32, Int32, Int32))]
placements l rect k = [(w, fields t) | (WindowId w, t) <- pureLayout l rect (stackOf k)]

screen :: Rect
screen = rectAt 0 0 1920 1080

testRatioZero :: Assertion
testRatioZero =
  assertEqual
    "placements"
    [(2, (0, 0, 1920, 540)), (3, (0, 540, 1920, 540))]
    (placements (Tall 1 (3 / 100) 0) screen 3)

testRatioOne :: Assertion
testRatioOne = assertEqual "placements" [(1, (0, 0, 1920, 1080))] (placements (Tall 1 (3 / 100) 1) screen 3)

testZeroMasters :: Assertion
testZeroMasters = do
  let column = [(1, (0, 0, 1920, 360)), (2, (0, 360, 1920, 360)), (3, (0, 720, 1920, 360))]
  assertEqual "half" column (placements (Tall 0 (3 / 100) (1 / 2)) screen 3)
  assertEqual "ratio 0" column (placements (Tall 0 (3 / 100) 0) screen 3)

testZeroMastersRatioOne :: Assertion
testZeroMastersRatioOne = assertEqual "placements" [] (placements (Tall 0 (3 / 100) 1) screen 3)

testMoreMasters :: Assertion
testMoreMasters =
  assertEqual
    "placements"
    [(1, (0, 0, 1920, 360)), (2, (0, 360, 1920, 360)), (3, (0, 720, 1920, 360))]
    (placements (Tall 5 (3 / 100) (1 / 2)) screen 3)

testZeroWidthMaster :: Assertion
testZeroWidthMaster =
  assertEqual
    "placements"
    [(1, (0, 0, 0, 30)), (2, (0, 0, 50, 30))]
    (placements (Tall 1 (3 / 100) (1 / 100)) (rectAt 0 0 50 30) 2)

testOnePixel :: Assertion
testOnePixel =
  assertEqual
    "placements"
    [(1, (0, 0, 0, 1)), (2, (0, 0, 1, 0)), (3, (0, 0, 1, 1))]
    (placements (Tall 1 (3 / 100) (1 / 2)) (rectAt 0 0 1 1) 3)

defaultTall :: Tall
defaultTall = Tall 1 (3 / 100) (1 / 2)

testGoldenOrigin :: Assertion
testGoldenOrigin = do
  let rect = rectAt 0 0 1920 1080
  assertEqual "one" [(1, (0, 0, 1920, 1080))] (placements defaultTall rect 1)
  assertEqual "two" [(1, (0, 0, 960, 1080)), (2, (960, 0, 960, 1080))] (placements defaultTall rect 2)
  assertEqual
    "three"
    [(1, (0, 0, 960, 1080)), (2, (960, 0, 960, 540)), (3, (960, 540, 960, 540))]
    (placements defaultTall rect 3)

testGoldenOdd :: Assertion
testGoldenOdd = do
  let rect = rectAt 0 0 1921 1081
  assertEqual "one" [(1, (0, 0, 1921, 1081))] (placements defaultTall rect 1)
  assertEqual "two" [(1, (0, 0, 960, 1081)), (2, (960, 0, 961, 1081))] (placements defaultTall rect 2)
  assertEqual
    "three"
    [(1, (0, 0, 960, 1081)), (2, (960, 0, 961, 540)), (3, (960, 540, 961, 541))]
    (placements defaultTall rect 3)

testGoldenNegative :: Assertion
testGoldenNegative = do
  let rect = rectAt (-1920) 0 1920 1080
  assertEqual "one" [(1, (-1920, 0, 1920, 1080))] (placements defaultTall rect 1)
  assertEqual "two" [(1, (-1920, 0, 960, 1080)), (2, (-960, 0, 960, 1080))] (placements defaultTall rect 2)
  assertEqual
    "three"
    [(1, (-1920, 0, 960, 1080)), (2, (-960, 0, 960, 540)), (3, (-960, 540, 960, 540))]
    (placements defaultTall rect 3)
