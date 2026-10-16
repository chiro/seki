module Seki.Internal.Core.GeometryTest (tests) where

import Control.Monad (void)
import Data.Int (Int32, Int64)
import Seki.Internal.Core.Geometry (
  GeometryError (..),
  Point (..),
  Rect,
  mirror,
  mkRect,
  mkSize,
  rectHeight,
  rectWidth,
  rectX,
  rectY,
  splitHorizontally,
  splitHorizontallyBy,
  splitVertically,
  splitVerticallyBy,
  translate,
 )
import Test.QuickCheck (
  Gen,
  Property,
  chooseInt,
  conjoin,
  counterexample,
  elements,
  forAll,
  frequency,
  oneof,
  (===),
 )
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Core geometry"
    [ testGroup
        "construction"
        [ testCase "accepts negative origins" testNegativeOrigin
        , testCase "accepts zero area" testZeroArea
        , testCase "rejects negative sizes" testNegativeSize
        , testCase "rejects a far edge beyond Int32" testFarEdgeOverflow
        , testCase "accepts a far edge at Int32 maxBound" testFarEdgeAtBound
        , testCase "translation checks the far edge" testTranslateOverflow
        , testCase "translation keeps the size" testTranslate
        ]
    , testGroup
        "splitting"
        [ testCase "splits odd heights with the remainder last" testOddVerticalSplit
        , testCase "splits odd widths with the remainder last" testOddHorizontalSplit
        , testCase "splits one pixel among three tiles" testOnePixelSplit
        , testCase "keeps the rectangle below two tiles" testSplitBelowTwo
        , testCase "splits by ratio with a floored first part" testSplitByRatio
        , testCase "clamps ratios outside the unit interval" testSplitByClamp
        , testCase "splits a negative-origin rectangle" testNegativeOriginSplit
        , testProperty "n-way splits partition the rectangle" propSplitPartitions
        , testProperty "ratio splits partition the rectangle" propSplitByPartitions
        , testProperty "n-way splits produce max 1 n tiles" propSplitCount
        ]
    , testGroup
        "mirroring"
        [ testCase "exchanges axes with negative origins" testMirror
        , testProperty "is an involution" propMirrorInvolution
        , testProperty "relates horizontal and vertical splits" propMirrorSplit
        ]
    ]

testNegativeOrigin :: Assertion
testNegativeOrigin = do
  rect <- rectOf (-1920) (-20) 1280 720
  assertEqual "fields" (-1920, -20, 1280, 720) (fields rect)

testZeroArea :: Assertion
testZeroArea = do
  rect <- rectOf 10 10 0 0
  assertEqual "fields" (10, 10, 0, 0) (fields rect)

testNegativeSize :: Assertion
testNegativeSize = do
  assertEqual "width" (Left (NegativeSize (-1) 5)) (void (mkSize (-1) 5))
  assertEqual "height" (Left (NegativeSize 5 (-1))) (void (mkSize 5 (-1)))

testFarEdgeOverflow :: Assertion
testFarEdgeOverflow = do
  size <- expectRight (mkSize 2 0)
  assertEqual
    "overflow"
    (Left (RectOutOfRange (fromIntegral (maxBound :: Int32) - 1) 0 2 0))
    (void (mkRect (Point (maxBound - 1) 0) size))

testFarEdgeAtBound :: Assertion
testFarEdgeAtBound = do
  rect <- rectOf (maxBound - 1) (maxBound - 1) 1 1
  assertEqual "fields" (maxBound - 1, maxBound - 1, 1, 1) (fields rect)

testTranslateOverflow :: Assertion
testTranslateOverflow = do
  rect <- rectOf 0 0 10 10
  assertEqual
    "overflow"
    (Left (RectOutOfRange 0 (fromIntegral (maxBound :: Int32)) 10 10))
    (void (translate 0 maxBound rect))
  low <- rectOf minBound 0 10 10
  assertEqual
    "underflow"
    (Left (RectOutOfRange (fromIntegral (minBound :: Int32) - 1) 0 10 10))
    (void (translate (-1) 0 low))

testTranslate :: Assertion
testTranslate = do
  rect <- rectOf 100 200 30 40
  moved <- expectRight (translate (-150) 5 rect)
  assertEqual "fields" (-50, 205, 30, 40) (fields moved)

testOddVerticalSplit :: Assertion
testOddVerticalSplit = do
  rect <- rectOf 0 0 100 7
  assertEqual
    "tiles"
    [(0, 0, 100, 2), (0, 2, 100, 2), (0, 4, 100, 3)]
    (map fields (splitVertically 3 rect))

testOddHorizontalSplit :: Assertion
testOddHorizontalSplit = do
  rect <- rectOf 0 0 5 10
  assertEqual
    "tiles"
    [(0, 0, 2, 10), (2, 0, 3, 10)]
    (map fields (splitHorizontally 2 rect))

testOnePixelSplit :: Assertion
testOnePixelSplit = do
  rect <- rectOf 0 0 1 1
  assertEqual
    "tiles"
    [(0, 0, 0, 1), (0, 0, 0, 1), (0, 0, 1, 1)]
    (map fields (splitHorizontally 3 rect))

testSplitBelowTwo :: Assertion
testSplitBelowTwo = do
  rect <- rectOf 3 4 5 6
  assertEqual "one" [fields rect] (map fields (splitVertically 1 rect))
  assertEqual "zero" [fields rect] (map fields (splitVertically 0 rect))
  assertEqual "negative" [fields rect] (map fields (splitHorizontally (-2) rect))

testSplitByRatio :: Assertion
testSplitByRatio = do
  rect <- rectOf 0 0 1001 11
  let (left, right) = splitHorizontallyBy (1 / 2) rect
  assertEqual "left" (0, 0, 500, 11) (fields left)
  assertEqual "right" (500, 0, 501, 11) (fields right)
  let (top, bottom) = splitVerticallyBy (1 / 3) rect
  assertEqual "top" (0, 0, 1001, 3) (fields top)
  assertEqual "bottom" (0, 3, 1001, 8) (fields bottom)

testSplitByClamp :: Assertion
testSplitByClamp = do
  rect <- rectOf 10 0 100 10
  let (wideLeft, wideRight) = splitHorizontallyBy 2 rect
  assertEqual "above one" ((10, 0, 100, 10), (110, 0, 0, 10)) (fields wideLeft, fields wideRight)
  let (narrowLeft, narrowRight) = splitHorizontallyBy (-1) rect
  assertEqual "below zero" ((10, 0, 0, 10), (10, 0, 100, 10)) (fields narrowLeft, fields narrowRight)

testNegativeOriginSplit :: Assertion
testNegativeOriginSplit = do
  rect <- rectOf (-1920) (-1080) 1920 1080
  let (left, right) = splitHorizontallyBy (1 / 2) rect
  assertEqual "left" (-1920, -1080, 960, 1080) (fields left)
  assertEqual "right" (-960, -1080, 960, 1080) (fields right)

propSplitPartitions :: Property
propSplitPartitions =
  forAll genRect $ \rect ->
    forAll (chooseInt (-1, 12)) $ \n ->
      conjoin [partitions rect (splitVertically n rect), partitions rect (splitHorizontally n rect)]

propSplitByPartitions :: Property
propSplitByPartitions =
  forAll genRect $ \rect ->
    forAll genRatio $ \ratio ->
      let pair (a, b) = [a, b]
       in conjoin
            [ partitions rect (pair (splitHorizontallyBy ratio rect))
            , partitions rect (pair (splitVerticallyBy ratio rect))
            ]

propSplitCount :: Property
propSplitCount =
  forAll genRect $ \rect ->
    forAll (chooseInt (-3, 40)) $ \n ->
      length (splitVertically n rect) === max 1 n

testMirror :: Assertion
testMirror = do
  rect <- rectOf (-1920) 20 1280 720
  assertEqual "mirrored" (20, -1920, 720, 1280) (fields (mirror rect))

propMirrorInvolution :: Property
propMirrorInvolution = forAll genRect $ \rect -> mirror (mirror rect) === rect

propMirrorSplit :: Property
propMirrorSplit =
  forAll genRect $ \rect ->
    forAll (chooseInt (-1, 12)) $ \n ->
      forAll genRatio $ \ratio ->
        conjoin
          [ splitHorizontally n rect === map mirror (splitVertically n (mirror rect))
          , splitVerticallyBy ratio rect === both mirror (splitHorizontallyBy ratio (mirror rect))
          ]
 where
  both f (a, b) = (f a, f b)

partitions :: Rect -> [Rect] -> Property
partitions whole parts =
  counterexample (show (whole, parts)) $
    conjoin
      [ sum (map area parts) === area whole
      , QC.property (all (`within` whole) parts)
      , QC.property (and [overlapArea a b == 0 | (i, a) <- indexed, (j, b) <- indexed, i < j])
      ]
 where
  indexed = zip [0 :: Int ..] parts

area :: Rect -> Integer
area rect = toInteger (rectWidth rect) * toInteger (rectHeight rect)

within :: Rect -> Rect -> Bool
within inner outer =
  rectX inner >= rectX outer
    && rectY inner >= rectY outer
    && farX inner <= farX outer
    && farY inner <= farY outer

overlapArea :: Rect -> Rect -> Integer
overlapArea a b =
  toInteger (max 0 (min (farX a) (farX b) - max (x64 a) (x64 b)))
    * toInteger (max 0 (min (farY a) (farY b) - max (y64 a) (y64 b)))

x64, y64, farX, farY :: Rect -> Int64
x64 = fromIntegral . rectX
y64 = fromIntegral . rectY
farX rect = x64 rect + fromIntegral (rectWidth rect)
farY rect = y64 rect + fromIntegral (rectHeight rect)

genRatio :: Gen Rational
genRatio =
  frequency
    [ (1, elements [0, 1, 1 / 2, 3 / 100, 97 / 100, -1, 2])
    , (4, chooseInt (1, 1000) >>= \d -> (\n -> toRational n / toRational d) <$> chooseInt (0, d))
    ]

genRect :: Gen Rect
genRect = do
  x <- genCoordinate
  y <- genCoordinate
  w <- genLength x
  h <- genLength y
  either (error . show) pure (mkSize w h >>= mkRect (Point x y))
 where
  genCoordinate = oneof [genSmall, QC.chooseBoundedIntegral (minBound, maxBound)]
  genSmall = fromIntegral <$> chooseInt (-3000, 3000)
  genLength origin =
    let room = min (fromIntegral (maxBound :: Int32)) (fromIntegral (maxBound :: Int32) - fromIntegral origin) :: Int64
     in oneof
          [ fromIntegral <$> chooseInt (0, fromIntegral (min room 4000))
          , fromIntegral <$> QC.chooseBoundedIntegral (0, room)
          ]

rectOf :: Int32 -> Int32 -> Int32 -> Int32 -> IO Rect
rectOf x y w h = expectRight (mkSize w h >>= mkRect (Point x y))

fields :: Rect -> (Int32, Int32, Int32, Int32)
fields rect = (rectX rect, rectY rect, rectWidth rect, rectHeight rect)

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (assertFailure . show) pure
