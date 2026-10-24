{-# LANGUAGE LambdaCase #-}

module Seki.Internal.Core.PointerOpTest (tests) where

import Data.Int (Int32)
import Seki.Internal.Core.Geometry (OutputRect (..), Rect, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.GeometryGen (fields, genRect, rectAt)
import Seki.Internal.Core.PointerOp (HEdge (..), ResizeEdges (..), SizeBounds (..), VEdge (..), moveBox, noSizeBounds, resizeBox)
import Test.QuickCheck (Gen, Property, chooseInt, conjoin, counterexample, elements, forAll, oneof, (===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Core pointer op geometry"
    [ testCase "a bottom-right resize keeps the top-left and stops at the minimum hint" $
        fields (unwrap (resizeBox (SizeBounds 200 150 0 0) (Corner RightEdge BottomEdge) (box 100 100 400 300) (-300) (-200))) @?= (100, 100, 200, 150)
    , testCase "a top-left resize keeps the bottom-right edges" $
        fields (unwrap (resizeBox noSizeBounds (Corner LeftEdge TopEdge) (box 100 100 400 300) (-20) (-10))) @?= (80, 90, 420, 310)
    , testCase "a left resize past the right edge stops at one pixel with the right edge fixed" $
        fields (unwrap (resizeBox noSizeBounds (Horizontal LeftEdge) (box 100 100 400 300) 1000 0)) @?= (499, 100, 1, 300)
    , testCase "a resize stops at the maximum hint" $
        fields (unwrap (resizeBox (SizeBounds 0 0 450 320) (Corner RightEdge BottomEdge) (box 0 0 400 300) 100 100)) @?= (0, 0, 450, 320)
    , testCase "a maximum below the minimum caps at the minimum" $
        fields (unwrap (resizeBox (SizeBounds 300 0 200 0) (Horizontal RightEdge) (box 0 0 400 300) 100 0)) @?= (0, 0, 300, 300)
    , testCase "a move shifts the origin by the offset and keeps the size" $
        fields (unwrap (moveBox (box 2 2 496 596) 30 10)) @?= (32, 12, 496, 596)
    , testCase "a move stops at the Int32 range" $
        fields (unwrap (moveBox (box 0 0 10 10) maxBound minBound)) @?= (maxBound - 10, minBound, 10, 10)
    , testProperty "a dragged axis never falls below max 1 minimum, at any Int32 value" propFloor
    , testProperty "a start box at or above the floor yields no dimension below it" propNoDimensionBelowFloor
    , testProperty "a dragged axis never exceeds a positive maximum raised to the floor" propCap
    , testProperty "the edge opposite each dragged edge stays fixed" propAnchor
    , testProperty "an axis that is not dragged stays unchanged, at any Int32 value" propUndragged
    , testProperty "a move keeps the size and shifts by the offset" propMove
    , testProperty "a move keeps the size at any Int32 value" propMoveTotal
    ]

box :: Int32 -> Int32 -> Int32 -> Int32 -> OutputRect
box x y w h = OutputRect (rectAt x y w h)

unwrap :: OutputRect -> Rect
unwrap (OutputRect r) = r

data Axis = Axis {axisDragged :: Bool, axisNear :: Bool}

horizontal, vertical :: ResizeEdges -> Axis
horizontal = \case
  Horizontal e -> Axis True (e == LeftEdge)
  Corner e _ -> Axis True (e == LeftEdge)
  Vertical _ -> Axis False False
vertical = \case
  Vertical e -> Axis True (e == TopEdge)
  Corner _ e -> Axis True (e == TopEdge)
  Horizontal _ -> Axis False False

genEdges :: Gen ResizeEdges
genEdges = elements ([Horizontal h | h <- hs] ++ [Vertical v | v <- vs] ++ [Corner h v | h <- hs, v <- vs])
 where
  hs = [LeftEdge, RightEdge]
  vs = [TopEdge, BottomEdge]

genBounds :: Gen SizeBounds
genBounds = SizeBounds <$> bound <*> bound <*> bound <*> bound
 where
  bound = oneof [pure 0, small, QC.chooseBoundedIntegral (0, maxBound)]
  small = fromIntegral <$> chooseInt (1, 2000)

genSmallBounds :: Gen SizeBounds
genSmallBounds = SizeBounds <$> bound <*> bound <*> bound <*> bound
 where
  bound = oneof [pure 0, fromIntegral <$> chooseInt (1, 2000)]

genDelta :: Gen Int32
genDelta = oneof [fromIntegral <$> chooseInt (-3000, 3000), QC.chooseBoundedIntegral (minBound, maxBound)]

genSmallDelta :: Gen Int32
genSmallDelta = fromIntegral <$> chooseInt (-3000, 3000)

genSmallBox :: Gen OutputRect
genSmallBox = box <$> coordinate <*> coordinate <*> len <*> len
 where
  coordinate = fromIntegral <$> chooseInt (-3000, 3000)
  len = fromIntegral <$> chooseInt (0, 3000)

floorOf :: Int32 -> Integer
floorOf m = max 1 (toInteger m)

-- | Each axis as (bound minimum, bound maximum, start origin, start length, result origin, result length, axis).
axes :: SizeBounds -> ResizeEdges -> OutputRect -> OutputRect -> [(Int32, Int32, Integer, Integer, Integer, Integer, Axis)]
axes bounds edges (OutputRect s) (OutputRect r) =
  [ (boundsMinWidth bounds, boundsMaxWidth bounds, i (rectX s), i (rectWidth s), i (rectX r), i (rectWidth r), horizontal edges)
  , (boundsMinHeight bounds, boundsMaxHeight bounds, i (rectY s), i (rectHeight s), i (rectY r), i (rectHeight r), vertical edges)
  ]
 where
  i = toInteger

propFloor :: Property
propFloor = forAll genBounds $ \bounds -> forAll genEdges $ \edges -> forAll (OutputRect <$> genRect) $ \start -> forAll genDelta $ \dx -> forAll genDelta $ \dy ->
  let result = resizeBox bounds edges start dx dy
   in counterexample (show result) . conjoin $
        [ counterexample "dragged length below the floor" (len >= floorOf lo)
        | (lo, _, _, _, _, len, axis) <- axes bounds edges start result
        , axisDragged axis
        ]

propNoDimensionBelowFloor :: Property
propNoDimensionBelowFloor = forAll genBounds $ \bounds -> forAll genEdges $ \edges -> forAll (OutputRect <$> genRect) $ \start -> forAll genDelta $ \dx -> forAll genDelta $ \dy ->
  let result = resizeBox bounds edges start dx dy
      startAbove = and [len0 >= floorOf lo | (lo, _, _, len0, _, _, _) <- axes bounds edges start result]
   in startAbove QC.==> conjoin [counterexample (show result) (len >= floorOf lo) | (lo, _, _, _, _, len, _) <- axes bounds edges start result]

propCap :: Property
propCap = forAll genBounds $ \bounds -> forAll genEdges $ \edges -> forAll (OutputRect <$> genRect) $ \start -> forAll genDelta $ \dx -> forAll genDelta $ \dy ->
  let result = resizeBox bounds edges start dx dy
   in counterexample (show result) . conjoin $
        [ counterexample "dragged length above the maximum" (len <= max (floorOf lo) (toInteger hi))
        | (lo, hi, _, _, _, len, axis) <- axes bounds edges start result
        , axisDragged axis
        , hi > 0
        ]

propAnchor :: Property
propAnchor = forAll genSmallBounds $ \bounds -> forAll genEdges $ \edges -> forAll genSmallBox $ \start -> forAll genSmallDelta $ \dx -> forAll genSmallDelta $ \dy ->
  let result = resizeBox bounds edges start dx dy
   in counterexample (show result) . conjoin $
        [ if axisNear axis
          then counterexample "far edge moved" (origin + len === origin0 + len0)
          else counterexample "near edge moved" (origin === origin0)
        | (_, _, origin0, len0, origin, len, axis) <- axes bounds edges start result
        , axisDragged axis
        ]

propUndragged :: Property
propUndragged = forAll genBounds $ \bounds -> forAll genEdges $ \edges -> forAll (OutputRect <$> genRect) $ \start -> forAll genDelta $ \dx -> forAll genDelta $ \dy ->
  let result = resizeBox bounds edges start dx dy
   in counterexample (show result) . conjoin $
        [ (origin, len) === (origin0, len0)
        | (_, _, origin0, len0, origin, len, axis) <- axes bounds edges start result
        , not (axisDragged axis)
        ]

propMove :: Property
propMove = forAll genSmallBox $ \start -> forAll genSmallDelta $ \dx -> forAll genSmallDelta $ \dy ->
  let OutputRect s = start
      OutputRect r = moveBox start dx dy
   in fields r === (rectX s + dx, rectY s + dy, rectWidth s, rectHeight s)

propMoveTotal :: Property
propMoveTotal = forAll (OutputRect <$> genRect) $ \start -> forAll genDelta $ \dx -> forAll genDelta $ \dy ->
  let OutputRect s = start
      OutputRect r = moveBox start dx dy
   in (rectWidth r, rectHeight r) === (rectWidth s, rectHeight s)
