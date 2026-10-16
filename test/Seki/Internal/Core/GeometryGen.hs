-- | Rectangle generators and predicates shared by the geometry and layout tests.
module Seki.Internal.Core.GeometryGen (
  genRect,
  genRatio,
  within,
  overlapArea,
  rectOf,
  fields,
  expectRight,
) where

import Data.Int (Int32, Int64)
import Seki.Internal.Core.Geometry (Point (..), Rect, mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Test.QuickCheck (Gen, chooseInt, elements, frequency, oneof)
import Test.QuickCheck qualified as QC
import Test.Tasty.HUnit (assertFailure)

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
