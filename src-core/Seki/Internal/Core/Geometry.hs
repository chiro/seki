{- | Integer geometry in river's global logical coordinate space.

Coordinates are signed 'Int32' values, so outputs left of or above the origin
have negative positions. A 'Rect' covers the half-open ranges
@[x, x + width)@ and @[y, y + height)@. Its width and height are never
negative, and its far edges @x + width@ and @y + height@ never exceed
@maxBound :: Int32@. Construction and translation check these bounds in
'Int64' and report 'GeometryError' instead of wrapping. Splitting and
mirroring a valid rectangle always yield valid rectangles, so those operations
are total.

Zero-area rectangles are valid. Splits may produce zero-width or zero-height
tiles; callers decide whether such a tile is drawable.

The split functions are adapted from @XMonad.Layout@ in xmonad 0.18.1
(BSD-3-Clause, see @THIRD_PARTY_NOTICES.md@). One deliberate difference: the
ratio splits clamp their ratio to @[0, 1]@, where xmonad would produce a
negative dimension.
-}
module Seki.Internal.Core.Geometry (
  Point (..),
  Size,
  sizeWidth,
  sizeHeight,
  mkSize,
  Rect,
  rectOrigin,
  rectSize,
  rectX,
  rectY,
  rectWidth,
  rectHeight,
  mkRect,
  GeometryError (..),
  translate,
  mirror,
  splitVertically,
  splitHorizontally,
  splitVerticallyBy,
  splitHorizontallyBy,
) where

import Data.Int (Int32, Int64)

-- | A position in global logical coordinates.
data Point = Point
  { pointX :: !Int32
  , pointY :: !Int32
  }
  deriving (Eq, Ord, Show)

-- | A non-negative width and height. Build one with 'mkSize'.
data Size = Size
  { sizeWidth :: !Int32
  , sizeHeight :: !Int32
  }
  deriving (Eq, Ord, Show)

-- | A rectangle whose far edges fit in 'Int32'. Build one with 'mkRect'.
data Rect = Rect
  { rectOrigin :: !Point
  , rectSize :: !Size
  }
  deriving (Eq, Ord, Show)

-- | Why a size or rectangle could not be constructed.
data GeometryError
  = -- | The requested width and height, at least one of which is negative.
    NegativeSize !Int32 !Int32
  | -- | The requested x, y, width, and height, whose far edge or origin falls
    -- outside 'Int32'.
    RectOutOfRange !Int64 !Int64 !Int32 !Int32
  deriving (Eq, Show)

mkSize :: Int32 -> Int32 -> Either GeometryError Size
mkSize width height
  | width < 0 || height < 0 = Left (NegativeSize width height)
  | otherwise = Right (Size width height)

mkRect :: Point -> Size -> Either GeometryError Rect
mkRect (Point x y) = checkedRect (fromIntegral x) (fromIntegral y)

rectX, rectY, rectWidth, rectHeight :: Rect -> Int32
rectX = pointX . rectOrigin
rectY = pointY . rectOrigin
rectWidth = sizeWidth . rectSize
rectHeight = sizeHeight . rectSize

-- | Move a rectangle by the given offsets, keeping its size.
translate :: Int32 -> Int32 -> Rect -> Either GeometryError Rect
translate dx dy (Rect (Point x y) size) =
  checkedRect (fromIntegral x + fromIntegral dx) (fromIntegral y + fromIntegral dy) size

checkedRect :: Int64 -> Int64 -> Size -> Either GeometryError Rect
checkedRect x y size@(Size width height)
  | inRange x width && inRange y height =
      Right (Rect (Point (fromIntegral x) (fromIntegral y)) size)
  | otherwise = Left (RectOutOfRange x y width height)
 where
  inRange origin len =
    origin >= fromIntegral (minBound :: Int32)
      && origin + fromIntegral len <= fromIntegral (maxBound :: Int32)

-- | Exchange the x and y axes. @mirror . mirror = id@.
mirror :: Rect -> Rect
mirror (Rect (Point x y) (Size width height)) = Rect (Point y x) (Size height width)

{- | Split into @n@ tiles stacked top to bottom. Each tile but the last has
height @height `div` n@; the last takes the remainder. Fewer than two tiles
returns the rectangle unchanged.
-}
splitVertically :: Int -> Rect -> [Rect]
splitVertically n rect | n < 2 = [rect]
splitVertically n (Rect (Point x y) (Size width height)) =
  Rect (Point x y) (Size width small)
    : splitVertically (n - 1) (Rect (Point x (y + small)) (Size width (height - small)))
 where
  small = fromInteger (toInteger height `div` toInteger n)

-- | Split into @n@ tiles side by side, left to right, as 'splitVertically'.
splitHorizontally :: Int -> Rect -> [Rect]
splitHorizontally n = map mirror . splitVertically n . mirror

{- | Split into a left part whose width is the ratio of the whole, rounded
down, and a right part with the rest.
-}
splitHorizontallyBy :: Rational -> Rect -> (Rect, Rect)
splitHorizontallyBy ratio (Rect (Point x y) (Size width height)) =
  ( Rect (Point x y) (Size leftWidth height)
  , Rect (Point (x + leftWidth) y) (Size (width - leftWidth) height)
  )
 where
  leftWidth = floor (toRational width * max 0 (min 1 ratio))

-- | Split into a top and a bottom part, as 'splitHorizontallyBy'.
splitVerticallyBy :: Rational -> Rect -> (Rect, Rect)
splitVerticallyBy ratio rect =
  let (top, bottom) = splitHorizontallyBy ratio (mirror rect)
   in (mirror top, mirror bottom)
