{- | Integer geometry in river's global logical coordinate space.

Coordinates are signed, rectangles are half-open, and construction reports
'GeometryError' instead of wrapping on 'Data.Int.Int32' overflow.
-}
module Seki.Core.Geometry (
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

import Seki.Internal.Core.Geometry
