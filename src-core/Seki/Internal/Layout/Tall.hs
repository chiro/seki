{- | The layout with a master column on the left and the other windows stacked
on the right.

Ported from @XMonad.Layout@ in xmonad 0.18.1 (BSD-3-Clause, see
@THIRD_PARTY_NOTICES.md@), with xmonad's semantics. Tiles of zero width or
height stay in the result; the scene planner treats them as omitted.
-}
module Seki.Internal.Layout.Tall (
  Tall (..),
  tile,
) where

import Control.Applicative ((<|>))
import Seki.Internal.Core.Geometry (Rect, splitHorizontallyBy, splitVertically)
import Seki.Internal.Core.Layout (IncMasterN (..), LayoutClass (..), Resize (..), fromMessage)
import Seki.Internal.Core.StackSet (integrate)

data Tall = Tall
  { tallNMaster :: !Int
  -- ^ The number of master windows.
  , tallRatioIncrement :: !Rational
  -- ^ How much 'Shrink' and 'Expand' change the ratio.
  , tallRatio :: !Rational
  -- ^ The master column's share of the width.
  }
  deriving (Eq, Show, Read)

{- | A ratio of exactly 0 omits the masters and a ratio of exactly 1 omits the
other windows, so neither gets a zero-width column.
-}
instance LayoutClass Tall where
  pureLayout (Tall nmaster _ frac) r s
    | frac == 0 = drop nmaster placed
    | frac == 1 = take nmaster placed
    | otherwise = placed
   where
    ws = integrate s
    placed = zip ws (tile frac r nmaster (length ws))

  pureMessage (Tall nmaster delta frac) m =
    (resize <$> fromMessage m) <|> (incMaster <$> fromMessage m)
   where
    resize Shrink = Tall nmaster delta (max 0 (frac - delta))
    resize Expand = Tall nmaster delta (min 1 (frac + delta))
    incMaster (IncMasterN d) = Tall (max 0 (nmaster + d)) delta frac

  description _ = "Tall"

{- | Tile @n@ windows with @nmaster@ of them in a left column taking @frac@ of
the width. Without masters, or with no more windows than masters, one column
holds every window.
-}
tile :: Rational -> Rect -> Int -> Int -> [Rect]
tile frac r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically n r
  | otherwise = splitVertically nmaster r1 ++ splitVertically (n - nmaster) r2
 where
  (r1, r2) = splitHorizontallyBy frac r
