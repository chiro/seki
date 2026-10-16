{- | The layout that shows only the focused window, at the whole rectangle.

Ported from @XMonad.Layout@ in xmonad 0.18.1 (BSD-3-Clause, see
@THIRD_PARTY_NOTICES.md@).
-}
module Seki.Internal.Layout.Full (
  Full (..),
) where

import Seki.Internal.Core.Layout (LayoutClass)

data Full = Full
  deriving (Eq, Show)

instance LayoutClass Full
