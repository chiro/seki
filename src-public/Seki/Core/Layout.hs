{- | Layouts and the messages that change them, ported from xmonad 0.18.1's
@XMonad.Core@ and @XMonad.Layout@.

A layout places a workspace's windows in a rectangle with 'pureLayout' and
answers messages with 'pureMessage'. Combine layouts with '(|||)' and switch
between them with 'NextLayout' and 'FirstLayout'. A layout crops a client that
commits another size to its tile; wrap it in 'tolerant' to show the
committed size instead.
-}
module Seki.Core.Layout (
  -- * Messages
  Message,
  SomeMessage (..),
  fromMessage,
  Resize (..),
  IncMasterN (..),
  ChangeLayout (..),
  LayoutMessages (..),

  -- * Layouts
  SizePolicy (..),
  LayoutClass (..),
  runLayout,
  Mirror (..),
  Choose,
  (|||),

  -- * Layout modifiers
  LayoutModifier (..),
  ModifiedLayout (..),
  Tolerant (..),
  tolerant,
) where

import Seki.Internal.Core.Layout
