{- | The workspace, screen, and window zipper behind seki's tiling state,
ported from xmonad 0.18.1's @XMonad.StackSet@ with its semantics.

Windows and screens are river identities, and screens carry no geometry. The
runtime builds every 'StackSet', so this module offers no constructor.
-}
module Seki.Core.StackSet (
  -- * Types
  StackSet,
  current,
  visible,
  hidden,
  Screen (..),
  Workspace (..),
  Stack (..),
  WindowId,
  OutputId,

  -- * Observation
  peek,
  index,
  integrate,
  integrate',
  differentiate,
  currentTag,
  member,
  findTag,
  lookupWorkspace,
  screens,
  workspaces,
  allWindows,

  -- * Focus
  focusUp,
  focusDown,
  focusMaster,
  focusWindow,

  -- * Ordering
  swapUp,
  swapDown,
  swapMaster,
  shiftMaster,

  -- * Membership
  insertUp,
  delete,
  filter,
  shift,
  shiftWin,

  -- * Workspaces
  view,
  greedyView,
  mapWorkspace,
  mapLayout,
) where

import Seki.Internal.Core.StackSet
import Prelude ()
