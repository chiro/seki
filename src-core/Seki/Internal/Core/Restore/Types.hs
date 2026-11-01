module Seki.Internal.Core.Restore.Types (WindowKey (..), OutputKey (..), SavedFloat (..), SavedWorkspace (..), RestoreState (..), SavedScratchpad (..)) where

import Data.Int (Int32)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Internal.Core.StackSet (Stack)

-- | A river window identifier (version 4). Any text reads, since a key that matches no live window is dropped at restore.
newtype WindowKey = WindowKey Text deriving (Eq, Ord, Show, Read)

-- | A @wl_output@ global name, which stays the same across a window-manager reconnect.
newtype OutputKey = OutputKey Word32 deriving (Eq, Ord, Show, Read)

{- | A float's stored box as plain numbers: x, y, width, and height relative
to its output. Restore rebuilds the box through the geometry constructors,
so a file cannot bypass their checks.
-}
data SavedFloat = SavedClientSized | SavedFloatAt Int32 Int32 Int32 Int32
  deriving (Eq, Show, Read)

data SavedWorkspace i = SavedWorkspace
  { savedTag :: i
  , savedOutput :: Maybe OutputKey
  -- ^ The output that showed it, or 'Nothing' when hidden or headless.
  , savedLayout :: String
  -- ^ 'show' of the layout, read on its own so an unreadable layout falls back alone.
  , savedStack :: Maybe (Stack WindowKey)
  }
  deriving (Eq, Show, Read)

{- | Everything a restart carries over. Workspaces come in StackSet order:
the current one first (the remembered one when headless), then the visible
ones, then the hidden ones. Unassigned outputs and the pointer op are left
out: replaying the new connection's outputs rebuilds the first, and an op
does not survive a reconnect.
-}
data RestoreState i = RestoreState
  { savedWorkspaces :: [SavedWorkspace i]
  , savedFloats :: [(WindowKey, SavedFloat)]
  -- ^ Bottom to top, so this one list is both the floating map and the raise order.
  , savedFullscreen :: Set WindowKey
  , savedSpawnedOnce :: Set String
  , savedScratchpads :: [SavedScratchpad]
  }
  deriving (Eq, Show, Read)

-- | Owned windows only. Pending and latched launch attempts are never saved.
data SavedScratchpad = SavedScratchpad String WindowKey Bool SavedFloat (Maybe WindowKey)
  deriving (Eq, Show, Read)
