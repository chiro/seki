-- | The exact v1 Read representation, retained only at the decode boundary.
module Seki.Internal.Core.Restore.Legacy (RestoreState (..)) where

import Data.Set (Set)
import Seki.Internal.Core.Restore.Types (SavedFloat, SavedWorkspace, WindowKey)

data RestoreState i = RestoreState
  { savedWorkspaces :: [SavedWorkspace i]
  , savedFloats :: [(WindowKey, SavedFloat)]
  , savedFullscreen :: Set WindowKey
  , savedSpawnedOnce :: Set String
  }
  deriving (Eq, Show, Read)
