{- | The state a restart carries to the next seki process (M2-D06).

The saved form names windows by their river identifier and outputs by their
@wl_output@ global name, the two keys that survive a window-manager
reconnect. It has no 'WindowId' or 'OutputId' field, since those are wire
object ids that mean nothing to the next process. The file is a version
header followed by 'show' of a 'RestoreState'.
-}
module Seki.Internal.Core.Restore (
  WindowKey (..),
  OutputKey (..),
  SavedFloat (..),
  SavedWorkspace (..),
  RestoreState (..),
  RestoreError (..),
  restoreVersion,
  encodeRestoreState,
  decodeRestoreState,
) where

import Data.Int (Int32)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Internal.Core.StackSet (Stack)
import Text.Read (readMaybe)

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
  }
  deriving (Eq, Show, Read)

-- | Why a saved file was not used. Either way seki starts fresh.
data RestoreError = RestoreUnknownVersion String | RestoreUnreadable
  deriving (Eq, Show)

restoreVersion :: Int
restoreVersion = 1

-- | The header line @seki-restore 1@, then 'show' of the state.
encodeRestoreState :: (Show i) => RestoreState i -> String
encodeRestoreState st = header ++ "\n" ++ show st

{- | Read a file written by 'encodeRestoreState'. A header naming another
version is 'RestoreUnknownVersion', and anything else that does not read is
'RestoreUnreadable'.
-}
decodeRestoreState :: (Read i) => String -> Either RestoreError (RestoreState i)
decodeRestoreState text = case break (== '\n') text of
  (line, '\n' : body)
    | ["seki-restore", version] <- words line ->
        if version == show restoreVersion
          then maybe (Left RestoreUnreadable) Right (readMaybe body)
          else Left (RestoreUnknownVersion version)
  _ -> Left RestoreUnreadable

header :: String
header = "seki-restore " ++ show restoreVersion
