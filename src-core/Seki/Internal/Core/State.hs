module Seki.Internal.Core.State (
  CoreState (..),
  HeadlessWorkspaces (..),
  WorkspaceTagError (..),
  newCoreState,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Seki.Internal.Core.StackSet

-- | The core's workspaces, with or without outputs to show them on.
data CoreState i l
  = HeadlessState (HeadlessWorkspaces i l)
  | -- | The outputs that have no workspace yet, in arrival order.
    AttachedState (StackSet i l) [OutputId]
  deriving (Eq, Show)

-- | Every workspace while no output exists, remembering the current one.
data HeadlessWorkspaces i l = HeadlessWorkspaces
  { headlessCurrent :: Workspace i l
  , headlessHidden :: [Workspace i l]
  , headlessFloating :: Map WindowId FloatGeom
  }
  deriving (Eq, Show)

-- | Why a configured tag list was rejected.
data WorkspaceTagError i = NoWorkspaceTags | DuplicateWorkspaceTag i
  deriving (Eq, Show)

{- | Validate the configured tags and start headless on the first one, with
the rest hidden in order and every workspace empty.
-}
newCoreState :: (Eq i) => l -> [i] -> Either (WorkspaceTagError i) (CoreState i l)
newCoreState _ [] = Left NoWorkspaceTags
newCoreState l tags@(t : ts) = case firstRepeat [] tags of
  Just repeated -> Left (DuplicateWorkspaceTag repeated)
  Nothing -> Right (HeadlessState (HeadlessWorkspaces (empty t) (map empty ts) Map.empty))
 where
  empty i = Workspace i l Nothing
  firstRepeat _ [] = Nothing
  firstRepeat seen (x : xs)
    | x `elem` seen = Just x
    | otherwise = firstRepeat (x : seen) xs
