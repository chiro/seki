module Seki.Internal.Core.State (
  CoreState (..),
  HeadlessWorkspaces (..),
  WorkspaceTagError (..),
  newCoreState,
  addOutput,
  removeOutput,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Seki.Internal.Core.StackSet (FloatGeom, OutputId, Screen (..), StackSet (..), WindowId, Workspace (..))

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

{- | Give a new output the remembered workspace when headless, otherwise the
first hidden workspace, or leave it unassigned when none is hidden. Adding a
known output changes nothing.
-}
addOutput :: OutputId -> CoreState i l -> CoreState i l
addOutput o (HeadlessState (HeadlessWorkspaces c hs f)) = AttachedState (StackSet (Screen c o) [] hs f) []
addOutput o st@(AttachedState ss unassigned)
  | o `elem` map output (current ss : visible ss) || o `elem` unassigned = st
  | h : hs <- hidden ss = AttachedState ss{visible = visible ss ++ [Screen h o], hidden = hs} unassigned
  | otherwise = AttachedState ss (unassigned ++ [o])

{- | Forget an output. A removed screen's workspace becomes the first hidden
one, and unassigned outputs then take hidden workspaces in order. The first
surviving screen becomes current; with none left the state goes headless and
remembers the removed workspace. Removing an unknown output changes nothing.
-}
removeOutput :: OutputId -> CoreState i l -> CoreState i l
removeOutput _ st@HeadlessState{} = st
removeOutput o st@(AttachedState ss unassigned)
  | o `elem` unassigned = AttachedState ss (filter (/= o) unassigned)
  | otherwise = case break ((== o) . output) (current ss : visible ss) of
      (_, []) -> st
      (before, Screen w _ : after) ->
        let freed = w : hidden ss
            assigned = zipWith (flip Screen) unassigned freed
            n = length assigned
         in case before ++ after ++ assigned of
              [] -> HeadlessState (HeadlessWorkspaces w (hidden ss) (floating ss))
              c : vs -> AttachedState ss{current = c, visible = vs, hidden = drop n freed} (drop n unassigned)
