{- | The core's workspaces across output hotplug, including the time when no
output exists. An attached state keeps an xmonad 'StackSet' unchanged; the
headless state stands in for it, since a StackSet always has a current
screen.

Every transition keeps these invariants:

* every configured workspace appears exactly once;
* every window appears in at most one workspace stack;
* current and visible screens have distinct outputs and workspaces;
* hidden workspaces have no output;
* the focused window, when present, is a member of the current workspace;
* an output waits unassigned only while no workspace is hidden.

Hotplug rules:

* The first output shows the remembered workspace. Each later output shows
  the first hidden workspace, appended after the visible screens, or waits
  unassigned in arrival order when none is hidden.
* Removing a screen's output puts its workspace at the front of the hidden
  workspaces, then pairs unassigned outputs with hidden workspaces in order.
  The first surviving screen becomes current. With no screen left the state
  goes headless and remembers the removed workspace.
* Adding a known output or removing an unknown one changes nothing.

Managing a window runs its manage hook in either state. Headless, the hook
sees the workspaces as a StackSet whose one screen shows the remembered
workspace on an output no river object can have, and the result maps back:
the current screen's workspace becomes the remembered one and every other
workspace is hidden, in order. So a hook places a window the same way whether
or not an output exists when it is created: @doShift@ to a hidden workspace
leaves it hidden until that workspace is shown.
-}
module Seki.Internal.Core.State (
  CoreState (..),
  Workspaces (..),
  HeadlessWorkspaces (..),
  WorkspaceTagError (..),
  newCoreState,
  fromWorkspaces,
  overWorkspaces,
  addOutput,
  removeOutput,
  manageWindow,
  unmanageWindow,
  focusWindowIn,
  syncRaiseOrder,
  syncFullscreen,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Seki.Internal.Core.StackSet (
  FloatGeom,
  OutputId (..),
  Screen (..),
  StackSet (..),
  WindowId,
  Workspace (..),
  delete,
  focusWindow,
  insertUp,
  member,
  peek,
  screens,
 )
import Seki.Internal.Core.StackSet qualified as StackSet

-- | Everything the core reduces events into.
data CoreState i l = CoreState
  { coreWorkspaces :: Workspaces i l
  , coreSpawnedOnce :: Set String
  -- ^ The commands 'Seki.Internal.Core.Action.spawnOnce' has launched.
  , coreRaiseOrder :: [WindowId]
  -- ^ Every floating window once, bottom to top. See 'syncRaiseOrder'.
  , coreFullscreen :: Set WindowId
  -- ^ The fullscreen windows. See 'syncFullscreen'.
  }
  deriving (Eq, Show)

-- | The core's workspaces, with or without outputs to show them on.
data Workspaces i l
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
  Nothing -> Right (fromWorkspaces (HeadlessState (HeadlessWorkspaces (empty t) (map empty ts) Map.empty)))
 where
  empty i = Workspace i l Nothing
  firstRepeat _ [] = Nothing
  firstRepeat seen (x : xs)
    | x `elem` seen = Just x
    | otherwise = firstRepeat (x : seen) xs

-- | A core state over these workspaces with nothing else recorded yet.
fromWorkspaces :: Workspaces i l -> CoreState i l
fromWorkspaces ws = CoreState ws Set.empty [] Set.empty

-- | Change the workspaces and keep the rest of the core state.
overWorkspaces :: (Workspaces i l -> Workspaces i l) -> CoreState i l -> CoreState i l
overWorkspaces f st = st{coreWorkspaces = f (coreWorkspaces st)}

{- | Give a new output the remembered workspace when headless, otherwise the
first hidden workspace, or leave it unassigned when none is hidden. Adding a
known output changes nothing.
-}
addOutput :: OutputId -> Workspaces i l -> Workspaces i l
addOutput o (HeadlessState (HeadlessWorkspaces c hs f)) = AttachedState (StackSet (Screen c o) [] hs f) []
addOutput o st@(AttachedState ss unassigned)
  | o `elem` map output (screens ss) || o `elem` unassigned = st
  | h : hs <- hidden ss = AttachedState ss{visible = visible ss ++ [Screen h o], hidden = hs} unassigned
  | otherwise = AttachedState ss (unassigned ++ [o])

{- | Forget an output. A removed screen's workspace becomes the first hidden
one, and unassigned outputs then take hidden workspaces in order. The first
surviving screen becomes current; with none left the state goes headless and
remembers the removed workspace. Removing an unknown output changes nothing.
-}
removeOutput :: OutputId -> Workspaces i l -> Workspaces i l
removeOutput _ st@HeadlessState{} = st
removeOutput o st@(AttachedState ss unassigned)
  | o `elem` unassigned = AttachedState ss (filter (/= o) unassigned)
  | otherwise = case break ((== o) . output) (screens ss) of
      (_, []) -> st
      (before, Screen w _ : after) ->
        let freed = w : hidden ss
            assigned = zipWith (flip Screen) unassigned freed
            n = length assigned
         in case before ++ after ++ assigned of
              [] -> HeadlessState (HeadlessWorkspaces w (hidden ss) (floating ss))
              c : vs -> AttachedState ss{current = c, visible = vs, hidden = drop n freed} (drop n unassigned)

{- | xmonad's manage: insert a window above the current workspace's focus and
focus it, then apply the manage hook's change. Headless, the remembered
current workspace takes it, so it shows when an output arrives. A window
already present anywhere changes nothing and skips the hook.
-}
manageWindow :: (StackSet i l -> StackSet i l) -> WindowId -> Workspaces i l -> Workspaces i l
manageWindow hook w = onStackSet (\ss -> if member w ss then ss else hook (insertUp w ss))

onStackSet :: (StackSet i l -> StackSet i l) -> Workspaces i l -> Workspaces i l
onStackSet f (AttachedState ss unassigned) = AttachedState (f ss) unassigned
onStackSet f (HeadlessState (HeadlessWorkspaces c hs fl)) =
  HeadlessState (HeadlessWorkspaces (workspace (current ss)) (map workspace (visible ss) ++ hidden ss) (floating ss))
 where
  ss = f (StackSet (Screen c headlessOutput) [] hs fl)

-- | The output headless workspaces stand on while a hook runs. Wayland never allocates object id 0, so no river output has it.
headlessOutput :: OutputId
headlessOutput = OutputId 0

-- | xmonad's delete, from every workspace and the floating map, in either state.
unmanageWindow :: WindowId -> Workspaces i l -> Workspaces i l
unmanageWindow w (AttachedState ss unassigned) = AttachedState (delete w ss) unassigned
unmanageWindow w (HeadlessState (HeadlessWorkspaces c hs f)) =
  HeadlessState (HeadlessWorkspaces (remove c) (map remove hs) (Map.delete w f))
 where
  remove ws = ws{stack = stack ws >>= StackSet.filter (/= w)}

{- | Focus a window and view its workspace, making its output current. Headless
or for an unknown window, nothing changes.
-}
focusWindowIn :: (Eq i) => WindowId -> Workspaces i l -> Workspaces i l
focusWindowIn w (AttachedState ss unassigned) = AttachedState (focusWindow w ss) unassigned
focusWindowIn _ st@HeadlessState{} = st

-- | Focusing a tile leaves the raise order alone (M2-D02). Syncing twice changes nothing more.
syncRaiseOrder :: CoreState i l -> CoreState i l
syncRaiseOrder st = st{coreRaiseOrder = filter (not . isFocused) order ++ filter isFocused order}
 where
  (floats, focused) = case coreWorkspaces st of
    AttachedState ss _ -> (floating ss, peek ss)
    HeadlessState hw -> (headlessFloating hw, Nothing)
  kept = filter (`Map.member` floats) (coreRaiseOrder st)
  order = kept ++ filter (`notElem` kept) (Map.keys floats)
  isFocused w = Just w == focused

{- | Keep only fullscreen windows that are their own workspace's focus, shown
or hidden. So a focus change to another window on the same workspace exits
fullscreen (M2-D03), while viewing another workspace does not, and a window
that is not its workspace's focus cannot enter.
-}
syncFullscreen :: CoreState i l -> CoreState i l
syncFullscreen st = st{coreFullscreen = Set.intersection (coreFullscreen st) focuses}
 where
  focuses = Set.fromList [StackSet.focus s | Just s <- map stack allWorkspaces]
  allWorkspaces = case coreWorkspaces st of
    AttachedState ss _ -> map workspace (screens ss) ++ hidden ss
    HeadlessState hw -> headlessCurrent hw : headlessHidden hw
