{-# LANGUAGE LambdaCase #-}

{- | Pure user actions over the core state.

An 'Action' changes 'CoreState' and records 'Intent's for the runtime to carry
out later, in order. It cannot run IO: launching a process or asking river to
close a window happens only when the runtime executes the recorded intents.
-}
module Seki.Internal.Core.Action (
  Intent (..),
  Action,
  runAction,
  windows,
  sendMessage,
  spawn,
  spawnOnce,
  kill,
  withFocused,
  float,
  sink,
  toggleFullscreen,
  restart,
) where

import Control.Monad (ap)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Seki.Internal.Core.Layout (LayoutClass, Message, SomeMessage (..), messageCurrentLayout)
import Seki.Internal.Core.StackSet (FloatGeom (..), StackSet (..), WindowId, member, peek)
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Internal.Core.State (CoreState (..), Workspaces (..), overWorkspaces)

-- | An effect an action asks the runtime to perform.
data Intent
  = -- | Launch a shell command.
    Spawn String
  | -- | Ask river to close the window.
    CloseWindow WindowId
  | ReclaimFocus
  | -- | Replace this seki process with the executable it started from, keeping the window state.
    Restart
  deriving (Eq, Show)

-- | A state change on 'CoreState' that also records 'Intent's in order.
newtype Action i l a = Action (CoreState i l -> (a, CoreState i l, [Intent]))

instance Functor (Action i l) where
  fmap f (Action g) = Action $ \s -> let (a, s', is) = g s in (f a, s', is)

instance Applicative (Action i l) where
  pure a = Action (a,,[])
  (<*>) = ap

instance Monad (Action i l) where
  Action g >>= k = Action $ \s ->
    let (a, s1, is1) = g s
        (b, s2, is2) = runAction (k a) s1
     in (b, s2, is1 ++ is2)

-- | Run an action, returning its result, the new state, and its intents in order.
runAction :: Action i l a -> CoreState i l -> (a, CoreState i l, [Intent])
runAction (Action g) = g

modify :: (CoreState i l -> CoreState i l) -> Action i l ()
modify f = Action (\s -> ((), f s, []))

record :: (CoreState i l -> [Intent]) -> Action i l ()
record f = Action (\s -> ((), s, f s))

{- | Change the attached StackSet and reclaim non-exclusive layer focus.
Even an identity change requests focus. Headless state has no StackSet to change.
-}
windows :: (StackSet i l -> StackSet i l) -> Action i l ()
windows f = do
  record (const [ReclaimFocus])
  modifyWindows f

modifyWindows :: (StackSet i l -> StackSet i l) -> Action i l ()
modifyWindows f = modify . overWorkspaces $ \case
  AttachedState ss unassigned -> AttachedState (f ss) unassigned
  st@HeadlessState{} -> st

-- | Send a message to the current workspace's layout; nothing happens headless.
sendMessage :: (LayoutClass l, Message m) => m -> Action i l ()
sendMessage m = modifyWindows (messageCurrentLayout (SomeMessage m))

-- | Launch a shell command once the action has run.
spawn :: String -> Action i l ()
spawn cmd = record (const [Spawn cmd])

{- | Launch a shell command unless 'spawnOnce' already launched the same
command string, as xmonad-contrib's @SpawnOnce@ does. The recorded set is
part of the core state, not of the process.
-}
spawnOnce :: String -> Action i l ()
spawnOnce cmd = Action $ \st ->
  if cmd `Set.member` coreSpawnedOnce st
    then ((), st, [])
    else ((), st{coreSpawnedOnce = Set.insert cmd (coreSpawnedOnce st)}, [Spawn cmd])

-- | Run an action on the window focused at this point, if there is one.
withFocused :: (WindowId -> Action i l ()) -> Action i l ()
withFocused f = Action (\st -> runAction (maybe (pure ()) f (focused st)) st)
 where
  focused st = case coreWorkspaces st of
    AttachedState ss _ -> peek ss
    HeadlessState{} -> Nothing

{- | Float a managed window. A tiled window gets no stored box, so its client
picks its size and it is centered on its output; a floating window keeps its
box. xmonad floats a tiled window at its current rectangle instead.
-}
float :: WindowId -> Action i l ()
float w = windows $ \ss -> if member w ss && Map.notMember w (floating ss) then StackSet.float w ClientSized ss else ss

-- | Tile a floating window again.
sink :: WindowId -> Action i l ()
sink = windows . StackSet.sink

{- | Ask river to close the window focused at this point in the action. The
window stays managed until river reports it closed.
-}
kill :: Action i l ()
kill = record $ \st -> case coreWorkspaces st of
  AttachedState ss _ | Just w <- peek ss -> [CloseWindow w]
  _ -> []

{- | Make a window fullscreen on its workspace's output, or end its fullscreen.
Only a workspace's focused window can be fullscreen, so for any other window
this does nothing.
-}
toggleFullscreen :: WindowId -> Action i l ()
toggleFullscreen w = modify $ \st ->
  st{coreFullscreen = (if w `Set.member` coreFullscreen st then Set.delete else Set.insert) w (coreFullscreen st)}

{- | Restart seki in place once the action has run, keeping workspaces,
stacks, focus, layouts, floats, fullscreen windows, and the spawned-once set.
-}
restart :: Action i l ()
restart = record (const [Restart])
