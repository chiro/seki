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
  kill,
) where

import Control.Monad (ap)
import Seki.Internal.Core.Layout (LayoutClass, Message, SomeMessage (..), messageCurrentLayout)
import Seki.Internal.Core.StackSet (StackSet, WindowId, peek)
import Seki.Internal.Core.State (CoreState (..))

-- | An effect an action asks the runtime to perform.
data Intent
  = -- | Launch a shell command.
    Spawn String
  | -- | Ask river to close the window.
    CloseWindow WindowId
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

-- | Change the attached StackSet. Headless there is none, so nothing changes.
windows :: (StackSet i l -> StackSet i l) -> Action i l ()
windows f = modify $ \case
  AttachedState ss unassigned -> AttachedState (f ss) unassigned
  st@HeadlessState{} -> st

-- | Send a message to the current workspace's layout; nothing happens headless.
sendMessage :: (LayoutClass l, Message m) => m -> Action i l ()
sendMessage m = windows (messageCurrentLayout (SomeMessage m))

-- | Launch a shell command once the action has run.
spawn :: String -> Action i l ()
spawn cmd = record (const [Spawn cmd])

{- | Ask river to close the window focused at this point in the action. The
window stays managed until river reports it closed.
-}
kill :: Action i l ()
kill = record $ \case
  AttachedState ss _ | Just w <- peek ss -> [CloseWindow w]
  _ -> []
