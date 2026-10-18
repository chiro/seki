{- | Core events and the reducer that applies them to 'CoreState'.

Output geometry, usable area, and layer-shell focus are not core events: the
StackSet holds no geometry, and the scene planner and reconciler read those
facts from the runtime inventory. Pointer enter and leave produce no event in
M1.
-}
module Seki.Internal.Core.Event (
  CoreEvent (..),
  stepCore,
  drainCore,
) where

import Data.List (foldl')
import Data.Monoid (Endo (..))
import Seki.Internal.Core.Action (Action, Intent, runAction)
import Seki.Internal.Core.ManageHook (ManageHook, WindowProps, runQuery)
import Seki.Internal.Core.StackSet (OutputId, WindowId)
import Seki.Internal.Core.State (CoreState, addOutput, focusWindowIn, manageWindow, overWorkspaces, removeOutput, unmanageWindow)

-- | A change river reported, or a key binding's action.
data CoreEvent i l
  = WindowCreated WindowId WindowProps
  | WindowClosed WindowId
  | OutputCreated OutputId
  | OutputRemoved OutputId
  | -- | A click or other interaction with the window (click-to-focus).
    WindowInteraction WindowId
  | RunAction (Action i l ())

-- | Apply one event, returning the new state and the intents it recorded.
stepCore :: (Eq i) => ManageHook i l -> CoreEvent i l -> CoreState i l -> (CoreState i l, [Intent])
stepCore hook event st = case event of
  WindowCreated w props -> (overWorkspaces (manageWindow (appEndo (runQuery hook w props)) w) st, [])
  WindowClosed w -> (overWorkspaces (unmanageWindow w) st, [])
  OutputCreated o -> (overWorkspaces (addOutput o) st, [])
  OutputRemoved o -> (overWorkspaces (removeOutput o) st, [])
  WindowInteraction w -> (overWorkspaces (focusWindowIn w) st, [])
  RunAction action -> let ((), st', intents) = runAction action st in (st', intents)

-- | Apply events in arrival order, concatenating their intents in order.
drainCore :: (Eq i) => ManageHook i l -> [CoreEvent i l] -> CoreState i l -> (CoreState i l, [Intent])
drainCore hook events st0 = foldl' step (st0, []) events
 where
  step (st, intents) event = let (st', new) = stepCore hook event st in (st', intents ++ new)
