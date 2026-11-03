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
  drainCoreWithScratchpads,
) where

import Control.Monad (mfilter)
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Seki.Internal.Core.Action (Action, Intent, runAction, scratchpadIntents)
import Seki.Internal.Core.ManageHook (ManageHook, WindowProps)
import Seki.Internal.Core.PointerOp (OpId, OpRequest, SeatId)
import Seki.Internal.Core.PointerOp qualified as PointerOp
import Seki.Internal.Core.Scratchpad (closePad, managePad)
import Seki.Internal.Core.Scratchpad.Types (Scratchpad)
import Seki.Internal.Core.StackSet (OutputId, WindowId)
import Seki.Internal.Core.State (CoreState (..), addOutput, applyOpInput, focusWindowIn, normalizeCore, overWorkspaces, removeOutput, startPointerOp)

-- | A change river reported, or a key binding's action.
data CoreEvent i l
  = WindowCreated WindowId WindowProps
  | WindowClosed WindowId
  | OutputCreated OutputId
  | OutputRemoved OutputId
  | -- | A click or other interaction with the window (click-to-focus).
    WindowInteraction WindowId
  | -- | The client asked to be fullscreen. Its output hint is ignored: the
    -- window goes fullscreen on its own workspace's output.
    FullscreenRequested WindowId
  | ExitFullscreenRequested WindowId
  | -- | A pointer binding press, or a client's move or resize request.
    StartOp OpRequest
  | -- | Input for the op river was running on the seat when the drain began.
    OpInput SeatId (Maybe OpId) PointerOp.OpInput
  | SeatRemoved SeatId
  | RunAction (Action i l ())

{- | Apply one event, returning the new state and the intents it recorded. Every
event ends by raising a focused float, dropping fullscreen windows that no
longer focus their workspace, and ending an op whose window no longer allows it.
-}
stepCore :: (Eq i) => ManageHook i l -> CoreEvent i l -> CoreState i l -> (CoreState i l, [Intent])
stepCore = stepCoreWithScratchpads []

stepCoreWithScratchpads :: (Eq i) => [Scratchpad] -> ManageHook i l -> CoreEvent i l -> CoreState i l -> (CoreState i l, [Intent])
stepCoreWithScratchpads definitions hook event = first normalizeCore . reduce definitions hook event

reduce :: (Eq i) => [Scratchpad] -> ManageHook i l -> CoreEvent i l -> CoreState i l -> (CoreState i l, [Intent])
reduce definitions hook event st = case event of
  WindowCreated w props -> let (next, effects) = managePad definitions hook w props st in (next, scratchpadIntents effects)
  WindowClosed w -> (closePad w st, [])
  OutputCreated o -> (overWorkspaces (addOutput o) st, [])
  OutputRemoved o -> (overWorkspaces (removeOutput o) st, [])
  WindowInteraction w -> (overWorkspaces (focusWindowIn w) st, [])
  FullscreenRequested w -> (st{coreFullscreen = Set.insert w (coreFullscreen st)}, [])
  ExitFullscreenRequested w -> (st{coreFullscreen = Set.delete w (coreFullscreen st)}, [])
  StartOp request -> (startPointerOp request st, [])
  OpInput seat op input -> (applyOpInput seat op input st, [])
  SeatRemoved seat -> (st{corePointerOp = mfilter ((/= seat) . PointerOp.opSeat) (corePointerOp st)}, [])
  RunAction action -> let ((), st', intents) = runAction action st in (st', intents)

-- | Apply events in arrival order, concatenating their intents in order.
drainCore :: (Eq i) => ManageHook i l -> [CoreEvent i l] -> CoreState i l -> (CoreState i l, [Intent])
drainCore = drainCoreWithScratchpads []

-- | Configured matching and ordinary management share the same reducer.
drainCoreWithScratchpads :: (Eq i) => [Scratchpad] -> ManageHook i l -> [CoreEvent i l] -> CoreState i l -> (CoreState i l, [Intent])
drainCoreWithScratchpads definitions hook events st0 = foldl' step (st0, []) events
 where
  step (st, intents) event = let (next, effects) = stepCoreWithScratchpads definitions hook event st in (next, intents ++ effects)
