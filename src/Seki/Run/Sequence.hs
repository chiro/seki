-- | Pure ordering rules for river manage and render sequences.
module Seki.Run.Sequence (
  RiverSequenceState,
  RiverSequencePhase (..),
  RiverSequenceOperation (..),
  RiverSequenceCompletion (..),
  RiverSequenceError (..),
  initialRiverSequenceState,
  riverSequencePhase,
  startManageSequence,
  startRenderSequence,
  completeRiverSequence,
  managementChangesAllowed,
  renderingChangesAllowed,
)
where

-- | The externally observable phase of river's sequence protocol.
data RiverSequencePhase
  = AwaitingInitialManage
  | Managing
  | AwaitingRequiredRender
  | Rendering
  | AwaitingManageOrRender
  deriving (Eq, Show)

{- | Opaque sequence state. Construct it with 'initialRiverSequenceState' and
advance it only through the checked transition functions.
-}
newtype RiverSequenceState = RiverSequenceState RiverSequencePhase
  deriving (Eq, Show)

-- | An operation checked by the sequence state machine.
data RiverSequenceOperation
  = StartManageSequence
  | StartRenderSequence
  | CompleteActiveSequence
  deriving (Eq, Show)

-- | The protocol request required to complete the active sequence.
data RiverSequenceCompletion
  = SendManageFinish
  | SendRenderFinish
  deriving (Eq, Show)

-- | A server event or client completion occurred outside its valid phase.
data RiverSequenceError
  = InvalidRiverSequenceTransition RiverSequencePhase RiverSequenceOperation
  deriving (Eq, Show)

-- | State before river has delivered the first @manage_start@ or @render_start@ event.
initialRiverSequenceState :: RiverSequenceState
initialRiverSequenceState = RiverSequenceState AwaitingInitialManage

riverSequencePhase :: RiverSequenceState -> RiverSequencePhase
riverSequencePhase (RiverSequenceState phase) = phase

-- | Enter a manage sequence after receiving @manage_start@.
startManageSequence :: RiverSequenceState -> Either RiverSequenceError RiverSequenceState
startManageSequence state = case riverSequencePhase state of
  AwaitingInitialManage -> Right (RiverSequenceState Managing)
  AwaitingManageOrRender -> Right (RiverSequenceState Managing)
  phase -> invalidTransition phase StartManageSequence

{- | Enter a render sequence after receiving @render_start@. One can come
before the first manage sequence: river keeps a single sequence state across
window managers, so a manager that binds while its predecessor's render is
pending starts with a render (M2-D08).
-}
startRenderSequence :: RiverSequenceState -> Either RiverSequenceError RiverSequenceState
startRenderSequence state = case riverSequencePhase state of
  AwaitingInitialManage -> Right (RiverSequenceState Rendering)
  AwaitingRequiredRender -> Right (RiverSequenceState Rendering)
  AwaitingManageOrRender -> Right (RiverSequenceState Rendering)
  phase -> invalidTransition phase StartRenderSequence

{- | Complete the active sequence.

The returned value identifies the request the runtime must submit. After a
manage sequence river requires at least one render sequence; after a render
sequence it may begin either another render sequence or a new manage sequence.
-}
completeRiverSequence ::
  RiverSequenceState ->
  Either RiverSequenceError (RiverSequenceCompletion, RiverSequenceState)
completeRiverSequence state = case riverSequencePhase state of
  Managing -> Right (SendManageFinish, RiverSequenceState AwaitingRequiredRender)
  Rendering -> Right (SendRenderFinish, RiverSequenceState AwaitingManageOrRender)
  phase -> invalidTransition phase CompleteActiveSequence

-- | Whether window-management requests are legal in the current phase.
managementChangesAllowed :: RiverSequenceState -> Bool
managementChangesAllowed state = riverSequencePhase state == Managing

-- | Whether rendering requests are legal in the current phase.
renderingChangesAllowed :: RiverSequenceState -> Bool
renderingChangesAllowed state = case riverSequencePhase state of
  Managing -> True
  Rendering -> True
  _ -> False

invalidTransition ::
  RiverSequencePhase ->
  RiverSequenceOperation ->
  Either RiverSequenceError value
invalidTransition phase operation =
  Left (InvalidRiverSequenceTransition phase operation)
