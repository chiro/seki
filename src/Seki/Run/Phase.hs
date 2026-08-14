{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Phase-safe request plans for river manage and render sequences.
module Seki.Run.Phase (
  ManageM,
  RenderM,
  runManageM,
  runRenderM,
  closeWindow,
  proposeWindowDimensions,
  useWindowCsd,
  useWindowSsd,
  setWindowTiled,
  setWindowCapabilities,
  setWindowDimensionBounds,
  focusWindow,
  clearFocus,
  hideWindow,
  showWindow,
  setWindowBorders,
  setWindowClipBox,
  setWindowContentClipBox,
  setNodePosition,
  placeNodeTop,
  placeNodeBottom,
  placeNodeAbove,
  placeNodeBelow,
)
where

import Data.Int (Int32)
import Data.Sequence (Seq, (><))
import Data.Sequence qualified as Seq
import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Wire.Binding (Interface, RequestEncoder)
import Seki.Wire.Connection (Connection, ConnectionError, submitRequest)
import Seki.Wire.Connection.ObjectTable (Object, objectId)
import Seki.Wire.Transport (OwnedFd)

data ManagePhase

data RenderPhase

-- | A pure, ordered plan containing only manage-sequence operations.
type ManageM = PhaseM ManagePhase

-- | A pure, ordered plan containing only render-sequence operations.
type RenderM = PhaseM RenderPhase

data PlannedRequest where
  PlannedRequest ::
    Interface interface ->
    Object interface ->
    RequestEncoder OwnedFd ->
    PlannedRequest

data PhasePlan a = PhasePlan (Seq PlannedRequest) a

newtype PhaseM phase a = PhaseM (PhasePlan a)

instance Functor (PhaseM phase) where
  fmap transform (PhaseM (PhasePlan requests value)) =
    PhaseM (PhasePlan requests (transform value))

instance Applicative (PhaseM phase) where
  pure = PhaseM . PhasePlan Seq.empty
  PhaseM (PhasePlan firstRequests transform)
    <*> PhaseM (PhasePlan secondRequests value) =
      PhaseM (PhasePlan (firstRequests >< secondRequests) (transform value))

instance Monad (PhaseM phase) where
  PhaseM (PhasePlan firstRequests value) >>= next =
    let PhaseM (PhasePlan secondRequests result) = next value
     in PhaseM (PhasePlan (firstRequests >< secondRequests) result)

{- | Execute a manage plan in request order. Runtime code calls this only while
the checked sequence state is 'Seki.Run.Sequence.Managing'.
-}
runManageM :: Connection -> ManageM a -> IO (Either ConnectionError a)
runManageM = runPhaseM

{- | Execute a render plan in request order. Runtime code calls this only while
the checked sequence state is 'Seki.Run.Sequence.Rendering'.
-}
runRenderM :: Connection -> RenderM a -> IO (Either ConnectionError a)
runRenderM = runPhaseM

runPhaseM :: Connection -> PhaseM phase a -> IO (Either ConnectionError a)
runPhaseM connection (PhaseM (PhasePlan requests result)) =
  submitAll requests
 where
  submitAll remaining = case Seq.viewl remaining of
    Seq.EmptyL -> pure (Right result)
    PlannedRequest interface object request Seq.:< rest ->
      submitRequest connection interface object request >>= \case
        Left errorValue -> pure (Left errorValue)
        Right () -> submitAll rest

phaseRequest ::
  Interface interface ->
  Object interface ->
  RequestEncoder OwnedFd ->
  PhaseM phase ()
phaseRequest interface object request =
  PhaseM (PhasePlan (Seq.singleton (PlannedRequest interface object request)) ())

-- | Ask a window to close. This modifies window-management state.
closeWindow :: Object River.RiverWindowV1 -> ManageM ()
closeWindow window =
  phaseRequest River.riverWindowV1Interface window River.encodeRiverWindowV1Close

proposeWindowDimensions :: Object River.RiverWindowV1 -> Int32 -> Int32 -> ManageM ()
proposeWindowDimensions window width height =
  phaseRequest
    River.riverWindowV1Interface
    window
    (River.encodeRiverWindowV1ProposeDimensions width height)

useWindowCsd :: Object River.RiverWindowV1 -> ManageM ()
useWindowCsd window =
  phaseRequest River.riverWindowV1Interface window River.encodeRiverWindowV1UseCsd

useWindowSsd :: Object River.RiverWindowV1 -> ManageM ()
useWindowSsd window =
  phaseRequest River.riverWindowV1Interface window River.encodeRiverWindowV1UseSsd

setWindowTiled :: Object River.RiverWindowV1 -> River.RiverWindowV1EdgesFlags -> ManageM ()
setWindowTiled window edges =
  phaseRequest River.riverWindowV1Interface window (River.encodeRiverWindowV1SetTiled edges)

setWindowCapabilities ::
  Object River.RiverWindowV1 ->
  River.RiverWindowV1CapabilitiesFlags ->
  ManageM ()
setWindowCapabilities window capabilities =
  phaseRequest
    River.riverWindowV1Interface
    window
    (River.encodeRiverWindowV1SetCapabilities capabilities)

setWindowDimensionBounds :: Object River.RiverWindowV1 -> Int32 -> Int32 -> ManageM ()
setWindowDimensionBounds window maximumWidth maximumHeight =
  phaseRequest
    River.riverWindowV1Interface
    window
    (River.encodeRiverWindowV1SetDimensionBounds maximumWidth maximumHeight)

focusWindow :: Object River.RiverSeatV1 -> Object River.RiverWindowV1 -> ManageM ()
focusWindow seat window =
  phaseRequest
    River.riverSeatV1Interface
    seat
    (River.encodeRiverSeatV1FocusWindow (objectId window))

clearFocus :: Object River.RiverSeatV1 -> ManageM ()
clearFocus seat =
  phaseRequest River.riverSeatV1Interface seat River.encodeRiverSeatV1ClearFocus

hideWindow :: Object River.RiverWindowV1 -> RenderM ()
hideWindow window =
  phaseRequest River.riverWindowV1Interface window River.encodeRiverWindowV1Hide

showWindow :: Object River.RiverWindowV1 -> RenderM ()
showWindow window =
  phaseRequest River.riverWindowV1Interface window River.encodeRiverWindowV1Show

setWindowBorders ::
  Object River.RiverWindowV1 ->
  River.RiverWindowV1EdgesFlags ->
  Int32 ->
  Word32 ->
  Word32 ->
  Word32 ->
  Word32 ->
  RenderM ()
setWindowBorders window edges width red green blue alpha =
  phaseRequest
    River.riverWindowV1Interface
    window
    (River.encodeRiverWindowV1SetBorders edges width red green blue alpha)

setWindowClipBox ::
  Object River.RiverWindowV1 ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  RenderM ()
setWindowClipBox window x y width height =
  phaseRequest
    River.riverWindowV1Interface
    window
    (River.encodeRiverWindowV1SetClipBox x y width height)

setWindowContentClipBox ::
  Object River.RiverWindowV1 ->
  Int32 ->
  Int32 ->
  Int32 ->
  Int32 ->
  RenderM ()
setWindowContentClipBox window x y width height =
  phaseRequest
    River.riverWindowV1Interface
    window
    (River.encodeRiverWindowV1SetContentClipBox x y width height)

setNodePosition :: Object River.RiverNodeV1 -> Int32 -> Int32 -> RenderM ()
setNodePosition node x y =
  phaseRequest River.riverNodeV1Interface node (River.encodeRiverNodeV1SetPosition x y)

placeNodeTop :: Object River.RiverNodeV1 -> RenderM ()
placeNodeTop node =
  phaseRequest River.riverNodeV1Interface node River.encodeRiverNodeV1PlaceTop

placeNodeBottom :: Object River.RiverNodeV1 -> RenderM ()
placeNodeBottom node =
  phaseRequest River.riverNodeV1Interface node River.encodeRiverNodeV1PlaceBottom

placeNodeAbove :: Object River.RiverNodeV1 -> Object River.RiverNodeV1 -> RenderM ()
placeNodeAbove node other =
  phaseRequest
    River.riverNodeV1Interface
    node
    (River.encodeRiverNodeV1PlaceAbove (objectId other))

placeNodeBelow :: Object River.RiverNodeV1 -> Object River.RiverNodeV1 -> RenderM ()
placeNodeBelow node other =
  phaseRequest
    River.riverNodeV1Interface
    node
    (River.encodeRiverNodeV1PlaceBelow (objectId other))
