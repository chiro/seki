{-# LANGUAGE LambdaCase #-}

{- | Reconcile pure selections against river's dimensions reports.

Every proposal records the inventory revision at submission. A newer report
settles it even when the accepted size differs or repeats the previous size.
River supplies no proposal correlation identifier.

A tile gets all four tiled edges and a float none. A float with no stored box
is proposed zero by zero, so its client picks its size, and every render
centers whatever size the client last committed. That size never becomes
stored geometry, so the window stays centered when its client resizes it.

A fullscreen placement is river's native fullscreen. Each window remembers
what river and its client were last told, so entering, moving to another
output, and exiting each send only the requests that differ, in one manage
sequence. Entering drops the window's proposal state, so @dimensions@ while
fullscreen settle nothing and exit always proposes the tile or float again
(M2-D03). A hidden fullscreen window keeps what river and its client were
told, so a workspace switch sends no fullscreen request, and the window
covers no output because it is hidden. River exits fullscreen by itself when
the output is removed, so such a window is only sent @fullscreen@ for the
output that later shows it, or informed if it leaves the set.
-}
module Seki.Run.Reconcile (
  ReconcileState,
  emptyReconcileState,
  reconcileManage,
  reconcileRender,
  clampToHint,
) where

import Control.Monad (unless, when)
import Data.Either (fromRight)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Seki.Internal.Core.Geometry (Point (..), Rect, Size, mkRect, mkSize, rectHeight, rectOrigin, rectSize, rectWidth, rectX, rectY, sizeHeight, sizeWidth)
import Seki.Internal.Core.Layout (SizePolicy (..))
import Seki.Internal.Core.Scene (BorderColor (..), DesiredScene (..), Placement (..), SceneStyle (..), placementShown, stackWindows)
import Seki.Internal.Core.StackSet (OutputId (..), WindowId (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory (RiverDimensionsHint (..), RiverInventory, RiverOutputState (..), RiverSize (..), RiverWindowState (..), lookupRiverOutput, lookupRiverWindow, riverInventoryWindows)
import Seki.Run.Phase (ManageM, RenderM, clearFocus, exitWindowFullscreen, focusWindow, fullscreenWindow, hideWindow, informWindowFullscreen, informWindowNotFullscreen, placeNodeTop, proposeWindowDimensions, setDefaultLayerShellOutput, setNodePosition, setWindowBorders, setWindowCapabilities, setWindowTiled, showWindow, useWindowSsd)
import Seki.Run.Runtime (RenderOutcome (..), RiverLayerFocus (..), RiverRuntimeSnapshot (..), RiverSeatBindings (..))
import Seki.Wire.Connection.ObjectTable (Object, objectId)
import Seki.Wire.Types (ObjectId, mkObjectId, objectIdValue)

data Proposal = NoProposal | AwaitingDimensions Size Integer deriving (Eq, Show)

-- | What river and the window's client were last told about fullscreen.
data FullscreenApplied
  = Windowed
  | FullscreenOn (Object River.RiverOutputV1)
  | -- | River dropped fullscreen with its output, and the client still believes it is fullscreen.
    Released
  deriving (Eq, Show)

-- | What a placement asks of fullscreen: none, whatever river and the client already have, or covering this output.
data FullscreenGoal = NotFullscreen | KeepFullscreen | CoverOutput (Object River.RiverOutputV1)

-- | What a render shows: nothing, a settled box, or river's fullscreen cover.
data RenderTarget = Hidden | ShownAt Rect | Covering deriving (Eq, Show)

data Applied = Applied
  { appliedWindow :: Object River.RiverWindowV1
  , initialized :: Bool
  , tiled :: River.RiverWindowV1EdgesFlags
  , proposal :: Proposal
  , fullscreen :: FullscreenApplied
  , settledRequest :: Maybe Size
  , actual :: Maybe Size
  , appliedRect :: Maybe Rect
  , appliedColor :: Maybe BorderColor
  , appliedBorderWidth :: Maybe Int32
  , appliedShown :: Maybe Bool
  }
  deriving (Eq, Show)

data SeatApplied = SeatApplied (Object River.RiverSeatV1) Integer (Maybe (Maybe WindowId)) Bool deriving (Eq, Show)

data ReconcileState = ReconcileState (Map WindowId Applied) (Map ObjectId SeatApplied) [Object River.RiverNodeV1] deriving (Eq, Show)
emptyReconcileState :: ReconcileState
emptyReconcileState = ReconcileState Map.empty Map.empty []

fresh :: RiverWindowState -> Applied
fresh window = Applied (riverWindowObject window) False River.RiverWindowV1EdgesFlagsNone NoProposal Windowed Nothing Nothing Nothing Nothing Nothing Nothing

windowIdOf :: RiverWindowState -> WindowId
windowIdOf = WindowId . objectIdValue . objectId . riverWindowObject

liveStates :: RiverRuntimeSnapshot -> Map WindowId Applied -> Map WindowId Applied
liveStates snapshot old = Map.fromList [(windowIdOf w, release (settle w (previous w))) | w <- riverInventoryWindows inventory]
 where
  inventory = riverRuntimeInventory snapshot
  previous w = case Map.lookup (windowIdOf w) old of
    Just applied | appliedWindow applied == riverWindowObject w -> applied
    _ -> fresh w
  settle w a = case proposal a of
    AwaitingDimensions requested revision | riverWindowDimensionsRevision w > revision -> a{proposal = NoProposal, settledRequest = Just requested, actual = observed w}
    NoProposal -> a{actual = observed w}
    _ -> a
  observed w = riverWindowDimensions w >>= \(RiverSize width height) -> either (const Nothing) Just (mkSize width height)
  release a = case fullscreen a of
    FullscreenOn o | (riverOutputObject <$> lookupRiverOutput (objectId o) inventory) /= Just o -> a{fullscreen = Released}
    _ -> a

reconcileManage :: Bool -> DesiredScene -> RiverRuntimeSnapshot -> ReconcileState -> (ReconcileState, ManageM ())
reconcileManage reclaim scene snapshot (ReconcileState old oldSeats stacking) = (ReconcileState managed seats stacking, plan)
 where
  inventory = riverRuntimeInventory snapshot
  states = liveStates snapshot old
  windows = riverInventoryWindows inventory
  desired w = Map.findWithDefault Omitted (windowIdOf w) (sceneWindows scene)
  needsProposal w a = case (desired w, proposal a) of
    (placement, NoProposal) | Just size <- proposalSize w placement -> settledRequest a /= Just size
    _ -> False
  proposed = Map.fromList [(windowIdOf w, ()) | w <- windows, needsProposal w (states Map.! windowIdOf w)]
  fullscreenGoal w = case desired w of
    Fullscreen (OutputId o) -> maybe NotFullscreen (CoverOutput . riverOutputObject) (mkObjectId o >>= (`lookupRiverOutput` inventory))
    HiddenFullscreen -> KeepFullscreen
    _ -> NotFullscreen
  transitions = Map.fromList [(windowIdOf w, fullscreenTransition (riverWindowObject w) (fullscreen (states Map.! windowIdOf w)) (fullscreenGoal w)) | w <- windows]
  update w =
    let a = states Map.! windowIdOf w
        next =
          a
            { initialized = True
            , tiled = tiledEdges (desired w) (tiled a)
            , proposal = case proposalSize w (desired w) of
                Just size | Map.member (windowIdOf w) proposed -> AwaitingDimensions size (riverWindowDimensionsRevision w)
                _ -> proposal a
            , fullscreen = fst (transitions Map.! windowIdOf w)
            }
     in case fullscreen next of
          FullscreenOn _ -> next{proposal = NoProposal, settledRequest = Nothing}
          _ -> next
  managed = Map.fromList [(windowIdOf w, update w) | w <- windows]
  eligible w = case (Map.lookup w states, Map.lookup w (sceneWindows scene)) of
    (Just a, Just placement) | placementShown placement -> appliedShown a == Just True && proposal a == NoProposal && Map.notMember w proposed
    _ -> False
  focus = sceneFocus scene >>= \w -> if eligible w then Just w else Nothing
  seatSteps = map seatStep (riverRuntimeSeatBindings snapshot)
  seats = Map.fromList [(identifier, value) | (identifier, value, _) <- seatSteps]
  seatStep seat = (identifier, SeatApplied (riverBindingsSeat seat) revision nextFocus overridden, request)
   where
    identifier = objectId (riverBindingsSeat seat)
    revision = riverSeatLayerFocusRevision seat
    previous = Map.lookup identifier oldSeats
    (lastFocus, reclaimed) = case previous of
      Just (SeatApplied handle r f override) | handle == riverBindingsSeat seat && r == revision -> (f, override)
      _ -> (Nothing, False)
    layer = riverSeatLayerFocus seat
    allowed = layer == RiverLayerFocusNone || (layer == RiverLayerFocusNonExclusive && (reclaim || reclaimed))
    send = allowed && (lastFocus /= Just focus || (reclaim && layer == RiverLayerFocusNonExclusive && not reclaimed))
    nextFocus = if send then Just focus else lastFocus
    overridden = reclaimed || (send && layer == RiverLayerFocusNonExclusive)
    request
      | not send = pure ()
      | otherwise = case focus >>= lookupWindow inventory of
          Just w -> focusWindow (riverBindingsSeat seat) (riverWindowObject w)
          Nothing -> clearFocus (riverBindingsSeat seat)
  initialize w = unless (initialized (states Map.! windowIdOf w)) $ do
    useWindowSsd (riverWindowObject w)
    setWindowCapabilities (riverWindowObject w) River.RiverWindowV1CapabilitiesFlagsFullscreen
  applyTiled w =
    let current = tiled (states Map.! windowIdOf w)
        edges = tiledEdges (desired w) current
     in when (edges /= current) (setWindowTiled (riverWindowObject w) edges)
  propose w = case proposalSize w (desired w) of
    Just size
      | Map.member (windowIdOf w) proposed ->
          proposeWindowDimensions (riverWindowObject w) (sizeWidth size) (sizeHeight size)
    _ -> pure ()
  plan = do
    mapM_ initialize windows
    mapM_ (snd . (transitions Map.!) . windowIdOf) windows
    mapM_ applyTiled windows
    mapM_ propose windows
    mapM_ setDefaultLayerShellOutput currentOutput
    mapM_ (\(_, _, request) -> request) seatSteps
  currentOutput = sceneOutput scene >>= \(OutputId o) -> mkObjectId o >>= (`lookupRiverOutput` inventory) >>= riverOutputLayerShell

reconcileRender :: SceneStyle -> DesiredScene -> RiverRuntimeSnapshot -> ReconcileState -> (ReconcileState, RenderM RenderOutcome)
reconcileRender style scene snapshot (ReconcileState old seats oldStacking) = (ReconcileState rendered seats stacking, plan)
 where
  inventory = riverRuntimeInventory snapshot
  states = liveStates snapshot old
  windows = riverInventoryWindows inventory
  layerOwns seat = case Map.lookup (objectId (riverBindingsSeat seat)) seats of
    Just (SeatApplied handle revision _ override) | handle == riverBindingsSeat seat && revision == riverSeatLayerFocusRevision seat -> riverSeatLayerFocus seat /= RiverLayerFocusNone && not override
    _ -> riverSeatLayerFocus seat /= RiverLayerFocusNone
  layerFocused = any layerOwns (riverRuntimeSeatBindings snapshot)
  color w
    | not layerFocused && sceneFocus scene == Just w = sceneFocusedColor style
    | otherwise = sceneUnfocusedColor style
  settledTargetRect w a = do
    placement <- Map.lookup (windowIdOf w) (sceneWindows scene)
    _ <- settledRequest a
    size <- actual a
    _ <- riverWindowNode w
    origin <- topLeft placement size
    either (const Nothing) Just (mkRect origin size)
  target w a = case (Map.lookup (windowIdOf w) (sceneWindows scene), fullscreen a, riverWindowNode w) of
    (Just Fullscreen{}, FullscreenOn _, Just _) -> Covering
    _ -> maybe Hidden ShownAt (settledTargetRect w a)
  targets = Map.fromList [(windowIdOf w, target w (states Map.! windowIdOf w)) | w <- windows]
  update w =
    let wid = windowIdOf w
        a = states Map.! wid
     in case targets Map.! wid of
          ShownAt rect -> a{appliedRect = Just rect, appliedShown = Just True, appliedColor = Just (color wid), appliedBorderWidth = Just borderWidth}
          t -> a{appliedRect = Nothing, appliedShown = Just (t == Covering)}
  stableOrder = sceneWindowOrder scene ++ [windowIdOf w | w <- windows, windowIdOf w `notElem` sceneWindowOrder scene]
  liveOrder = [w | w <- stableOrder, Just window <- [lookupWindow inventory w], Just _ <- [riverWindowNode window]]
  shownWindows = [w | w <- liveOrder, Just t <- [Map.lookup w targets], t /= Hidden]
  covering = [w | w <- liveOrder, Map.lookup w targets == Just Covering]
  parents = Map.fromList [(windowIdOf w, WindowId . objectIdValue <$> riverWindowParent w) | w <- windows]
  stacking =
    mapMaybe (\wid -> lookupWindow inventory wid >>= riverWindowNode) $
      stackWindows liveOrder (sceneRaiseOrder scene) covering shownWindows (sceneFocus scene) parents
  rendered = Map.fromList [(windowIdOf w, update w) | w <- windows]
  changedFocusVisibility = case sceneFocus scene of
    Nothing -> False
    Just w -> (appliedShown =<< Map.lookup w states) /= Just True && (appliedShown =<< Map.lookup w rendered) == Just True
  focusRecovery = any needsFocus (riverRuntimeSeatBindings snapshot)
  needsFocus seat = case Map.lookup (objectId (riverBindingsSeat seat)) seats of
    Just (SeatApplied handle revision previous override)
      | handle == riverBindingsSeat seat
      , revision == riverSeatLayerFocusRevision seat
      , riverSeatLayerFocus seat == RiverLayerFocusNone || override -> case sceneFocus scene of
          Just w | Just a <- Map.lookup w rendered, appliedShown a == Just True, proposal a == NoProposal -> previous /= Just (Just w)
          _ -> False
    _ -> False
  superseded =
    any
      ( \w -> case (Map.lookup (windowIdOf w) (sceneWindows scene), proposal (states Map.! windowIdOf w)) of
          (Just placement, NoProposal) | Just size <- proposalSize w placement -> settledRequest (states Map.! windowIdOf w) /= Just size
          _ -> False
      )
      windows
  borderWidth = max 0 (sceneBorderWidth style)
  position w = case targets Map.! windowIdOf w of
    ShownAt rect
      | appliedRect (states Map.! windowIdOf w) /= Just rect ->
          mapM_ (\node -> setNodePosition node (rectX rect) (rectY rect)) (riverWindowNode w)
    _ -> pure ()
  borders w = when changed $ do
    let BorderColor r g b alpha = color wid
    setWindowBorders (riverWindowObject w) (River.RiverWindowV1EdgesFlags 15) borderWidth r g b alpha
   where
    wid = windowIdOf w
    previous = states Map.! wid
    changed =
      isShownAt (targets Map.! wid)
        && (appliedColor previous /= Just (color wid) || appliedBorderWidth previous /= Just borderWidth)
  visibility w =
    unless (appliedShown (states Map.! windowIdOf w) == Just shown) $
      (if shown then showWindow else hideWindow) (riverWindowObject w)
   where
    shown = targets Map.! windowIdOf w /= Hidden
  plan = do
    mapM_ position windows
    unless (stacking == oldStacking) $
      mapM_ placeNodeTop stacking
    mapM_ borders windows
    mapM_ visibility windows
    pure (RenderOutcome (changedFocusVisibility || focusRecovery || superseded))

isShownAt :: RenderTarget -> Bool
isShownAt = \case
  ShownAt _ -> True
  _ -> False

{- | Move what river and the client believe toward the goal: enter with both
requests, follow a moved workspace with @fullscreen@ alone, and exit with
both unless river already exited when the output went away. A hidden window
keeps its state, so one that asked while hidden enters when shown.
-}
fullscreenTransition :: Object River.RiverWindowV1 -> FullscreenApplied -> FullscreenGoal -> (FullscreenApplied, ManageM ())
fullscreenTransition window applied goal = case (applied, goal) of
  (_, KeepFullscreen) -> (applied, pure ())
  (FullscreenOn current, CoverOutput o) | current == o -> (applied, pure ())
  (Windowed, CoverOutput o) -> (FullscreenOn o, fullscreenWindow window o >> informWindowFullscreen window)
  (_, CoverOutput o) -> (FullscreenOn o, fullscreenWindow window o)
  (FullscreenOn _, NotFullscreen) -> (Windowed, exitWindowFullscreen window >> informWindowNotFullscreen window)
  (Released, NotFullscreen) -> (Windowed, informWindowNotFullscreen window)
  (Windowed, NotFullscreen) -> (Windowed, pure ())

lookupWindow :: RiverInventory -> WindowId -> Maybe RiverWindowState
lookupWindow inventory (WindowId w) = mkObjectId w >>= (`lookupRiverWindow` inventory)

{- | The size to propose: a box's size, a 'Tolerate' tile's size clamped to the
window's hint, or zero by zero so the client picks its own.
-}
proposalSize :: RiverWindowState -> Placement -> Maybe Size
proposalSize window = \case
  Omitted -> Nothing
  Placed Crop r -> Just (rectSize r)
  Placed Tolerate r -> Just (clampToHint (riverWindowDimensionsHint window) (rectSize r))
  Floating r -> Just (rectSize r)
  Centered _ -> either (const Nothing) Just (mkSize 0 0)
  Fullscreen _ -> Nothing
  HiddenFullscreen -> Nothing

{- | Clamp each axis to at least the hint's minimum and at most its maximum,
where a zero bound is no preference.
-}
clampToHint :: Maybe RiverDimensionsHint -> Size -> Size
clampToHint hint size = case hint of
  Nothing -> size
  Just (RiverDimensionsHint minWidth minHeight maxWidth maxHeight) ->
    fromRight size (mkSize (axis minWidth maxWidth (sizeWidth size)) (axis minHeight maxHeight (sizeHeight size)))
 where
  axis lo hi = (if hi > 0 then min hi else id) . max lo

tiledEdges :: Placement -> River.RiverWindowV1EdgesFlags -> River.RiverWindowV1EdgesFlags
tiledEdges placement current = case placement of
  Omitted -> current
  Placed _ _ -> River.RiverWindowV1EdgesFlags 15
  Floating _ -> River.RiverWindowV1EdgesFlagsNone
  Centered _ -> River.RiverWindowV1EdgesFlagsNone
  Fullscreen _ -> current
  HiddenFullscreen -> current

topLeft :: Placement -> Size -> Maybe Point
topLeft placement size = case placement of
  Omitted -> Nothing
  Placed _ r -> Just (rectOrigin r)
  Floating r -> Just (rectOrigin r)
  Fullscreen _ -> Nothing
  HiddenFullscreen -> Nothing
  Centered r -> Just (Point (centre (rectX r) (rectWidth r) (sizeWidth size)) (centre (rectY r) (rectHeight r) (sizeHeight size)))
 where
  centre origin extent len = origin + max 0 ((extent - len) `div` 2)
