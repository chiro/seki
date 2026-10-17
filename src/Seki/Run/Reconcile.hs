{- | Reconcile pure selections against river's dimensions reports.

Every proposal records the inventory revision at submission. A newer report
settles it even when the accepted size differs or repeats the previous size.
River supplies no proposal correlation identifier.
-}
module Seki.Run.Reconcile (
  ReconcileState,
  emptyReconcileState,
  reconcileManage,
  reconcileRender,
) where

import Control.Monad (unless, when)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Seki.Internal.Core.Geometry (Rect, Size, mkRect, mkSize, rectHeight, rectOrigin, rectSize, rectWidth, rectX, rectY)
import Seki.Internal.Core.Scene (BorderColor (..), DesiredScene (..), Placement (..), SceneStyle (..), stackWindows)
import Seki.Internal.Core.StackSet (OutputId (..), WindowId (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory (RiverInventory, RiverOutputState (..), RiverSize (..), RiverWindowState (..), lookupRiverOutput, lookupRiverWindow, riverInventoryWindows)
import Seki.Run.Phase (ManageM, RenderM, clearFocus, focusWindow, hideWindow, placeNodeTop, proposeWindowDimensions, setDefaultLayerShellOutput, setNodePosition, setWindowBorders, setWindowCapabilities, setWindowTiled, showWindow, useWindowSsd)
import Seki.Run.Runtime (RenderOutcome (..), RiverLayerFocus (..), RiverRuntimeSnapshot (..), RiverSeatBindings (..))
import Seki.Wire.Connection.ObjectTable (Object, objectId)
import Seki.Wire.Types (ObjectId, mkObjectId, objectIdValue)

data Proposal = NoProposal | AwaitingDimensions Size Integer deriving (Eq, Show)

data Applied = Applied
  { appliedWindow :: Object River.RiverWindowV1
  , initialized :: Bool
  , tiled :: Bool
  , proposal :: Proposal
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
fresh window = Applied (riverWindowObject window) False False NoProposal Nothing Nothing Nothing Nothing Nothing Nothing

windowIdOf :: RiverWindowState -> WindowId
windowIdOf = WindowId . objectIdValue . objectId . riverWindowObject

liveStates :: RiverRuntimeSnapshot -> Map WindowId Applied -> Map WindowId Applied
liveStates snapshot old = Map.fromList [(windowIdOf w, settle w (previous w)) | w <- riverInventoryWindows (riverRuntimeInventory snapshot)]
 where
  previous w = case Map.lookup (windowIdOf w) old of
    Just applied | appliedWindow applied == riverWindowObject w -> applied
    _ -> fresh w
  settle w a = case proposal a of
    AwaitingDimensions requested revision | riverWindowDimensionsRevision w > revision -> a{proposal = NoProposal, settledRequest = Just requested, actual = observed w}
    NoProposal -> a{actual = observed w}
    _ -> a
  observed w = riverWindowDimensions w >>= \(RiverSize width height) -> either (const Nothing) Just (mkSize width height)

reconcileManage :: Bool -> DesiredScene -> RiverRuntimeSnapshot -> ReconcileState -> (ReconcileState, ManageM ())
reconcileManage reclaim scene snapshot (ReconcileState old oldSeats stacking) = (ReconcileState managed seats stacking, plan)
 where
  inventory = riverRuntimeInventory snapshot
  states = liveStates snapshot old
  windows = riverInventoryWindows inventory
  desired w = Map.findWithDefault Omitted (windowIdOf w) (sceneWindows scene)
  needsProposal w a = case (desired w, proposal a) of
    (Placed rect, NoProposal) -> settledRequest a /= Just (rectSize rect)
    _ -> False
  proposed = Map.fromList [(windowIdOf w, ()) | w <- windows, needsProposal w (states Map.! windowIdOf w)]
  update w =
    let a = states Map.! windowIdOf w
     in a
          { initialized = True
          , tiled = tiled a || isPlaced (desired w)
          , proposal = case desired w of
              Placed rect | Map.member (windowIdOf w) proposed -> AwaitingDimensions (rectSize rect) (riverWindowDimensionsRevision w)
              _ -> proposal a
          }
  managed = Map.fromList [(windowIdOf w, update w) | w <- windows]
  eligible w = case (Map.lookup w states, Map.lookup w (sceneWindows scene)) of
    (Just a, Just Placed{}) -> appliedShown a == Just True && proposal a == NoProposal && Map.notMember w proposed
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
    setWindowCapabilities (riverWindowObject w) (River.RiverWindowV1CapabilitiesFlags 0)
  applyTiled w =
    when (isPlaced (desired w) && not (tiled (states Map.! windowIdOf w))) $
      setWindowTiled (riverWindowObject w) (River.RiverWindowV1EdgesFlags 15)
  propose w = case desired w of
    Placed rect
      | Map.member (windowIdOf w) proposed ->
          proposeWindowDimensions (riverWindowObject w) (rectWidth rect) (rectHeight rect)
    _ -> pure ()
  plan = do
    mapM_ initialize windows
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
    Placed r <- Map.lookup (windowIdOf w) (sceneWindows scene)
    _ <- settledRequest a
    size <- actual a
    _ <- riverWindowNode w
    either (const Nothing) Just (mkRect (rectOrigin r) size)
  targets = Map.fromList [(windowIdOf w, settledTargetRect w (states Map.! windowIdOf w)) | w <- windows]
  update w =
    let wid = windowIdOf w
        a = states Map.! wid
        rect = targets Map.! wid
     in a
          { appliedRect = rect
          , appliedShown = Just (maybe False (const True) rect)
          , appliedColor = if rect == Nothing then appliedColor a else Just (color wid)
          , appliedBorderWidth = if rect == Nothing then appliedBorderWidth a else Just borderWidth
          }
  stableOrder = sceneWindowOrder scene ++ [windowIdOf w | w <- windows, windowIdOf w `notElem` sceneWindowOrder scene]
  liveOrder = [w | w <- stableOrder, Just window <- [lookupWindow inventory w], Just _ <- [riverWindowNode window]]
  shownWindows = [w | w <- liveOrder, Just (Just _) <- [Map.lookup w targets]]
  parents = Map.fromList [(windowIdOf w, WindowId . objectIdValue <$> riverWindowParent w) | w <- windows]
  stacking =
    mapMaybe (\wid -> lookupWindow inventory wid >>= riverWindowNode) $
      stackWindows liveOrder shownWindows (sceneFocus scene) parents
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
          (Just (Placed rect), NoProposal) -> settledRequest (states Map.! windowIdOf w) /= Just (rectSize rect)
          _ -> False
      )
      windows
  borderWidth = max 0 (sceneBorderWidth style)
  position w = case targets Map.! windowIdOf w of
    Just rect
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
      targets Map.! wid /= Nothing
        && (appliedColor previous /= Just (color wid) || appliedBorderWidth previous /= Just borderWidth)
  visibility w =
    unless (appliedShown (states Map.! windowIdOf w) == Just shown) $
      (if shown then showWindow else hideWindow) (riverWindowObject w)
   where
    shown = targets Map.! windowIdOf w /= Nothing
  plan = do
    mapM_ position windows
    unless (stacking == oldStacking) $
      mapM_ placeNodeTop stacking
    mapM_ borders windows
    mapM_ visibility windows
    pure (RenderOutcome (changedFocusVisibility || focusRecovery || superseded))

lookupWindow :: RiverInventory -> WindowId -> Maybe RiverWindowState
lookupWindow inventory (WindowId w) = mkObjectId w >>= (`lookupRiverWindow` inventory)

isPlaced :: Placement -> Bool
isPlaced Placed{} = True
isPlaced Omitted = False
