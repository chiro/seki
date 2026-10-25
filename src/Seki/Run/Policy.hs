{-# LANGUAGE LambdaCase #-}

{- | The runtime policy that owns 'CoreState' and executes action intents.

River callbacks and key-binding presses append core events to one queue, so
their arrival order is kept across kinds. The startup hook joins the queue
once per process, right after the first output's event, so it sees an
attached state and outlives later detach and reattach. A restarted process
runs it again; 'Seki.Internal.Core.Action.spawnOnce' is what keeps its
launches from repeating. Each @manage_start@ drains the
queue into the core state, giving each new window the properties the
inventory holds by then, launches spawned commands, and sends close
requests in the manage sequence. Queued events read the inventory, the
boxes the last render showed, and the op river runs on each seat when the
drain begins, so a pointer press starts from what is on screen and input
for an op river no longer runs is dropped. While an output is attached, every manage
sequence then marks the current screen's output as the layer-shell default.
The request is idempotent, and river forgets the default when that output's
layer-shell object is destroyed, so it is sent every time rather than on
change.
-}
module Seki.Run.Policy (
  PolicyConfig (..),
  PolicyError (..),
  parseResizeEdges,
  Policy,
  newPolicy,
  policyRuntimeHandlers,
  readPolicyState,
  launchDetached,
)
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Bits (complement, (.&.))
import Data.Foldable (find)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Seki.Internal.Core.Action (Action, Intent (..))
import Seki.Internal.Core.Event (CoreEvent (..), drainCore)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), Rect, mkRect, mkSize, rectX, rectY, translate)
import Seki.Internal.Core.Layout (LayoutClass)
import Seki.Internal.Core.ManageHook (DimensionsHint (..), ManageHook, WindowProps (..), noWindowProps)
import Seki.Internal.Core.PointerOp (HEdge (..), OpId, OpInput (..), OpKind (..), OpOrigin (..), OpRequest (..), ResizeEdges (..), SeatId (..), SizeBounds (..), VEdge (..), noSizeBounds)
import Seki.Internal.Core.Scene (BorderColor (..), DesiredScene (..), OutputArea (..), SceneStyle (..), planScene)
import Seki.Internal.Core.StackSet (OutputId (..), Screen (..), StackSet (..), WindowId (..), Workspace (..), integrate')
import Seki.Internal.Core.State (CoreState (..), WorkspaceTagError, Workspaces (..), newCoreState)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Inventory (RiverDimensionsHint (..), RiverInventory, RiverOutputState (..), RiverRectangle (..), RiverWindowState (..), lookupRiverOutput, lookupRiverWindow, riverInventoryOutputs, riverInventoryWindows, riverOutputRectangle)
import Seki.Run.Phase (ManageM, RenderM, closeWindow)
import Seki.Run.Pointer (RiverPointerBinding (..))
import Seki.Run.Reconcile (ReconcileState, emptyReconcileState, reconcileManage, reconcileRender, reconciledBoxes, reconciledOps)
import Seki.Run.River (RiverWindowManagerHandlers (..))
import Seki.Run.Runtime (ManageOutcome (..), PointerBindingEvent (..), RenderOutcome, RiverRuntimeHandlers (..), RiverRuntimeSnapshot (..))
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (Object, objectId)
import Seki.Wire.Types (mkObjectId, objectIdValue)
import System.Exit (ExitCode)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, getProcessExitCode, proc)

data PolicyConfig i l = PolicyConfig
  { policyLayout :: l
  , policyTags :: [i]
  , policyBindings :: [(RiverKeyBinding, Action i l ())]
  , policySceneStyle :: SceneStyle
  , policyLaunch :: String -> IO ()
  -- ^ Runs while a manage sequence is prepared, so it must not block; see 'launchDetached'.
  , policyManageHook :: ManageHook i l
  , policyStartupHook :: Action i l ()
  -- ^ Queued once per process, right after the first output arrives.
  , policyPointerBindings :: [(RiverPointerBinding, OpKind)]
  }

data PolicyError i
  = PolicyInvalidSceneStyle SceneStyle
  | PolicyTagError (WorkspaceTagError i)
  | PolicyBindingConflict RiverKeyBinding
  | PolicyInvalidModifiers RiverKeyBinding
  | PolicyPointerBindingConflict RiverPointerBinding
  | PolicyInvalidPointerModifiers RiverPointerBinding
  deriving (Eq, Show)

-- | What a queued event reads when the drain begins.
data DrainView = DrainView
  { drainInventory :: RiverInventory
  , drainShownBox :: WindowId -> Maybe OutputRect
  -- ^ The content box the last render showed, relative to the output that
  -- shows the window's workspace before the drain.
  , drainRiverOp :: SeatId -> Maybe OpId
  -- ^ The op river runs, not the op the core wants.
  }

data Policy i l = Policy
  { policyConfig :: PolicyConfig i l
  , policyState :: IORef (CoreState i l)
  , policyReconcile :: IORef ReconcileState
  , policyScene :: IORef DesiredScene
  , policyQueue :: IORef [DrainView -> CoreEvent i l]
  -- ^ Newest first.
  , policyPendingStartup :: IORef (Maybe (Action i l ()))
  -- ^ The startup hook until an output arrives.
  }

-- | Validate tags, binding identities and modifiers, border width, and premultiplied colors before connecting.
newPolicy :: (Eq i) => PolicyConfig i l -> IO (Either (PolicyError i) (Policy i l))
newPolicy config
  | not validStyle = pure (Left (PolicyInvalidSceneStyle style))
  | binding : _ <- filter invalidModifiers (map fst (policyBindings config)) = pure (Left (PolicyInvalidModifiers binding))
  | binding : _ <- filter (invalidBits . riverPointerModifiers) (map fst (policyPointerBindings config)) = pure (Left (PolicyInvalidPointerModifiers binding))
  | Just binding <- firstRepeat [] (map fst (policyPointerBindings config)) = pure (Left (PolicyPointerBindingConflict binding))
  | otherwise = case newCoreState (policyLayout config) (policyTags config) of
      Left e -> pure (Left (PolicyTagError e))
      Right st -> case firstRepeat [] (map fst (policyBindings config)) of
        Just binding -> pure (Left (PolicyBindingConflict binding))
        Nothing -> Right <$> (Policy config <$> newIORef st <*> newIORef emptyReconcileState <*> newIORef (DesiredScene Map.empty [] [] [] Nothing Nothing Nothing) <*> newIORef [] <*> newIORef (Just (policyStartupHook config)))
 where
  -- River excludes capslock and numlock bits from its modifier enum.
  invalidModifiers = invalidBits . riverKeyBindingModifiers
  invalidBits modifiers = River.riverSeatV1ModifiersFlagsValue modifiers .&. complement 0xed /= 0
  style = policySceneStyle config
  validStyle = sceneBorderWidth style >= 0 && validColor (sceneFocusedColor style) && validColor (sceneUnfocusedColor style)
  validColor (BorderColor r g b a) = all (<= a) [r, g, b]

firstRepeat :: (Eq a) => [a] -> [a] -> Maybe a
firstRepeat _ [] = Nothing
firstRepeat seen (x : xs)
  | x `elem` seen = Just x
  | otherwise = firstRepeat (x : seen) xs

policyRuntimeHandlers :: (Eq i, LayoutClass l) => Policy i l -> RiverRuntimeHandlers
policyRuntimeHandlers policy =
  RiverRuntimeHandlers
    { riverProtocolHandlers =
        RiverWindowManagerHandlers
          { handleRiverWindowManagerEvent = \_ _ -> pure ()
          , handleRiverWindowCreated = \window -> enqueueWith (WindowCreated (windowIdOf window) . windowProps window . drainInventory)
          , handleRiverWindowEvent = \window -> \case
              River.RiverWindowV1Closed -> enqueue (WindowClosed (windowIdOf window))
              River.RiverWindowV1FullscreenRequested _ -> enqueue (FullscreenRequested (windowIdOf window))
              River.RiverWindowV1ExitFullscreenRequested -> enqueue (ExitFullscreenRequested (windowIdOf window))
              River.RiverWindowV1PointerMoveRequested seat -> enqueueWith (startOp FromClient Move (SeatId (objectIdValue seat)) (windowIdOf window))
              River.RiverWindowV1PointerResizeRequested seat edges
                | Just parsed <- parseResizeEdges edges -> enqueueWith (startOp FromClient (Resize parsed) (SeatId (objectIdValue seat)) (windowIdOf window))
              _ -> pure ()
          , handleRiverOutputCreated = \output -> do
              enqueue (OutputCreated (outputIdOf output))
              atomicModifyIORef' (policyPendingStartup policy) (Nothing,) >>= mapM_ (enqueue . RunAction)
          , handleRiverOutputEvent = \output -> \case
              River.RiverOutputV1Removed -> enqueue (OutputRemoved (outputIdOf output))
              _ -> pure ()
          , handleRiverSeatCreated = \_ -> pure ()
          , handleRiverSeatEvent = \seat -> \case
              River.RiverSeatV1WindowInteraction window -> enqueue (WindowInteraction (WindowId (objectIdValue window)))
              River.RiverSeatV1OpDelta dx dy -> enqueueWith (opInput seat (OpMoved dx dy))
              River.RiverSeatV1OpRelease -> enqueueWith (opInput seat OpReleased)
              River.RiverSeatV1Removed -> enqueue (SeatRemoved (seatIdOf seat))
              _ -> pure ()
          }
    , riverKeyBindings = map fst bindings
    , handleRiverKeyBindingEvent = \definition _ event ->
        case (event, lookup definition bindings) of
          (Xkb.RiverXkbBindingV1Pressed, Just action) -> enqueue (RunAction action)
          _ -> pure ()
    , riverPointerBindings = map fst pointerBindings
    , handleRiverPointerBindingEvent = \definition seat event ->
        case (event, lookup definition pointerBindings) of
          (PointerPressed (Just window), Just kind) -> enqueueWith (startOp FromBinding kind (seatIdOf seat) (windowIdOf window))
          (PointerReleased, Just _) -> enqueueWith (opInput seat OpReleased)
          _ -> pure ()
    , riverSequenceRecorder = Nothing
    , prepareRiverManagePlan = prepareManagePlan policy
    , prepareRiverRenderPlan = prepareRenderPlan policy
    }
 where
  bindings = policyBindings (policyConfig policy)
  pointerBindings = policyPointerBindings (policyConfig policy)
  startOp origin kind seat window view =
    StartOp (OpRequest seat window kind origin (drainShownBox view window) (sizeBounds window (drainInventory view)))
  opInput seat input view = OpInput (seatIdOf seat) (drainRiverOp view (seatIdOf seat)) input
  enqueue = enqueueWith . const
  enqueueWith event = atomicModifyIORef' (policyQueue policy) (\queue -> (event : queue, ()))

prepareManagePlan :: (Eq i, LayoutClass l) => Policy i l -> RiverRuntimeSnapshot -> IO (ManageM ManageOutcome)
prepareManagePlan policy snapshot = do
  before <- readIORef (policyState policy)
  previous <- readIORef (policyReconcile policy)
  let view = DrainView inventory (shownBox before previous) (`Map.lookup` reconciledOps previous)
  events <- map ($ view) . reverse <$> atomicModifyIORef' (policyQueue policy) ([],)
  let (st, intents) = drainCore (policyManageHook (policyConfig policy)) events before
  writeIORef (policyState policy) st
  let effects = interpretIntents intents
  mapM_ (policyLaunch (policyConfig policy)) (planLaunch effects)
  let scene =
        planScene
          (policySceneStyle (policyConfig policy))
          st
          (Map.fromList [(outputIdOf (riverOutputObject o), OutputArea bounds area) | o <- riverInventoryOutputs inventory, Just bounds <- [outputRect o], Just area <- [riverOutputUsableArea o]])
          (Map.fromList [(windowIdOf (riverWindowObject w), WindowId . objectIdValue <$> riverWindowParent w) | w <- riverInventoryWindows inventory])
      reclaim = planReclaim effects || any isInteraction events
  writeIORef (policyScene policy) scene
  let (next, plan) = reconcileManage reclaim scene snapshot previous
  writeIORef (policyReconcile policy) next
  pure $ do
    mapM_ closeWindow (mapMaybe liveWindow (planClose effects))
    plan
    pure KeepManaging
 where
  inventory = riverRuntimeInventory snapshot
  liveWindow (WindowId w) =
    riverWindowObject <$> (mkObjectId w >>= (`lookupRiverWindow` inventory))
  isInteraction WindowInteraction{} = True
  isInteraction (StartOp request) = reqOrigin request == FromBinding
  isInteraction _ = False
  shownBox st reconciled w = do
    rect <- Map.lookup w (reconciledBoxes reconciled)
    OutputId o <- workspaceOutput st w
    origin <- mkObjectId o >>= (`lookupRiverOutput` inventory) >>= outputRect
    either (const Nothing) (Just . OutputRect) (translate (negate (rectX origin)) (negate (rectY origin)) rect)

-- | Every intent's effect on a manage sequence, in order.
data IntentPlan = IntentPlan
  { planLaunch :: [String]
  , planClose :: [WindowId]
  , planReclaim :: Bool
  }

-- | A case over every 'Intent', so a new one cannot be silently ignored.
interpretIntents :: [Intent] -> IntentPlan
interpretIntents = foldr add (IntentPlan [] [] False)
 where
  add intent plan = case intent of
    Spawn cmd -> plan{planLaunch = cmd : planLaunch plan}
    CloseWindow w -> plan{planClose = w : planClose plan}
    ReclaimFocus -> plan{planReclaim = True}
    Restart -> plan

-- | The output that shows the window's workspace.
workspaceOutput :: CoreState i l -> WindowId -> Maybe OutputId
workspaceOutput st w = case coreWorkspaces st of
  AttachedState ss _ -> output <$> find ((w `elem`) . integrate' . stack . workspace) (current ss : visible ss)
  HeadlessState _ -> Nothing

-- | The window's dimensions hint now, which can differ from the one it was created with.
sizeBounds :: WindowId -> RiverInventory -> SizeBounds
sizeBounds (WindowId w) inventory = case mkObjectId w >>= (`lookupRiverWindow` inventory) >>= riverWindowDimensionsHint of
  Just (RiverDimensionsHint minW minH maxW maxH) -> SizeBounds minW minH maxW maxH
  Nothing -> noSizeBounds

{- | River's resize edges, or 'Nothing' for none, unknown bits, or both
opposite edges, which river never sends.
-}
parseResizeEdges :: River.RiverWindowV1EdgesFlags -> Maybe ResizeEdges
parseResizeEdges (River.RiverWindowV1EdgesFlags bits)
  | bits .&. complement 15 /= 0 = Nothing
  | otherwise = case (side 4 8 LeftEdge RightEdge, side 1 2 TopEdge BottomEdge) of
      (Right (Just h), Right (Just v)) -> Just (Corner h v)
      (Right (Just h), Right Nothing) -> Just (Horizontal h)
      (Right Nothing, Right (Just v)) -> Just (Vertical v)
      _ -> Nothing
 where
  side :: Word32 -> Word32 -> e -> e -> Either () (Maybe e)
  side near far nearEdge farEdge = case (bits .&. near /= 0, bits .&. far /= 0) of
    (True, True) -> Left ()
    (True, False) -> Right (Just nearEdge)
    (False, True) -> Right (Just farEdge)
    (False, False) -> Right Nothing

prepareRenderPlan :: Policy i l -> RiverRuntimeSnapshot -> IO (RenderM RenderOutcome)
prepareRenderPlan policy snapshot = do
  scene <- readIORef (policyScene policy)
  previous <- readIORef (policyReconcile policy)
  let (next, plan) = reconcileRender (policySceneStyle (policyConfig policy)) scene snapshot previous
  writeIORef (policyReconcile policy) next
  pure plan

readPolicyState :: Policy i l -> IO (CoreState i l)
readPolicyState = readIORef . policyState

{- | Run a command through @/bin/sh -c@ without waiting for it. A forked thread
reaps the shell when it exits and reports a launch failure on stderr.
-}
launchDetached :: String -> IO ()
launchDetached cmd = void . forkIO $ do
  started <- try (createProcess (proc "/bin/sh" ["-c", cmd]))
  case started of
    Left e -> hPutStrLn stderr ("seki: launch failed: " <> show (e :: IOException))
    Right (_, _, _, handle) -> reap (getProcessExitCode handle) 1000
 where
  -- waitForProcess would block every Haskell thread under the non-threaded
  -- RTS, so poll instead, backing off to once a second.
  reap :: IO (Maybe ExitCode) -> Int -> IO ()
  reap check delay =
    check >>= \case
      Just _ -> pure ()
      Nothing -> threadDelay delay >> reap check (min 1000000 (delay * 2))

-- | A window closed before the drain has left the inventory and reports nothing.
windowProps :: Object River.RiverWindowV1 -> RiverInventory -> WindowProps
windowProps window inventory = maybe noWindowProps props (lookupRiverWindow (objectId window) inventory)
 where
  props w =
    WindowProps
      { propAppId = riverWindowAppId w
      , propTitle = riverWindowTitle w
      , propParent = WindowId . objectIdValue <$> riverWindowParent w
      , propDimensionsHint = hint <$> riverWindowDimensionsHint w
      }
  hint (RiverDimensionsHint minW minH maxW maxH) = DimensionsHint minW minH maxW maxH

seatIdOf :: Object River.RiverSeatV1 -> SeatId
seatIdOf = SeatId . rawId

windowIdOf :: Object River.RiverWindowV1 -> WindowId
windowIdOf = WindowId . rawId

outputIdOf :: Object River.RiverOutputV1 -> OutputId
outputIdOf = OutputId . rawId

rawId :: Object interface -> Word32
rawId = objectIdValue . objectId

outputRect :: RiverOutputState -> Maybe Rect
outputRect output = do
  RiverRectangle x y width height <- riverOutputRectangle output
  either (const Nothing) Just (mkSize width height >>= mkRect (Point x y))
