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
requests in the manage sequence. While an output is attached, every manage
sequence then marks the current screen's output as the layer-shell default.
The request is idempotent, and river forgets the default when that output's
layer-shell object is destroyed, so it is sent every time rather than on
change.
-}
module Seki.Run.Policy (
  PolicyConfig (..),
  PolicyError (..),
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
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Seki.Internal.Core.Action (Action, Intent (..))
import Seki.Internal.Core.Event (CoreEvent (..), drainCore)
import Seki.Internal.Core.Geometry (Point (..), Rect, mkRect, mkSize)
import Seki.Internal.Core.Layout (LayoutClass)
import Seki.Internal.Core.ManageHook (DimensionsHint (..), ManageHook, WindowProps (..), noWindowProps)
import Seki.Internal.Core.Scene (BorderColor (..), DesiredScene (..), OutputArea (..), SceneStyle (..), planScene)
import Seki.Internal.Core.StackSet (OutputId (..), WindowId (..))
import Seki.Internal.Core.State (CoreState (..), WorkspaceTagError, newCoreState)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Inventory (RiverDimensionsHint (..), RiverInventory, RiverOutputState (..), RiverRectangle (..), RiverWindowState (..), lookupRiverWindow, riverInventoryOutputs, riverInventoryWindows, riverOutputRectangle)
import Seki.Run.Phase (ManageM, RenderM, closeWindow)
import Seki.Run.Reconcile (ReconcileState, emptyReconcileState, reconcileManage, reconcileRender)
import Seki.Run.River (RiverWindowManagerHandlers (..))
import Seki.Run.Runtime (RenderOutcome, RiverRuntimeHandlers (..), RiverRuntimeSnapshot (..))
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
  }

data PolicyError i
  = PolicyInvalidSceneStyle SceneStyle
  | PolicyTagError (WorkspaceTagError i)
  | PolicyBindingConflict RiverKeyBinding
  | PolicyInvalidModifiers RiverKeyBinding
  deriving (Eq, Show)

data Policy i l = Policy
  { policyConfig :: PolicyConfig i l
  , policyState :: IORef (CoreState i l)
  , policyReconcile :: IORef ReconcileState
  , policyScene :: IORef DesiredScene
  , policyQueue :: IORef [RiverInventory -> CoreEvent i l]
  -- ^ Newest first.
  , policyPendingStartup :: IORef (Maybe (Action i l ()))
  -- ^ The startup hook until an output arrives.
  }

-- | Validate tags, binding identities and modifiers, border width, and premultiplied colors before connecting.
newPolicy :: (Eq i) => PolicyConfig i l -> IO (Either (PolicyError i) (Policy i l))
newPolicy config
  | not validStyle = pure (Left (PolicyInvalidSceneStyle style))
  | binding : _ <- filter invalidModifiers (map fst (policyBindings config)) = pure (Left (PolicyInvalidModifiers binding))
  | otherwise = case newCoreState (policyLayout config) (policyTags config) of
      Left e -> pure (Left (PolicyTagError e))
      Right st -> case firstRepeat [] (map fst (policyBindings config)) of
        Just binding -> pure (Left (PolicyBindingConflict binding))
        Nothing -> Right <$> (Policy config <$> newIORef st <*> newIORef emptyReconcileState <*> newIORef (DesiredScene Map.empty [] [] [] Nothing Nothing) <*> newIORef [] <*> newIORef (Just (policyStartupHook config)))
 where
  -- River excludes capslock and numlock bits from its modifier enum.
  invalidModifiers binding = River.riverSeatV1ModifiersFlagsValue (riverKeyBindingModifiers binding) .&. complement 0xed /= 0
  style = policySceneStyle config
  validStyle = sceneBorderWidth style >= 0 && validColor (sceneFocusedColor style) && validColor (sceneUnfocusedColor style)
  validColor (BorderColor r g b a) = all (<= a) [r, g, b]
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
          , handleRiverWindowCreated = \window -> enqueueWith (WindowCreated (windowIdOf window) . windowProps window)
          , handleRiverWindowEvent = \window -> \case
              River.RiverWindowV1Closed -> enqueue (WindowClosed (windowIdOf window))
              River.RiverWindowV1FullscreenRequested _ -> enqueue (FullscreenRequested (windowIdOf window))
              River.RiverWindowV1ExitFullscreenRequested -> enqueue (ExitFullscreenRequested (windowIdOf window))
              _ -> pure ()
          , handleRiverOutputCreated = \output -> do
              enqueue (OutputCreated (outputIdOf output))
              atomicModifyIORef' (policyPendingStartup policy) (Nothing,) >>= mapM_ (enqueue . RunAction)
          , handleRiverOutputEvent = \output -> \case
              River.RiverOutputV1Removed -> enqueue (OutputRemoved (outputIdOf output))
              _ -> pure ()
          , handleRiverSeatCreated = \_ -> pure ()
          , handleRiverSeatEvent = \_ -> \case
              River.RiverSeatV1WindowInteraction window -> enqueue (WindowInteraction (WindowId (objectIdValue window)))
              _ -> pure ()
          }
    , riverKeyBindings = map fst bindings
    , handleRiverKeyBindingEvent = \definition _ event ->
        case (event, lookup definition bindings) of
          (Xkb.RiverXkbBindingV1Pressed, Just action) -> enqueue (RunAction action)
          _ -> pure ()
    , riverSequenceRecorder = Nothing
    , prepareRiverManagePlan = prepareManagePlan policy
    , prepareRiverRenderPlan = prepareRenderPlan policy
    }
 where
  bindings = policyBindings (policyConfig policy)
  enqueue = enqueueWith . const
  enqueueWith event = atomicModifyIORef' (policyQueue policy) (\queue -> (event : queue, ()))

prepareManagePlan :: (Eq i, LayoutClass l) => Policy i l -> RiverRuntimeSnapshot -> IO (ManageM ())
prepareManagePlan policy snapshot = do
  events <- map ($ inventory) . reverse <$> atomicModifyIORef' (policyQueue policy) ([],)
  (st, intents) <- drainCore (policyManageHook (policyConfig policy)) events <$> readIORef (policyState policy)
  writeIORef (policyState policy) st
  mapM_ (policyLaunch (policyConfig policy)) [cmd | Spawn cmd <- intents]
  let scene =
        planScene
          (policySceneStyle (policyConfig policy))
          st
          (Map.fromList [(outputIdOf (riverOutputObject o), OutputArea bounds area) | o <- riverInventoryOutputs inventory, Just bounds <- [outputRect o], Just area <- [riverOutputUsableArea o]])
          (Map.fromList [(windowIdOf (riverWindowObject w), WindowId . objectIdValue <$> riverWindowParent w) | w <- riverInventoryWindows inventory])
      reclaim = ReclaimFocus `elem` intents || any isInteraction events
  writeIORef (policyScene policy) scene
  previous <- readIORef (policyReconcile policy)
  let (next, plan) = reconcileManage reclaim scene snapshot previous
  writeIORef (policyReconcile policy) next
  pure $ do
    mapM_ closeWindow (mapMaybe liveWindow [w | CloseWindow w <- intents])
    plan
 where
  inventory = riverRuntimeInventory snapshot
  liveWindow (WindowId w) =
    riverWindowObject <$> (mkObjectId w >>= (`lookupRiverWindow` inventory))
  isInteraction WindowInteraction{} = True
  isInteraction _ = False

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
