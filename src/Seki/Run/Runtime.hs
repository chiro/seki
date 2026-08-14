{-# LANGUAGE LambdaCase #-}

-- | Owned river runtime startup and continuous Wayland event dispatch.
module Seki.Run.Runtime (
  RiverSession,
  riverWaylandSession,
  riverWindowManager,
  readRiverInventory,
  RiverRuntimeHandlers (..),
  RiverRuntimeError (..),
  openRiverSession,
  openRiverSessionAt,
  runRiverSession,
  closeRiverSession,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (finally, mask, onException)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory (
  RiverInventory,
  RiverInventoryError,
  RiverWindowState (..),
  addRiverOutput,
  addRiverWindow,
  applyRiverOutputEvent,
  applyRiverWindowEvent,
  attachRiverWindowNode,
  emptyRiverInventory,
  lookupRiverWindow,
 )
import Seki.Run.Phase (ManageM, RenderM, runManageM, runRenderM)
import Seki.Run.River (
  RiverBootstrapError,
  RiverWindowManagerHandlers (..),
  bindRiverWindowManager,
 )
import Seki.Run.Sequence (
  RiverSequenceCompletion (..),
  RiverSequenceError,
  RiverSequenceState,
  completeRiverSequence,
  initialRiverSequenceState,
  startManageSequence,
  startRenderSequence,
 )
import Seki.Run.Startup (
  WaylandSession,
  WaylandStartupError,
  closeWaylandSession,
  openWaylandSession,
  openWaylandSessionAt,
  readWaylandGlobalCatalog,
  waylandConnection,
  waylandRegistry,
 )
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  DispatchResult (..),
  dispatchNextEvent,
  submitInheritedObjectRequest,
  submitRequest,
 )
import Seki.Wire.Connection.ObjectTable (Object, ObjectLimit, objectId)
import Seki.Wire.Connection.Registry (RegistryError)
import Seki.Wire.Transport (ConnectionTarget, OwnedFd)

-- | A synchronized Wayland session with the river manager bound and routed.
data RiverSession = RiverSession
  { riverWaylandSession :: WaylandSession
  , riverWindowManager :: Object River.RiverWindowManagerV1
  , riverEventLoopClaim :: MVar Bool
  , riverSequenceStatus :: IORef (Either RiverRuntimeError RiverSequenceState)
  , riverInventoryState :: IORef RiverInventory
  }

{- | Low-level river event handlers plus phase-safe plans prepared at each
sequence boundary.

Plan preparation is a pure function of the runtime-owned inventory, and the
returned plans cannot lift arbitrary IO.
-}
data RiverRuntimeHandlers = RiverRuntimeHandlers
  { riverProtocolHandlers :: RiverWindowManagerHandlers
  , prepareRiverManagePlan :: RiverInventory -> ManageM ()
  , prepareRiverRenderPlan :: RiverInventory -> RenderM ()
  }

data RiverRuntimeError
  = RiverRuntimeStartupError WaylandStartupError
  | RiverRuntimeBootstrapError RiverBootstrapError
  | RiverRuntimeConnectionError ConnectionError
  | RiverRuntimeRegistryError RegistryError
  | RiverRuntimeInventoryError RiverInventoryError
  | RiverRuntimeSequenceError RiverSequenceError
  | RiverRuntimeEventLoopAlreadyOwned
  deriving (Eq, Show)

-- | Discover Wayland, synchronize globals, and bind the river manager.
openRiverSession ::
  RiverRuntimeHandlers ->
  IO (Either RiverRuntimeError RiverSession)
openRiverSession = openRiverSessionWith openWaylandSession

-- | Open an explicit Wayland target and bind its river manager.
openRiverSessionAt ::
  ConnectionTarget ->
  ObjectLimit ->
  RiverRuntimeHandlers ->
  IO (Either RiverRuntimeError RiverSession)
openRiverSessionAt target limit =
  openRiverSessionWith (openWaylandSessionAt target limit)

openRiverSessionWith ::
  IO (Either WaylandStartupError WaylandSession) ->
  RiverRuntimeHandlers ->
  IO (Either RiverRuntimeError RiverSession)
openRiverSessionWith openWayland runtimeHandlers =
  mask $ \restore -> do
    restore openWayland >>= \case
      Left errorValue -> pure (Left (RiverRuntimeStartupError errorValue))
      Right wayland -> do
        sequenceStatus <- newIORef (Right initialRiverSequenceState)
        inventoryState <- newIORef emptyRiverInventory
        let checkedHandlers = sequenceCheckedHandlers wayland sequenceStatus inventoryState runtimeHandlers
        result <- restore (bindManager wayland checkedHandlers) `onException` closeWaylandSession wayland
        case result of
          Left errorValue -> closeWaylandSession wayland >> pure (Left errorValue)
          Right manager -> do
            eventLoopClaim <- newMVar False
            pure (Right (RiverSession wayland manager eventLoopClaim sequenceStatus inventoryState))

bindManager ::
  WaylandSession ->
  RiverWindowManagerHandlers ->
  IO (Either RiverRuntimeError (Object River.RiverWindowManagerV1))
bindManager wayland handlers =
  readWaylandGlobalCatalog wayland >>= \case
    Left errorValue -> pure (Left (RiverRuntimeRegistryError errorValue))
    Right catalog ->
      either (Left . RiverRuntimeBootstrapError) Right
        <$> bindRiverWindowManager
          (waylandConnection wayland)
          (waylandRegistry wayland)
          catalog
          handlers

{- | Own the event loop until clean EOF or failure, then close the session.

Handler exceptions are rethrown after the underlying connection has been
closed by the connection and runtime layers.
-}
runRiverSession :: RiverSession -> IO (Either RiverRuntimeError ())
runRiverSession session = do
  claimed <-
    modifyMVar (riverEventLoopClaim session) $ \alreadyClaimed ->
      pure (True, not alreadyClaimed)
  if claimed
    then eventLoop `finally` closeRiverSession session
    else pure (Left RiverRuntimeEventLoopAlreadyOwned)
 where
  wayland = riverWaylandSession session
  eventLoop =
    dispatchNextEvent (waylandConnection wayland) >>= \case
      Left errorValue -> pure (Left (RiverRuntimeConnectionError errorValue))
      Right DispatchFinished -> pure (Right ())
      Right (EventDispatched _) ->
        readIORef (riverSequenceStatus session) >>= \case
          Left errorValue -> pure (Left errorValue)
          Right _ ->
            readWaylandGlobalCatalog wayland >>= \case
              Left errorValue -> pure (Left (RiverRuntimeRegistryError errorValue))
              Right _ -> eventLoop

-- | Close a runtime without running it. Repeated calls are safe.
closeRiverSession :: RiverSession -> IO ()
closeRiverSession = closeWaylandSession . riverWaylandSession

-- | Read the latest window/output snapshot accumulated by event dispatch.
readRiverInventory :: RiverSession -> IO RiverInventory
readRiverInventory = readIORef . riverInventoryState

sequenceCheckedHandlers ::
  WaylandSession ->
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  IORef RiverInventory ->
  RiverRuntimeHandlers ->
  RiverWindowManagerHandlers
sequenceCheckedHandlers wayland sequenceStatus inventoryState runtimeHandlers =
  handlers
    { handleRiverWindowManagerEvent = handleManagerEvent
    , handleRiverWindowCreated = handleWindowCreated
    , handleRiverWindowEvent = handleWindowEvent
    , handleRiverOutputCreated = handleOutputCreated
    , handleRiverOutputEvent = handleOutputEvent
    }
 where
  handleManagerEvent manager event = case event of
    River.RiverWindowManagerV1ManageStart ->
      runSequence
        (waylandConnection wayland)
        sequenceStatus
        handlers
        manager
        event
        startManageSequence
        (readIORef inventoryState >>= runManageM connection . prepareRiverManagePlan runtimeHandlers)
    River.RiverWindowManagerV1RenderStart ->
      runSequence
        (waylandConnection wayland)
        sequenceStatus
        handlers
        manager
        event
        startRenderSequence
        (readIORef inventoryState >>= runRenderM connection . prepareRiverRenderPlan runtimeHandlers)
    _ -> handleRiverWindowManagerEvent handlers manager event
  handleWindowCreated window = do
    added <- updateInventory sequenceStatus inventoryState (addRiverWindow window)
    when added $
      submitInheritedObjectRequest
        connection
        River.riverWindowV1Interface
        window
        River.riverNodeV1Interface
        (River.encodeRiverWindowV1GetNode . objectId)
        >>= \case
          Left errorValue ->
            recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
          Right node -> do
            attached <-
              updateInventory
                sequenceStatus
                inventoryState
                (attachRiverWindowNode window node)
            when attached (handleRiverWindowCreated handlers window)
  handleWindowEvent window event = do
    previous <- lookupRiverWindow (objectId window) <$> readIORef inventoryState
    updated <-
      updateInventory
        sequenceStatus
        inventoryState
        (applyRiverWindowEvent window event)
    when updated $ do
      handleRiverWindowEvent handlers window event
      case event of
        River.RiverWindowV1Closed ->
          case previous of
            Nothing -> pure ()
            Just state -> destroyWindow state
        _ -> pure ()
  handleOutputCreated output = do
    added <- updateInventory sequenceStatus inventoryState (addRiverOutput output)
    when added (handleRiverOutputCreated handlers output)
  handleOutputEvent output event = do
    updated <-
      updateInventory
        sequenceStatus
        inventoryState
        (applyRiverOutputEvent output event)
    when updated $ do
      handleRiverOutputEvent handlers output event
      case event of
        River.RiverOutputV1Removed ->
          submitRequest
            connection
            River.riverOutputV1Interface
            output
            River.encodeRiverOutputV1Destroy
            >>= recordConnectionResult
        _ -> pure ()
  destroyWindow state =
    destroyNode >>= \case
      False -> pure ()
      True ->
        submitRequest
          connection
          River.riverWindowV1Interface
          (riverWindowObject state)
          River.encodeRiverWindowV1Destroy
          >>= recordConnectionResult
   where
    destroyNode = case riverWindowNode state of
      Nothing -> pure True
      Just node ->
        submitRequest
          connection
          River.riverNodeV1Interface
          node
          River.encodeRiverNodeV1Destroy
          >>= \case
            Left errorValue -> do
              recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
              pure False
            Right () -> pure True
  recordConnectionResult = \case
    Left errorValue ->
      recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
    Right () -> pure ()
  handlers = riverProtocolHandlers runtimeHandlers
  connection = waylandConnection wayland

updateInventory ::
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  IORef RiverInventory ->
  (RiverInventory -> Either RiverInventoryError RiverInventory) ->
  IO Bool
updateInventory runtimeStatus inventoryState transition = do
  result <-
    atomicModifyIORef' inventoryState $ \inventory -> case transition inventory of
      Left errorValue -> (inventory, Left errorValue)
      Right next -> (next, Right ())
  case result of
    Left errorValue -> do
      recordRuntimeError runtimeStatus (RiverRuntimeInventoryError errorValue)
      pure False
    Right () -> pure True

recordRuntimeError ::
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  RiverRuntimeError ->
  IO ()
recordRuntimeError runtimeStatus newError =
  atomicModifyIORef' runtimeStatus $ \current ->
    case current of
      Left _ -> (current, ())
      Right _ -> (Left newError, ())

runSequence ::
  Connection ->
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  RiverWindowManagerHandlers ->
  Object River.RiverWindowManagerV1 ->
  River.RiverWindowManagerV1Event OwnedFd ->
  (RiverSequenceState -> Either RiverSequenceError RiverSequenceState) ->
  IO (Either ConnectionError ()) ->
  IO ()
runSequence connection sequenceStatus handlers manager event startSequence runPlan = do
  started <-
    atomicModifyIORef' sequenceStatus $ \current -> case current of
      Left errorValue -> (current, Left errorValue)
      Right state -> case startSequence state of
        Left errorValue ->
          let runtimeError = RiverRuntimeSequenceError errorValue
           in (Left runtimeError, Left runtimeError)
        Right active -> (Right active, Right active)
  case started of
    Left _ -> pure ()
    Right active -> do
      handleRiverWindowManagerEvent handlers manager event
      runPlan >>= \case
        Left errorValue ->
          writeIORef sequenceStatus (Left (RiverRuntimeConnectionError errorValue))
        Right () ->
          case completeRiverSequence active of
            Left errorValue ->
              writeIORef sequenceStatus (Left (RiverRuntimeSequenceError errorValue))
            Right (completion, next) ->
              submitSequenceCompletion connection manager completion >>= \case
                Left errorValue ->
                  writeIORef sequenceStatus (Left (RiverRuntimeConnectionError errorValue))
                Right () -> writeIORef sequenceStatus (Right next)

submitSequenceCompletion ::
  Connection ->
  Object River.RiverWindowManagerV1 ->
  RiverSequenceCompletion ->
  IO (Either ConnectionError ())
submitSequenceCompletion connection manager completion =
  submitRequest
    connection
    River.riverWindowManagerV1Interface
    manager
    ( case completion of
        SendManageFinish -> River.encodeRiverWindowManagerV1ManageFinish
        SendRenderFinish -> River.encodeRiverWindowManagerV1RenderFinish
    )
