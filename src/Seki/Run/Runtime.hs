{-# LANGUAGE LambdaCase #-}

-- | Owned river runtime startup and continuous Wayland event dispatch.
module Seki.Run.Runtime (
  RiverSession,
  riverWaylandSession,
  riverWindowManager,
  riverXkbBindings,
  riverLayerShell,
  readRiverInventory,
  readRiverSequencePhase,
  RiverKeyBindingState (..),
  RiverPointerBindingState (..),
  PointerBindingEvent (..),
  RiverSeatBindings (..),
  RiverLayerFocus (..),
  RiverRuntimeSnapshot (..),
  readRiverSeatBindings,
  RiverRuntimeHandlers (..),
  RenderOutcome (..),
  ManageOutcome (..),
  RiverExit (..),
  RiverRuntimeError (..),
  openRiverSession,
  openRiverSessionAt,
  runRiverSession,
  closeRiverSession,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (finally, mask, onException)
import Control.Monad (void, when, (<=<))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Protocol.RiverLayerShellV1 qualified as LayerShell
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.External (ExternalLifecycle, bindExternalWake, closeExternal, wakeExternalPending)
import Seki.Run.Inventory (
  RiverInventory,
  RiverInventoryError (..),
  RiverOutputState (..),
  RiverWindowState (..),
  addRiverOutput,
  addRiverWindow,
  applyRiverLayerShellOutputEvent,
  applyRiverOutputEvent,
  applyRiverWindowEvent,
  attachRiverOutputLayerShell,
  attachRiverWindowNode,
  emptyRiverInventory,
  lookupRiverOutput,
  lookupRiverWindow,
 )
import Seki.Run.Latency
import Seki.Run.LayerShell (
  RiverLayerShellBootstrapError,
  bindRiverLayerShell,
  createRiverLayerShellOutput,
  createRiverLayerShellSeat,
  selectRiverLayerShellGlobal,
 )
import Seki.Run.Phase (
  ManageM,
  RenderM,
  enableKeyBinding,
  enablePointerBinding,
  runManageM,
  runRenderM,
  setKeyBindingLayoutOverride,
 )
import Seki.Run.Pointer (RiverPointerBinding, createRiverPointerBinding)
import Seki.Run.River (
  RiverBootstrapError,
  RiverWindowManagerHandlers (..),
  bindRiverWindowManager,
  selectRiverWindowManagerGlobal,
 )
import Seki.Run.Sequence (
  RiverSequenceCompletion (..),
  RiverSequenceError,
  RiverSequencePhase,
  RiverSequenceState,
  completeRiverSequence,
  initialRiverSequenceState,
  riverSequencePhase,
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
import Seki.Run.Xkb (
  RiverKeyBinding (..),
  RiverXkbBootstrapError,
  bindRiverXkbBindings,
  createRiverKeyBinding,
  selectRiverXkbBindingsGlobal,
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
import Seki.Wire.Types (ObjectId)

-- | A synchronized Wayland session with its required river globals bound.
data RiverSession = RiverSession
  { riverWaylandSession :: WaylandSession
  , riverWindowManager :: Object River.RiverWindowManagerV1
  , riverXkbBindings :: Object Xkb.RiverXkbBindingsV1
  , riverLayerShell :: Object LayerShell.RiverLayerShellV1
  , riverEventLoopClaim :: MVar Bool
  , riverSequenceStatus :: IORef (Either RiverRuntimeError RiverSequenceState)
  , riverInventoryState :: IORef RiverInventory
  , riverSeatBindingsState :: IORef (Map ObjectId RiverSeatBindings)
  , riverExternalLifecycle :: Maybe ExternalLifecycle
  , riverLifecycle :: IORef Lifecycle
  }

{- | Where the manager object is in river's stop handshake: @stop@ is sent at
most once, and @finished@ ends the session after @destroy@, remembering
whether seki asked for it.
-}
data Lifecycle = Managing | Stopping | Finished Bool
  deriving (Eq)

-- | What the runtime sends after a manage sequence's @manage_finish@.
data ManageOutcome = KeepManaging | StopManaging
  deriving (Eq, Show)

-- | How a session ended without an error.
data RiverExit
  = -- | River closed the connection, or sent a @finished@ seki did not ask for.
    RiverEnded
  | -- | @finished@ answered seki's @stop@; @destroy@ was sent before the connection closed.
    RiverStopped
  deriving (Eq, Show)

data RiverKeyBindingState = RiverKeyBindingState
  { riverKeyBindingDefinition :: RiverKeyBinding
  , riverKeyBindingObject :: Object Xkb.RiverXkbBindingV1
  , riverKeyBindingInitialized :: Bool
  }
  deriving (Eq, Show)

data RiverPointerBindingState = RiverPointerBindingState
  { riverPointerBindingDefinition :: RiverPointerBinding
  , riverPointerBindingObject :: Object River.RiverPointerBindingV1
  , riverPointerBindingInitialized :: Bool
  }
  deriving (Eq, Show)

data RiverSeatBindings = RiverSeatBindings
  { riverBindingsSeat :: Object River.RiverSeatV1
  , riverSeatLayerShell :: Object LayerShell.RiverLayerShellSeatV1
  , riverSeatLayerFocusRevision :: Integer
  , riverSeatLayerFocus :: RiverLayerFocus
  , riverSeatKeyBindings :: [RiverKeyBindingState]
  , riverSeatPointerBindings :: [RiverPointerBindingState]
  , riverSeatPointerWindow :: Maybe (Object River.RiverWindowV1)
  -- ^ The window river last said the seat's pointer entered, until it leaves or closes.
  }
  deriving (Eq, Show)

-- | A pointer binding event, with the window under the seat's pointer at a press.
data PointerBindingEvent = PointerPressed (Maybe (Object River.RiverWindowV1)) | PointerReleased
  deriving (Eq, Show)

{- | Which kind of layer surface river last said owns the seat's keyboard.

The runtime only records river's latest @focus_exclusive@,
@focus_non_exclusive@, or @focus_none@. The WP6 reconciler applies the
M1-D01 rules from it, since focus requests and borders are generated there.
-}
data RiverLayerFocus
  = RiverLayerFocusNone
  | RiverLayerFocusExclusive
  | RiverLayerFocusNonExclusive
  deriving (Eq, Show)

-- | The runtime requests another manage sequence only after render_finish succeeds.
newtype RenderOutcome = RenderOutcome {renderNeedsManage :: Bool} deriving (Eq, Show)

-- | Pure inputs available to runtime manage and render planning.
data RiverRuntimeSnapshot = RiverRuntimeSnapshot
  { riverRuntimeInventory :: RiverInventory
  , riverRuntimeSeatBindings :: [RiverSeatBindings]
  }
  deriving (Eq, Show)

data RiverRuntimeHandlers = RiverRuntimeHandlers
  { riverExternalIngress :: Maybe ExternalLifecycle
  , riverProtocolHandlers :: RiverWindowManagerHandlers
  , riverKeyBindings :: [RiverKeyBinding]
  , handleRiverKeyBindingEvent :: RiverKeyBinding -> Object Xkb.RiverXkbBindingV1 -> Xkb.RiverXkbBindingV1Event OwnedFd -> IO ()
  , riverPointerBindings :: [RiverPointerBinding]
  , handleRiverPointerBindingEvent :: RiverPointerBinding -> Object River.RiverSeatV1 -> PointerBindingEvent -> IO ()
  , riverSequenceRecorder :: Maybe Recorder
  , prepareRiverManagePlan :: RiverRuntimeSnapshot -> IO (ManageM ManageOutcome)
  , prepareRiverRenderPlan :: RiverRuntimeSnapshot -> IO (RenderM RenderOutcome)
  }

data RiverRuntimeError
  = RiverRuntimeStartupError WaylandStartupError
  | RiverRuntimeBootstrapError RiverBootstrapError
  | RiverRuntimeXkbBootstrapError RiverXkbBootstrapError
  | RiverRuntimeLayerShellBootstrapError RiverLayerShellBootstrapError
  | RiverRuntimeConnectionError ConnectionError
  | RiverRuntimeRegistryError RegistryError
  | RiverRuntimeInventoryError RiverInventoryError
  | RiverRuntimeSeatAlreadyTracked ObjectId
  | RiverRuntimeSeatNotTracked ObjectId
  | RiverRuntimeSeatHandleMismatch ObjectId
  | RiverRuntimeXkbGlobalUnavailable
  | RiverRuntimeLayerShellGlobalUnavailable
  | RiverRuntimeSequenceError RiverSequenceError
  | RiverRuntimeIngressAlreadyBound
  | RiverRuntimeEventLoopAlreadyOwned
  | RiverRuntimeWindowManagerUnavailable
  deriving (Eq, Show)

-- | Discover Wayland, synchronize globals, and bind required river globals.
openRiverSession ::
  RiverRuntimeHandlers ->
  IO (Either RiverRuntimeError RiverSession)
openRiverSession = openRiverSessionWith openWaylandSession

-- | Open an explicit Wayland target and bind required river globals.
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
      Left errorValue -> closeIngress >> pure (Left (RiverRuntimeStartupError errorValue))
      Right wayland -> do
        sequenceStatus <- newIORef (Right initialRiverSequenceState)
        inventoryState <- newIORef emptyRiverInventory
        xkbGlobalState <- newIORef Nothing
        layerShellGlobalState <- newIORef Nothing
        seatBindingsState <- newIORef Map.empty
        lifecycle <- newIORef Managing
        let checkedHandlers =
              sequenceCheckedHandlers
                wayland
                sequenceStatus
                inventoryState
                xkbGlobalState
                layerShellGlobalState
                seatBindingsState
                lifecycle
                runtimeHandlers
        result <- restore (bindRuntimeGlobals wayland checkedHandlers) `onException` (closeIngress >> closeWaylandSession wayland)
        case result of
          Left errorValue -> closeIngress >> closeWaylandSession wayland >> pure (Left errorValue)
          Right (manager, xkbBindings, layerShell) -> do
            writeIORef xkbGlobalState (Just xkbBindings)
            writeIORef layerShellGlobalState (Just layerShell)
            bound <- case riverExternalIngress runtimeHandlers of
              Nothing -> pure True
              Just ingress -> bindExternalWake ingress ((== Right ()) <$> submitRequest (waylandConnection wayland) River.riverWindowManagerV1Interface manager River.encodeRiverWindowManagerV1ManageDirty)
            if not bound
              then closeWaylandSession wayland >> pure (Left RiverRuntimeIngressAlreadyBound)
              else do
                eventLoopClaim <- newMVar False
                pure
                  ( Right
                      ( RiverSession
                          wayland
                          manager
                          xkbBindings
                          layerShell
                          eventLoopClaim
                          sequenceStatus
                          inventoryState
                          seatBindingsState
                          (riverExternalIngress runtimeHandlers)
                          lifecycle
                      )
                  )
 where
  closeIngress = mapM_ closeExternal (riverExternalIngress runtimeHandlers)

bindRuntimeGlobals ::
  WaylandSession ->
  RiverWindowManagerHandlers ->
  IO
    ( Either
        RiverRuntimeError
        ( Object River.RiverWindowManagerV1
        , Object Xkb.RiverXkbBindingsV1
        , Object LayerShell.RiverLayerShellV1
        )
    )
bindRuntimeGlobals wayland handlers =
  readWaylandGlobalCatalog wayland >>= \case
    Left errorValue -> pure (Left (RiverRuntimeRegistryError errorValue))
    Right catalog -> case selectRiverWindowManagerGlobal catalog of
      Left errorValue -> pure (Left (RiverRuntimeBootstrapError errorValue))
      Right _ -> case selectRiverXkbBindingsGlobal catalog of
        Left errorValue -> pure (Left (RiverRuntimeXkbBootstrapError errorValue))
        Right _ -> case selectRiverLayerShellGlobal catalog of
          Left errorValue -> pure (Left (RiverRuntimeLayerShellBootstrapError errorValue))
          Right _ ->
            bindRiverWindowManager connection registry catalog handlers >>= \case
              Left errorValue -> pure (Left (RiverRuntimeBootstrapError errorValue))
              Right manager ->
                bindRiverXkbBindings connection registry catalog >>= \case
                  Left errorValue -> pure (Left (RiverRuntimeXkbBootstrapError errorValue))
                  Right xkbBindings ->
                    bindRiverLayerShell connection registry catalog >>= \case
                      Left errorValue -> pure (Left (RiverRuntimeLayerShellBootstrapError errorValue))
                      Right layerShell -> pure (Right (manager, xkbBindings, layerShell))
 where
  connection = waylandConnection wayland
  registry = waylandRegistry wayland

{- | Own the event loop until clean EOF, @finished@, or failure, then close the
session. The close happens on the loop's own thread, which the non-threaded
runtime requires.

Handler exceptions are rethrown after the underlying connection has been
closed by the connection and runtime layers.
-}
runRiverSession :: RiverSession -> IO (Either RiverRuntimeError RiverExit)
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
      Right DispatchFinished -> pure (Right RiverEnded)
      Right (EventDispatched _) ->
        readIORef (riverSequenceStatus session) >>= \case
          Left errorValue -> pure (Left errorValue)
          Right _ ->
            readIORef (riverLifecycle session) >>= \case
              Finished requested -> pure (Right (if requested then RiverStopped else RiverEnded))
              _ ->
                readWaylandGlobalCatalog wayland >>= \case
                  Left errorValue -> pure (Left (RiverRuntimeRegistryError errorValue))
                  Right _ -> eventLoop

-- | Close a runtime without running it. Repeated calls are safe.
closeRiverSession :: RiverSession -> IO ()
closeRiverSession session = do
  mapM_ closeExternal (riverExternalLifecycle session)
  closeWaylandSession (riverWaylandSession session)

-- | Read the latest window/output snapshot accumulated by event dispatch.
readRiverInventory :: RiverSession -> IO RiverInventory
readRiverInventory = readIORef . riverInventoryState

readRiverSequencePhase :: RiverSession -> IO (Either RiverRuntimeError RiverSequencePhase)
readRiverSequencePhase session = fmap riverSequencePhase <$> readIORef (riverSequenceStatus session)

-- | Read the live seats and their initially configured key binding objects.
readRiverSeatBindings :: RiverSession -> IO [RiverSeatBindings]
readRiverSeatBindings session =
  Map.elems <$> readIORef (riverSeatBindingsState session)

sequenceCheckedHandlers ::
  WaylandSession ->
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  IORef RiverInventory ->
  IORef (Maybe (Object Xkb.RiverXkbBindingsV1)) ->
  IORef (Maybe (Object LayerShell.RiverLayerShellV1)) ->
  IORef (Map ObjectId RiverSeatBindings) ->
  IORef Lifecycle ->
  RiverRuntimeHandlers ->
  RiverWindowManagerHandlers
sequenceCheckedHandlers wayland sequenceStatus inventoryState xkbGlobalState layerShellGlobalState seatBindingsState lifecycle runtimeHandlers =
  handlers
    { handleRiverWindowManagerEvent = handleManagerEvent
    , handleRiverWindowCreated = handleWindowCreated
    , handleRiverWindowEvent = handleWindowEvent
    , handleRiverOutputCreated = handleOutputCreated
    , handleRiverOutputEvent = handleOutputEvent
    , handleRiverSeatCreated = handleSeatCreated
    , handleRiverSeatEvent = handleSeatEvent
    }
 where
  handleManagerEvent manager event = case event of
    River.RiverWindowManagerV1ManageStart ->
      runSequence
        (riverSequenceRecorder runtimeHandlers)
        ManageSequence
        (waylandConnection wayland)
        sequenceStatus
        handlers
        manager
        event
        startManageSequence
        runManagePlans
        ( \case
            KeepManaging -> mapM_ wakeExternalPending (riverExternalIngress runtimeHandlers)
            StopManaging -> do
              mapM_ closeExternal (riverExternalIngress runtimeHandlers)
              readIORef lifecycle >>= \case
                Managing -> submitManagerRequest manager River.encodeRiverWindowManagerV1Stop >> writeIORef lifecycle Stopping
                _ -> pure ()
        )
    River.RiverWindowManagerV1RenderStart ->
      runSequence
        (riverSequenceRecorder runtimeHandlers)
        RenderSequence
        (waylandConnection wayland)
        sequenceStatus
        handlers
        manager
        event
        startRenderSequence
        (readRuntimeSnapshot >>= prepareRiverRenderPlan runtimeHandlers >>= runRenderM connection)
        (\outcome -> when (renderNeedsManage outcome) (submitManagerRequest manager River.encodeRiverWindowManagerV1ManageDirty))
    River.RiverWindowManagerV1Unavailable -> recordRuntimeError sequenceStatus RiverRuntimeWindowManagerUnavailable
    River.RiverWindowManagerV1Finished -> do
      mapM_ closeExternal (riverExternalIngress runtimeHandlers)
      submitManagerRequest manager River.encodeRiverWindowManagerV1Destroy
      atomicModifyIORef' lifecycle (\state -> (Finished (state == Stopping), ()))
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
        River.RiverWindowV1Closed -> do
          atomicModifyIORef' seatBindingsState (\seats -> (fmap (unhover window) seats, ()))
          case previous of
            Nothing -> pure ()
            Just state -> destroyWindow state
        _ -> pure ()
  handleOutputCreated output = do
    added <- updateInventory sequenceStatus inventoryState (addRiverOutput output)
    when added $
      readIORef layerShellGlobalState >>= \case
        Nothing ->
          recordRuntimeError sequenceStatus RiverRuntimeLayerShellGlobalUnavailable
        Just layerShell ->
          createRiverLayerShellOutput
            connection
            layerShell
            output
            (handleLayerShellOutputEvent output)
            >>= \case
              Left errorValue ->
                recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
              Right layerShellOutput -> do
                attached <-
                  updateInventory
                    sequenceStatus
                    inventoryState
                    (attachRiverOutputLayerShell output layerShellOutput)
                when attached (handleRiverOutputCreated handlers output)
  handleLayerShellOutputEvent output event =
    void $
      updateInventory
        sequenceStatus
        inventoryState
        (applyRiverLayerShellOutputEvent output event)
  handleOutputEvent output event = do
    previous <- lookupRiverOutput (objectId output) <$> readIORef inventoryState
    updated <-
      updateInventory
        sequenceStatus
        inventoryState
        (applyRiverOutputEvent output event)
    when updated $ do
      handleRiverOutputEvent handlers output event
      case event of
        River.RiverOutputV1Removed ->
          destroyLayerShellOutput (previous >>= riverOutputLayerShell) >>= \case
            False -> pure ()
            True ->
              submitRequest
                connection
                River.riverOutputV1Interface
                output
                River.encodeRiverOutputV1Destroy
                >>= recordConnectionResult
        _ -> pure ()
  destroyLayerShellOutput = \case
    Nothing -> pure True
    Just layerShellOutput ->
      submitRequest
        connection
        LayerShell.riverLayerShellOutputV1Interface
        layerShellOutput
        LayerShell.encodeRiverLayerShellOutputV1Destroy
        >>= \case
          Left errorValue -> do
            recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
            pure False
          Right () -> pure True
  handleSeatCreated seat = do
    untracked <- requireSeatUntracked seat
    when untracked $
      readIORef layerShellGlobalState >>= \case
        Nothing ->
          recordRuntimeError sequenceStatus RiverRuntimeLayerShellGlobalUnavailable
        Just layerShell ->
          createRiverLayerShellSeat connection layerShell seat (handleLayerShellSeatEvent seat)
            >>= \case
              Left errorValue ->
                recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
              Right layerShellSeat -> do
                reserved <- reserveSeat seat layerShellSeat
                when reserved $
                  readIORef xkbGlobalState >>= \case
                    Nothing ->
                      recordRuntimeError sequenceStatus RiverRuntimeXkbGlobalUnavailable
                    Just xkbBindings -> do
                      created <- createBindings xkbBindings seat (riverKeyBindings runtimeHandlers)
                      pointerCreated <- if created then createPointerBindings seat (riverPointerBindings runtimeHandlers) else pure False
                      when pointerCreated (handleRiverSeatCreated handlers seat)
  handleLayerShellSeatEvent seat event = do
    let focus = case event of
          LayerShell.RiverLayerShellSeatV1FocusExclusive -> RiverLayerFocusExclusive
          LayerShell.RiverLayerShellSeatV1FocusNonExclusive -> RiverLayerFocusNonExclusive
          LayerShell.RiverLayerShellSeatV1FocusNone -> RiverLayerFocusNone
    result <-
      atomicModifyIORef' seatBindingsState $ \seats ->
        case Map.lookup (objectId seat) seats of
          Nothing -> (seats, Left (RiverRuntimeSeatNotTracked (objectId seat)))
          Just state ->
            (Map.insert (objectId seat) state{riverSeatLayerFocus = focus, riverSeatLayerFocusRevision = riverSeatLayerFocusRevision state + 1} seats, Right ())
    void (recordStateResult result)
  handleSeatEvent seat event = do
    tracked <- requireSeat seat
    when tracked $ do
      previous <- Map.lookup (objectId seat) <$> readIORef seatBindingsState
      validInteraction <- case event of
        River.RiverSeatV1WindowInteraction identifier -> do
          window <- lookupRiverWindow identifier <$> readIORef inventoryState
          recordStateResult $ case window of
            Nothing -> Left (RiverRuntimeInventoryError (RiverInventoryWindowNotTracked identifier))
            Just _ -> Right ()
        _ -> pure True
      when validInteraction $ do
        case event of
          River.RiverSeatV1PointerEnter identifier ->
            hover seat . fmap riverWindowObject . lookupRiverWindow identifier =<< readIORef inventoryState
          River.RiverSeatV1PointerLeave -> hover seat Nothing
          _ -> pure ()
        case event of
          River.RiverSeatV1Removed -> do
            removed <- removeSeat seat
            when removed $ do
              handleRiverSeatEvent handlers seat event
              case previous of
                Nothing -> pure ()
                Just state -> destroySeat state
          _ -> handleRiverSeatEvent handlers seat event
  hover seat window =
    atomicModifyIORef' seatBindingsState (\seats -> (Map.adjust (\state -> state{riverSeatPointerWindow = window}) (objectId seat) seats, ()))
  requireSeatUntracked seat = do
    seats <- readIORef seatBindingsState
    recordStateResult $
      if Map.member (objectId seat) seats
        then Left (RiverRuntimeSeatAlreadyTracked (objectId seat))
        else Right ()
  reserveSeat seat layerShellSeat = do
    result <-
      atomicModifyIORef' seatBindingsState $ \seats ->
        if Map.member (objectId seat) seats
          then (seats, Left (RiverRuntimeSeatAlreadyTracked (objectId seat)))
          else
            ( Map.insert
                (objectId seat)
                (RiverSeatBindings seat layerShellSeat 0 RiverLayerFocusNone [] [] Nothing)
                seats
            , Right ()
            )
    recordStateResult result
  requireSeat seat = do
    seats <- readIORef seatBindingsState
    recordStateResult $ case Map.lookup (objectId seat) seats of
      Nothing -> Left (RiverRuntimeSeatNotTracked (objectId seat))
      Just state
        | riverBindingsSeat state /= seat ->
            Left (RiverRuntimeSeatHandleMismatch (objectId seat))
        | otherwise -> Right ()
  removeSeat seat = do
    result <-
      atomicModifyIORef' seatBindingsState $ \seats ->
        case Map.lookup (objectId seat) seats of
          Nothing -> (seats, Left (RiverRuntimeSeatNotTracked (objectId seat)))
          Just state
            | riverBindingsSeat state /= seat ->
                (seats, Left (RiverRuntimeSeatHandleMismatch (objectId seat)))
            | otherwise -> (Map.delete (objectId seat) seats, Right ())
    recordStateResult result
  recordStateResult = \case
    Left errorValue -> recordRuntimeError sequenceStatus errorValue >> pure False
    Right () -> pure True
  createBindings _ _ [] = pure True
  createBindings xkbBindings seat (definition : rest) =
    createRiverKeyBinding
      connection
      xkbBindings
      seat
      definition
      (handleBindingEvent definition)
      >>= \case
        Left errorValue -> do
          recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
          pure False
        Right binding -> do
          atomicModifyIORef' seatBindingsState $ \seats ->
            ( Map.adjust
                ( \state ->
                    state
                      { riverSeatKeyBindings =
                          riverSeatKeyBindings state
                            <> [RiverKeyBindingState definition binding False]
                      }
                )
                (objectId seat)
                seats
            , ()
            )
          createBindings xkbBindings seat rest
  handleBindingEvent = handleRiverKeyBindingEvent runtimeHandlers
  createPointerBindings _ [] = pure True
  createPointerBindings seat (definition : rest) =
    createRiverPointerBinding connection seat definition (\_ event -> handlePointerBindingEvent seat definition event) >>= \case
      Left errorValue -> do
        recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
        pure False
      Right binding -> do
        atomicModifyIORef' seatBindingsState $ \seats ->
          ( Map.adjust
              (\state -> state{riverSeatPointerBindings = riverSeatPointerBindings state <> [RiverPointerBindingState definition binding False]})
              (objectId seat)
              seats
          , ()
          )
        createPointerBindings seat rest
  handlePointerBindingEvent seat definition event = do
    hovered <- (riverSeatPointerWindow <=< Map.lookup (objectId seat)) <$> readIORef seatBindingsState
    handleRiverPointerBindingEvent runtimeHandlers definition seat $ case event of
      River.RiverPointerBindingV1Pressed -> PointerPressed hovered
      River.RiverPointerBindingV1Released -> PointerReleased
  runManagePlans = do
    snapshot <- readRuntimeSnapshot
    policyPlan <- prepareRiverManagePlan runtimeHandlers snapshot
    let pending =
          [ bindingState
          | seatState <- riverRuntimeSeatBindings snapshot
          , bindingState <- riverSeatKeyBindings seatState
          , not (riverKeyBindingInitialized bindingState)
          ]
        pendingPointer =
          [ riverPointerBindingObject bindingState
          | seatState <- riverRuntimeSeatBindings snapshot
          , bindingState <- riverSeatPointerBindings seatState
          , not (riverPointerBindingInitialized bindingState)
          ]
        plan = do
          mapM_ prepareKeyBinding pending
          mapM_ enablePointerBinding pendingPointer
          policyPlan
    runManageM connection plan >>= \case
      Left errorValue -> pure (Left errorValue)
      Right outcome -> do
        markKeyBindingsInitialized
        pure (Right outcome)
  readRuntimeSnapshot = do
    inventory <- readIORef inventoryState
    seats <- Map.elems <$> readIORef seatBindingsState
    pure (RiverRuntimeSnapshot inventory seats)
  prepareKeyBinding bindingState = do
    let definition = riverKeyBindingDefinition bindingState
        binding = riverKeyBindingObject bindingState
    case riverKeyBindingLayoutOverride definition of
      Nothing -> pure ()
      Just layout -> setKeyBindingLayoutOverride binding layout
    enableKeyBinding binding
  markKeyBindingsInitialized =
    atomicModifyIORef' seatBindingsState $ \seats ->
      ( fmap
          ( \seatState ->
              seatState
                { riverSeatKeyBindings =
                    map
                      (\bindingState -> bindingState{riverKeyBindingInitialized = True})
                      (riverSeatKeyBindings seatState)
                , riverSeatPointerBindings =
                    map
                      (\bindingState -> bindingState{riverPointerBindingInitialized = True})
                      (riverSeatPointerBindings seatState)
                }
          )
          seats
      , ()
      )
  destroySeat state =
    destroyBindings (riverSeatKeyBindings state) >>= \case
      False -> pure ()
      True ->
        destroyPointerBindings (riverSeatPointerBindings state) >>= \case
          False -> pure ()
          True ->
            submitRequest
              connection
              LayerShell.riverLayerShellSeatV1Interface
              (riverSeatLayerShell state)
              LayerShell.encodeRiverLayerShellSeatV1Destroy
              >>= \case
                Left errorValue ->
                  recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
                Right () ->
                  submitRequest
                    connection
                    River.riverSeatV1Interface
                    (riverBindingsSeat state)
                    River.encodeRiverSeatV1Destroy
                    >>= recordConnectionResult
  destroyPointerBindings [] = pure True
  destroyPointerBindings (bindingState : rest) =
    submitRequest
      connection
      River.riverPointerBindingV1Interface
      (riverPointerBindingObject bindingState)
      River.encodeRiverPointerBindingV1Destroy
      >>= \case
        Left errorValue -> do
          recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
          pure False
        Right () -> destroyPointerBindings rest
  destroyBindings [] = pure True
  destroyBindings (bindingState : rest) =
    submitRequest
      connection
      Xkb.riverXkbBindingV1Interface
      (riverKeyBindingObject bindingState)
      Xkb.encodeRiverXkbBindingV1Destroy
      >>= \case
        Left errorValue -> do
          recordRuntimeError sequenceStatus (RiverRuntimeConnectionError errorValue)
          pure False
        Right () -> destroyBindings rest
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
  submitManagerRequest manager request =
    submitRequest connection River.riverWindowManagerV1Interface manager request >>= recordConnectionResult
  handlers = riverProtocolHandlers runtimeHandlers
  connection = waylandConnection wayland

unhover :: Object River.RiverWindowV1 -> RiverSeatBindings -> RiverSeatBindings
unhover window state
  | riverSeatPointerWindow state == Just window = state{riverSeatPointerWindow = Nothing}
  | otherwise = state

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
  Maybe Recorder ->
  SequenceKind ->
  Connection ->
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  RiverWindowManagerHandlers ->
  Object River.RiverWindowManagerV1 ->
  River.RiverWindowManagerV1Event OwnedFd ->
  (RiverSequenceState -> Either RiverSequenceError RiverSequenceState) ->
  IO (Either ConnectionError outcome) ->
  (outcome -> IO ()) ->
  IO ()
runSequence recorder kind connection sequenceStatus handlers manager event startSequence runPlan afterFinish = do
  result <- case recorder of
    Nothing -> submit
    Just recording -> do
      started <- getMonotonicTimeNSec
      completed <- submit `onException` recordFailure recording
      case completed of
        Nothing -> recordFailure recording
        Just _ -> do
          ended <- getMonotonicTimeNSec
          recordSuccess recording kind started ended
      pure completed
  case result of
    Nothing -> pure ()
    Just (next, outcome) -> do
      writeIORef sequenceStatus (Right next)
      afterFinish outcome
 where
  failed errorValue = writeIORef sequenceStatus (Left errorValue) >> pure Nothing
  submit = do
    started <-
      atomicModifyIORef' sequenceStatus $ \current -> case current of
        Left errorValue -> (current, Left errorValue)
        Right state -> case startSequence state of
          Left errorValue ->
            let runtimeError = RiverRuntimeSequenceError errorValue
             in (Left runtimeError, Left runtimeError)
          Right active -> (Right active, Right active)
    case started of
      Left _ -> pure Nothing
      Right active -> do
        handleRiverWindowManagerEvent handlers manager event
        runPlan >>= \case
          Left errorValue -> failed (RiverRuntimeConnectionError errorValue)
          Right outcome ->
            case completeRiverSequence active of
              Left errorValue -> failed (RiverRuntimeSequenceError errorValue)
              Right (completion, next) ->
                submitSequenceCompletion connection manager completion >>= \case
                  Left errorValue -> failed (RiverRuntimeConnectionError errorValue)
                  Right () -> pure (Just (next, outcome))

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
