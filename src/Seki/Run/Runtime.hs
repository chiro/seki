{-# LANGUAGE LambdaCase #-}

-- | Owned river runtime startup and continuous Wayland event dispatch.
module Seki.Run.Runtime (
  RiverSession,
  riverWaylandSession,
  riverWindowManager,
  riverXkbBindings,
  readRiverInventory,
  RiverKeyBindingState (..),
  RiverSeatBindings (..),
  readRiverSeatBindings,
  RiverManageAction (..),
  noRiverManageAction,
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
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
import Seki.Run.Phase (
  ManageM,
  RenderM,
  enableKeyBinding,
  runManageM,
  runRenderM,
  setKeyBindingLayoutOverride,
 )
import Seki.Run.River (
  RiverBootstrapError,
  RiverWindowManagerHandlers (..),
  bindRiverWindowManager,
  selectRiverWindowManagerGlobal,
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
  , riverEventLoopClaim :: MVar Bool
  , riverSequenceStatus :: IORef (Either RiverRuntimeError RiverSequenceState)
  , riverInventoryState :: IORef RiverInventory
  , riverSeatBindingsState :: IORef (Map ObjectId RiverSeatBindings)
  }

data RiverKeyBindingState = RiverKeyBindingState
  { riverKeyBindingDefinition :: RiverKeyBinding
  , riverKeyBindingObject :: Object Xkb.RiverXkbBindingV1
  , riverKeyBindingInitialized :: Bool
  }
  deriving (Eq, Show)

data RiverSeatBindings = RiverSeatBindings
  { riverBindingsSeat :: Object River.RiverSeatV1
  , riverSeatKeyBindings :: [RiverKeyBindingState]
  }
  deriving (Eq, Show)

-- | A deferred management action evaluated against the next inventory snapshot.
newtype RiverManageAction = RiverManageAction
  { prepareRiverManageAction :: RiverInventory -> ManageM ()
  }

noRiverManageAction :: RiverManageAction
noRiverManageAction = RiverManageAction (const (pure ()))

{- | Low-level river event handlers plus phase-safe plans prepared at each
sequence boundary.

Plan preparation is a pure function of the runtime-owned inventory, and the
returned plans cannot lift arbitrary IO.
-}
data RiverRuntimeHandlers = RiverRuntimeHandlers
  { riverProtocolHandlers :: RiverWindowManagerHandlers
  , riverKeyBindings :: [RiverKeyBinding]
  , handleRiverKeyBindingEvent :: RiverKeyBinding -> Object Xkb.RiverXkbBindingV1 -> Xkb.RiverXkbBindingV1Event OwnedFd -> IO RiverManageAction
  , prepareRiverManagePlan :: RiverInventory -> ManageM ()
  , prepareRiverRenderPlan :: RiverInventory -> RenderM ()
  }

data RiverRuntimeError
  = RiverRuntimeStartupError WaylandStartupError
  | RiverRuntimeBootstrapError RiverBootstrapError
  | RiverRuntimeXkbBootstrapError RiverXkbBootstrapError
  | RiverRuntimeConnectionError ConnectionError
  | RiverRuntimeRegistryError RegistryError
  | RiverRuntimeInventoryError RiverInventoryError
  | RiverRuntimeSeatAlreadyTracked ObjectId
  | RiverRuntimeSeatNotTracked ObjectId
  | RiverRuntimeSeatHandleMismatch ObjectId
  | RiverRuntimeXkbGlobalUnavailable
  | RiverRuntimeSequenceError RiverSequenceError
  | RiverRuntimeEventLoopAlreadyOwned
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
      Left errorValue -> pure (Left (RiverRuntimeStartupError errorValue))
      Right wayland -> do
        sequenceStatus <- newIORef (Right initialRiverSequenceState)
        inventoryState <- newIORef emptyRiverInventory
        xkbGlobalState <- newIORef Nothing
        seatBindingsState <- newIORef Map.empty
        manageActionsState <- newIORef []
        let checkedHandlers =
              sequenceCheckedHandlers
                wayland
                sequenceStatus
                inventoryState
                xkbGlobalState
                seatBindingsState
                manageActionsState
                runtimeHandlers
        result <- restore (bindRuntimeGlobals wayland checkedHandlers) `onException` closeWaylandSession wayland
        case result of
          Left errorValue -> closeWaylandSession wayland >> pure (Left errorValue)
          Right (manager, xkbBindings) -> do
            writeIORef xkbGlobalState (Just xkbBindings)
            eventLoopClaim <- newMVar False
            pure
              ( Right
                  ( RiverSession
                      wayland
                      manager
                      xkbBindings
                      eventLoopClaim
                      sequenceStatus
                      inventoryState
                      seatBindingsState
                  )
              )

bindRuntimeGlobals ::
  WaylandSession ->
  RiverWindowManagerHandlers ->
  IO
    ( Either
        RiverRuntimeError
        ( Object River.RiverWindowManagerV1
        , Object Xkb.RiverXkbBindingsV1
        )
    )
bindRuntimeGlobals wayland handlers =
  readWaylandGlobalCatalog wayland >>= \case
    Left errorValue -> pure (Left (RiverRuntimeRegistryError errorValue))
    Right catalog -> case selectRiverWindowManagerGlobal catalog of
      Left errorValue -> pure (Left (RiverRuntimeBootstrapError errorValue))
      Right _ -> case selectRiverXkbBindingsGlobal catalog of
        Left errorValue -> pure (Left (RiverRuntimeXkbBootstrapError errorValue))
        Right _ ->
          bindRiverWindowManager connection registry catalog handlers >>= \case
            Left errorValue -> pure (Left (RiverRuntimeBootstrapError errorValue))
            Right manager ->
              bindRiverXkbBindings connection registry catalog >>= \case
                Left errorValue -> pure (Left (RiverRuntimeXkbBootstrapError errorValue))
                Right xkbBindings -> pure (Right (manager, xkbBindings))
 where
  connection = waylandConnection wayland
  registry = waylandRegistry wayland

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

-- | Read the live seats and their initially configured key binding objects.
readRiverSeatBindings :: RiverSession -> IO [RiverSeatBindings]
readRiverSeatBindings session =
  Map.elems <$> readIORef (riverSeatBindingsState session)

sequenceCheckedHandlers ::
  WaylandSession ->
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  IORef RiverInventory ->
  IORef (Maybe (Object Xkb.RiverXkbBindingsV1)) ->
  IORef (Map ObjectId RiverSeatBindings) ->
  IORef [RiverManageAction] ->
  RiverRuntimeHandlers ->
  RiverWindowManagerHandlers
sequenceCheckedHandlers wayland sequenceStatus inventoryState xkbGlobalState seatBindingsState manageActionsState runtimeHandlers =
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
        (waylandConnection wayland)
        sequenceStatus
        handlers
        manager
        event
        startManageSequence
        runManagePlans
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
  handleSeatCreated seat = do
    reserved <- reserveSeat seat
    when reserved $
      readIORef xkbGlobalState >>= \case
        Nothing ->
          recordRuntimeError sequenceStatus RiverRuntimeXkbGlobalUnavailable
        Just xkbBindings -> do
          created <- createBindings xkbBindings seat (riverKeyBindings runtimeHandlers)
          when created (handleRiverSeatCreated handlers seat)
  handleSeatEvent seat event = do
    tracked <- requireSeat seat
    when tracked $ do
      previous <- Map.lookup (objectId seat) <$> readIORef seatBindingsState
      case event of
        River.RiverSeatV1Removed -> do
          removed <- removeSeat seat
          when removed $ do
            handleRiverSeatEvent handlers seat event
            case previous of
              Nothing -> pure ()
              Just state -> destroySeat state
        _ -> handleRiverSeatEvent handlers seat event
  reserveSeat seat = do
    result <-
      atomicModifyIORef' seatBindingsState $ \seats ->
        if Map.member (objectId seat) seats
          then (seats, Left (RiverRuntimeSeatAlreadyTracked (objectId seat)))
          else
            ( Map.insert (objectId seat) (RiverSeatBindings seat []) seats
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
  handleBindingEvent definition binding event = do
    action <- handleRiverKeyBindingEvent runtimeHandlers definition binding event
    atomicModifyIORef' manageActionsState $ \actions ->
      (actions <> [action], ())
  runManagePlans = do
    inventory <- readIORef inventoryState
    seats <- readIORef seatBindingsState
    actions <- readIORef manageActionsState
    let pending =
          [ bindingState
          | seatState <- Map.elems seats
          , bindingState <- riverSeatKeyBindings seatState
          , not (riverKeyBindingInitialized bindingState)
          ]
        plan = do
          mapM_ prepareKeyBinding pending
          mapM_ (\action -> prepareRiverManageAction action inventory) actions
          prepareRiverManagePlan runtimeHandlers inventory
    runManageM connection plan >>= \case
      Left errorValue -> pure (Left errorValue)
      Right () -> do
        markKeyBindingsInitialized
        writeIORef manageActionsState []
        pure (Right ())
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
                }
          )
          seats
      , ()
      )
  destroySeat state =
    destroyBindings (riverSeatKeyBindings state) >>= \case
      False -> pure ()
      True ->
        submitRequest
          connection
          River.riverSeatV1Interface
          (riverBindingsSeat state)
          River.encodeRiverSeatV1Destroy
          >>= recordConnectionResult
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
