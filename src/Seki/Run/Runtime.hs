{-# LANGUAGE LambdaCase #-}

-- | Owned river runtime startup and continuous Wayland event dispatch.
module Seki.Run.Runtime (
  RiverSession,
  riverWaylandSession,
  riverWindowManager,
  RiverRuntimeError (..),
  openRiverSession,
  openRiverSessionAt,
  runRiverSession,
  closeRiverSession,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (finally, mask, onException)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
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
  submitRequest,
 )
import Seki.Wire.Connection.ObjectTable (Object, ObjectLimit)
import Seki.Wire.Connection.Registry (RegistryError)
import Seki.Wire.Transport (ConnectionTarget, OwnedFd)

-- | A synchronized Wayland session with the river manager bound and routed.
data RiverSession = RiverSession
  { riverWaylandSession :: WaylandSession
  , riverWindowManager :: Object River.RiverWindowManagerV1
  , riverEventLoopClaim :: MVar Bool
  , riverSequenceStatus :: IORef (Either RiverRuntimeError RiverSequenceState)
  }

data RiverRuntimeError
  = RiverRuntimeStartupError WaylandStartupError
  | RiverRuntimeBootstrapError RiverBootstrapError
  | RiverRuntimeConnectionError ConnectionError
  | RiverRuntimeRegistryError RegistryError
  | RiverRuntimeSequenceError RiverSequenceError
  | RiverRuntimeEventLoopAlreadyOwned
  deriving (Eq, Show)

-- | Discover Wayland, synchronize globals, and bind the river manager.
openRiverSession ::
  RiverWindowManagerHandlers ->
  IO (Either RiverRuntimeError RiverSession)
openRiverSession = openRiverSessionWith openWaylandSession

-- | Open an explicit Wayland target and bind its river manager.
openRiverSessionAt ::
  ConnectionTarget ->
  ObjectLimit ->
  RiverWindowManagerHandlers ->
  IO (Either RiverRuntimeError RiverSession)
openRiverSessionAt target limit =
  openRiverSessionWith (openWaylandSessionAt target limit)

openRiverSessionWith ::
  IO (Either WaylandStartupError WaylandSession) ->
  RiverWindowManagerHandlers ->
  IO (Either RiverRuntimeError RiverSession)
openRiverSessionWith openWayland handlers =
  mask $ \restore -> do
    restore openWayland >>= \case
      Left errorValue -> pure (Left (RiverRuntimeStartupError errorValue))
      Right wayland -> do
        sequenceStatus <- newIORef (Right initialRiverSequenceState)
        let checkedHandlers = sequenceCheckedHandlers wayland sequenceStatus handlers
        result <- restore (bindManager wayland checkedHandlers) `onException` closeWaylandSession wayland
        case result of
          Left errorValue -> closeWaylandSession wayland >> pure (Left errorValue)
          Right manager -> do
            eventLoopClaim <- newMVar False
            pure (Right (RiverSession wayland manager eventLoopClaim sequenceStatus))

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

sequenceCheckedHandlers ::
  WaylandSession ->
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  RiverWindowManagerHandlers ->
  RiverWindowManagerHandlers
sequenceCheckedHandlers wayland sequenceStatus handlers =
  handlers
    { handleRiverWindowManagerEvent = handleManagerEvent
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
    River.RiverWindowManagerV1RenderStart ->
      runSequence
        (waylandConnection wayland)
        sequenceStatus
        handlers
        manager
        event
        startRenderSequence
    _ -> handleRiverWindowManagerEvent handlers manager event

runSequence ::
  Connection ->
  IORef (Either RiverRuntimeError RiverSequenceState) ->
  RiverWindowManagerHandlers ->
  Object River.RiverWindowManagerV1 ->
  River.RiverWindowManagerV1Event OwnedFd ->
  (RiverSequenceState -> Either RiverSequenceError RiverSequenceState) ->
  IO ()
runSequence connection sequenceStatus handlers manager event startSequence = do
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
