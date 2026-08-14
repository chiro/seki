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
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.River (
  RiverBootstrapError,
  RiverWindowManagerHandlers,
  bindRiverWindowManager,
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
  ConnectionError,
  DispatchResult (..),
  dispatchNextEvent,
 )
import Seki.Wire.Connection.ObjectTable (Object, ObjectLimit)
import Seki.Wire.Connection.Registry (RegistryError)
import Seki.Wire.Transport (ConnectionTarget)

-- | A synchronized Wayland session with the river manager bound and routed.
data RiverSession = RiverSession
  { riverWaylandSession :: WaylandSession
  , riverWindowManager :: Object River.RiverWindowManagerV1
  , riverEventLoopClaim :: MVar Bool
  }

data RiverRuntimeError
  = RiverRuntimeStartupError WaylandStartupError
  | RiverRuntimeBootstrapError RiverBootstrapError
  | RiverRuntimeConnectionError ConnectionError
  | RiverRuntimeRegistryError RegistryError
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
        result <- restore (bindManager wayland handlers) `onException` closeWaylandSession wayland
        case result of
          Left errorValue -> closeWaylandSession wayland >> pure (Left errorValue)
          Right manager -> do
            eventLoopClaim <- newMVar False
            pure (Right (RiverSession wayland manager eventLoopClaim))

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
        readWaylandGlobalCatalog wayland >>= \case
          Left errorValue -> pure (Left (RiverRuntimeRegistryError errorValue))
          Right _ -> eventLoop

-- | Close a runtime without running it. Repeated calls are safe.
closeRiverSession :: RiverSession -> IO ()
closeRiverSession = closeWaylandSession . riverWaylandSession
