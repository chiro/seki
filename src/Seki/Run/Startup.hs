{-# LANGUAGE LambdaCase #-}

-- | Wayland connection startup and initial registry synchronization.
module Seki.Run.Startup (
  WaylandSession,
  waylandConnection,
  waylandDisplay,
  waylandRegistry,
  readWaylandGlobalCatalog,
  WaylandStartupError (..),
  openWaylandSession,
  openWaylandSessionAt,
  closeWaylandSession,
)
where

import Control.Exception (mask, onException)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  DispatchResult (..),
  closeConnection,
  dispatchNextEvent,
  eventRoute,
  newConnection,
  registerEventRoute,
  submitNewObjectRequestWithRoute,
  wlDisplayEventRoute,
 )
import Seki.Wire.Connection.ObjectTable (Object, ObjectLimit, defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry (
  GlobalCatalog,
  RegistryError,
  applyRegistryEvent,
  emptyGlobalCatalog,
 )
import Seki.Wire.Transport (
  ConnectionDiscoveryError,
  ConnectionTarget,
  TransportError,
  closeTransport,
  discoverConnectionTarget,
  openTransport,
 )

-- | An open connection and its continuously maintained registry catalog.
data WaylandSession = WaylandSession
  { waylandConnection :: Connection
  , waylandDisplay :: Object Wayland.WlDisplay
  , waylandRegistry :: Object Wayland.WlRegistry
  , waylandCatalogState :: IORef (Either RegistryError GlobalCatalog)
  }

data WaylandStartupError
  = WaylandConnectionDiscoveryError ConnectionDiscoveryError
  | WaylandTransportOpenError TransportError
  | WaylandStartupConnectionError ConnectionError
  | WaylandRegistrySyncError RegistryError
  | WaylandRegistrySyncFinished
  deriving (Eq, Show)

-- | Read the latest catalog, including registry changes after initial sync.
readWaylandGlobalCatalog :: WaylandSession -> IO (Either RegistryError GlobalCatalog)
readWaylandGlobalCatalog = readIORef . waylandCatalogState

-- | Discover the standard Wayland target and synchronize its initial globals.
openWaylandSession :: IO (Either WaylandStartupError WaylandSession)
openWaylandSession = do
  discoverConnectionTarget >>= \case
    Left errorValue -> pure (Left (WaylandConnectionDiscoveryError errorValue))
    Right target -> openWaylandSessionAt target defaultObjectLimit

-- | Open an explicit target, primarily for nested displays and tests.
openWaylandSessionAt ::
  ConnectionTarget ->
  ObjectLimit ->
  IO (Either WaylandStartupError WaylandSession)
openWaylandSessionAt target limit =
  mask $ \restore -> do
    openTransport target >>= \case
      Left errorValue -> pure (Left (WaylandTransportOpenError errorValue))
      Right transport -> do
        initialized <- restore (newConnection transport limit Wayland.wlDisplayInterface) `onException` closeTransport transport
        case initialized of
          Left errorValue -> do
            closeTransport transport
            pure (Left (WaylandStartupConnectionError errorValue))
          Right (connection, display) -> do
            result <- restore (initializeSession connection display) `onException` closeConnection connection
            case result of
              Left errorValue -> closeConnection connection >> pure (Left errorValue)
              Right session -> pure (Right session)

-- | Close all connection resources. Repeated calls are safe.
closeWaylandSession :: WaylandSession -> IO ()
closeWaylandSession = closeConnection . waylandConnection

initializeSession ::
  Connection ->
  Object Wayland.WlDisplay ->
  IO (Either WaylandStartupError WaylandSession)
initializeSession connection display = do
  registerEventRoute connection Wayland.wlDisplayInterface display (wlDisplayEventRoute (\_ _ -> pure ())) >>= \case
    Left errorValue -> pure (Left (WaylandStartupConnectionError errorValue))
    Right () -> do
      catalogState <- newIORef (Right emptyGlobalCatalog)
      registryResult <-
        submitNewObjectRequestWithRoute
          connection
          Wayland.wlDisplayInterface
          display
          Wayland.wlRegistryInterface
          1
          (Wayland.encodeWlDisplayGetRegistry . objectId)
          (eventRoute Wayland.decodeWlRegistryEvent (updateCatalog catalogState))
      case registryResult of
        Left errorValue -> pure (Left (WaylandStartupConnectionError errorValue))
        Right registry -> do
          synchronized <- newIORef False
          callbackResult <-
            submitNewObjectRequestWithRoute
              connection
              Wayland.wlDisplayInterface
              display
              Wayland.wlCallbackInterface
              1
              (Wayland.encodeWlDisplaySync . objectId)
              (eventRoute Wayland.decodeWlCallbackEvent (\_ _ -> writeIORef synchronized True))
          case callbackResult of
            Left errorValue -> pure (Left (WaylandStartupConnectionError errorValue))
            Right _ -> do
              waitForRegistrySync connection catalogState synchronized >>= \case
                Left errorValue -> pure (Left errorValue)
                Right () ->
                  pure
                    ( Right
                        WaylandSession
                          { waylandConnection = connection
                          , waylandDisplay = display
                          , waylandRegistry = registry
                          , waylandCatalogState = catalogState
                          }
                    )

updateCatalog ::
  IORef (Either RegistryError GlobalCatalog) ->
  Object Wayland.WlRegistry ->
  Wayland.WlRegistryEvent fd ->
  IO ()
updateCatalog catalogState _ event =
  atomicModifyIORef' catalogState $ \catalog ->
    let updated = catalog >>= applyRegistryEvent event
     in (updated, ())

waitForRegistrySync ::
  Connection ->
  IORef (Either RegistryError GlobalCatalog) ->
  IORef Bool ->
  IO (Either WaylandStartupError ())
waitForRegistrySync connection catalogState synchronized =
  dispatchNextEvent connection >>= \case
    Left errorValue -> pure (Left (WaylandStartupConnectionError errorValue))
    Right DispatchFinished -> pure (Left WaylandRegistrySyncFinished)
    Right (EventDispatched _) -> do
      readIORef catalogState >>= \case
        Left errorValue -> pure (Left (WaylandRegistrySyncError errorValue))
        Right _ -> do
          reached <- readIORef synchronized
          if reached
            then pure (Right ())
            else waitForRegistrySync connection catalogState synchronized
