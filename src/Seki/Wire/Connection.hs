{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- | Serialized request submission and typed event dispatch for one Wayland
connection.

Object-table changes become visible only after their request has been sent or
their event has decoded completely. Transport and protocol failures are
terminal because the peer and client may otherwise disagree about stream or
object state.
-}
module Seki.Wire.Connection (
  Connection,
  ConnectionError (..),
  EventRoute,
  eventRoute,
  ServerObjectRoute,
  serverObjectRoute,
  eventRouteWithServerObjects,
  wlDisplayEventRoute,
  DispatchedEvent (..),
  DispatchResult (..),
  newConnection,
  closeConnection,
  registerEventRoute,
  dispatchNextEvent,
  submitRequest,
  submitNewObjectRequest,
  submitNewObjectRequestWithRoute,
  submitInheritedObjectRequest,
  submitInheritedObjectRequestWithRoute,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar, withMVar)
import Control.Exception (IOException, SomeException, displayException, fromException, mask, mask_, throwIO, try)
import Control.Monad (foldM, void)
import Data.Bifunctor (first)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Binding (EventDecoder, Interface (..), NewObject, RequestEncoder, newObjectId)
import Seki.Wire.Connection.Buffer (
  ArgumentPoll (..),
  FramePoll (..),
  ReceiveBuffer,
  ReceiveBufferError,
  appendReceivedChunk,
  closeReceiveBuffer,
  decodePendingArguments,
  defaultBufferLimits,
  markReceiveEof,
  newReceiveBuffer,
  pollFrame,
 )
import Seki.Wire.Connection.Event (
  EventPreparationError,
  PreparedEvent,
  finalizeDecodedEvent,
  finalizeDeleteIdEvent,
  prepareEvent,
  preparedEventArguments,
  preparedEventInterface,
  preparedEventName,
  preparedEventNewIds,
  preparedEventOpcode,
  preparedEventSender,
  preparedEventVersion,
 )
import Seki.Wire.Connection.ObjectTable (
  Object,
  ObjectLimit,
  ObjectTable,
  ObjectTableError,
  lookupObjectInfo,
  newObjectTable,
  objectId,
  objectInfoInterfaceName,
  registerServerObject,
  validateObject,
 )
import Seki.Wire.Connection.Request (
  PreparedRequest (..),
  RequestPreparationError,
  prepareInheritedObjectRequest,
  prepareNewObjectRequest,
  prepareRequest,
 )
import Seki.Wire.Transport (OwnedFd, Transport, TransportError, closeTransport, receiveChunk, sendMessage)
import Seki.Wire.Types (ObjectId, Opcode, RawFrame (..), objectIdValue)

data Connection = Connection
  { connectionTransport :: Transport
  , connectionState :: MVar ConnectionState
  , connectionReceiveBuffer :: ReceiveBuffer
  , connectionReceiveLock :: MVar ()
  }

data ConnectionData = ConnectionData
  { connectionObjects :: ObjectTable
  , connectionEventRoutes :: Map Word32 RegisteredEventRoute
  }

data ConnectionState
  = ConnectionOpen ConnectionData
  | ConnectionFailed ConnectionError
  | ConnectionFinished
  | ConnectionClosedState

-- | A generated decoder and typed handler for one protocol object.
data EventRoute interface
  = forall event.
    EventRoute
      (Opcode -> Maybe (EventDecoder OwnedFd event))
      (Interface interface -> PreparedEvent interface OwnedFd event -> event -> ObjectTable -> Either ConnectionError EventTransition)
      (Object interface -> event -> IO ())

-- | A typed route and creation callback for an object introduced by an event.
data ServerObjectRoute
  = forall interface.
    ServerObjectRoute
      (NewObject interface)
      (Interface interface)
      (EventRoute interface)
      (Object interface -> IO ())

data EventTransition = EventTransition
  { transitionObjects :: ObjectTable
  , transitionRoutes :: [RegisteredEventRoute]
  , transitionCallbacks :: [IO ()]
  }

data RegisteredEventRoute
  = forall interface event.
    RegisteredEventRoute
      (Interface interface)
      (Object interface)
      (Opcode -> Maybe (EventDecoder OwnedFd event))
      (Interface interface -> PreparedEvent interface OwnedFd event -> event -> ObjectTable -> Either ConnectionError EventTransition)
      (Object interface -> event -> IO ())

-- | Metadata for an event whose typed handler completed successfully.
data DispatchedEvent = DispatchedEvent
  { dispatchedEventSender :: ObjectId
  , dispatchedEventInterface :: Text
  , dispatchedEventName :: Text
  , dispatchedEventOpcode :: Opcode
  }
  deriving (Eq, Show)

data DispatchResult
  = EventDispatched DispatchedEvent
  | DispatchFinished
  deriving (Eq, Show)

-- | Failure to initialize, prepare, submit, receive, or dispatch.
data ConnectionError
  = ConnectionObjectTableError ObjectTableError
  | ConnectionRequestError RequestPreparationError
  | ConnectionEventError EventPreparationError
  | ConnectionReceiveBufferError ReceiveBufferError
  | ConnectionTransportError TransportError
  | ConnectionEventRouteMissing ObjectId
  | ConnectionEventRouteAlreadyRegistered ObjectId
  | ConnectionServerObjectRoutesMismatch [ObjectId] [ObjectId]
  | ConnectionDisplayError ObjectId Word32 (Maybe Text) Text
  | ConnectionUnexpectedError String
  | ConnectionClosed
  deriving (Eq, Show)

-- | Build a route whose only automatic lifecycle effect is sender destruction.
eventRoute ::
  (Opcode -> Maybe (EventDecoder OwnedFd event)) ->
  (Object interface -> event -> IO ()) ->
  EventRoute interface
eventRoute decoder =
  EventRoute decoder $ \interface prepared event table -> do
    requireServerObjectRoutes prepared event []
    nextTable <- first ConnectionEventError (finalizeDecodedEvent interface prepared table)
    pure (EventTransition nextTable [] [])

-- | Describe the route to install for one statically typed server-created ID.
serverObjectRoute ::
  NewObject interface ->
  Interface interface ->
  EventRoute interface ->
  (Object interface -> IO ()) ->
  ServerObjectRoute
serverObjectRoute = ServerObjectRoute

{- | Build a route that atomically registers and routes every @new_id@ carried
by an event. Creation callbacks run before the parent event handler.
-}
eventRouteWithServerObjects ::
  (Opcode -> Maybe (EventDecoder OwnedFd event)) ->
  (event -> [ServerObjectRoute]) ->
  (Object interface -> event -> IO ()) ->
  EventRoute interface
eventRouteWithServerObjects decoder selectRoutes =
  EventRoute decoder finalize
 where
  finalize interface prepared event table = do
    let selectedRoutes = selectRoutes event
    requireServerObjectRoutes prepared event selectedRoutes
    finalized <- first ConnectionEventError (finalizeDecodedEvent interface prepared table)
    (nextTable, routes, callbacks) <-
      foldM
        (registerServerObjectRoute (preparedEventVersion prepared))
        (finalized, [], [])
        selectedRoutes
    pure (EventTransition nextTable (reverse routes) (reverse callbacks))

{- | Build the core display route, including automatic @delete_id@ handling.

A @wl_display.error@ becomes a terminal 'ConnectionDisplayError'; ordinary
display events are delivered to the typed handler.
-}
wlDisplayEventRoute ::
  (Object Wayland.WlDisplay -> Wayland.WlDisplayEvent OwnedFd -> IO ()) ->
  EventRoute Wayland.WlDisplay
wlDisplayEventRoute = EventRoute Wayland.decodeWlDisplayEvent finalizeDisplay
 where
  finalizeDisplay interface prepared event table = case event of
    Wayland.WlDisplayDeleteId deletedId ->
      makeTransition <$> first ConnectionEventError (finalizeDeleteIdEvent interface prepared deletedId table)
    Wayland.WlDisplayError referencedObject code message ->
      Left
        ( ConnectionDisplayError
            referencedObject
            code
            (objectInfoInterfaceName <$> lookupObjectInfo referencedObject table)
            message
        )

  makeTransition nextTable = EventTransition nextTable [] []

{- | Attach connection state to an open transport and install object ID 1.

The caller retains ownership of the transport when initialization fails.
-}
newConnection ::
  Transport ->
  ObjectLimit ->
  Interface display ->
  IO (Either ConnectionError (Connection, Object display))
newConnection transport limit displayInterface =
  case newObjectTable limit displayInterface of
    Left errorValue -> pure (Left (ConnectionObjectTableError errorValue))
    Right (table, display) -> do
      state <- newMVar (ConnectionOpen (ConnectionData table Map.empty))
      receiveBuffer <- newReceiveBuffer defaultBufferLimits
      receiveLock <- newMVar ()
      pure (Right (Connection transport state receiveBuffer receiveLock, display))

-- | Close the transport and receive buffer, rejecting future operations.
closeConnection :: Connection -> IO ()
closeConnection connection =
  mask_ $ do
    modifyMVar (connectionState connection) $ \case
      ConnectionClosedState -> pure (ConnectionClosedState, ())
      _ -> do
        closeConnectionResources connection
        pure (ConnectionClosedState, ())

-- | Register exactly one typed event route for an active object.
registerEventRoute ::
  Connection ->
  Interface interface ->
  Object interface ->
  EventRoute interface ->
  IO (Either ConnectionError ())
registerEventRoute connection interface object (EventRoute decoder finalize handler) =
  modifyMVar (connectionState connection) $ \case
    ConnectionClosedState -> pure (ConnectionClosedState, Left ConnectionClosed)
    ConnectionFinished -> pure (ConnectionFinished, Left ConnectionClosed)
    failed@(ConnectionFailed errorValue) -> pure (failed, Left errorValue)
    open@(ConnectionOpen connectionData) ->
      case validateObject interface object (connectionObjects connectionData) of
        Left errorValue -> pure (open, Left (ConnectionObjectTableError errorValue))
        Right _
          | Map.member rawId (connectionEventRoutes connectionData) ->
              pure (open, Left (ConnectionEventRouteAlreadyRegistered (objectId object)))
          | otherwise ->
              let registered = RegisteredEventRoute interface object decoder finalize handler
                  nextData =
                    connectionData
                      { connectionEventRoutes = Map.insert rawId registered (connectionEventRoutes connectionData)
                      }
               in pure (ConnectionOpen nextData, Right ())
 where
  rawId = objectIdValue (objectId object)

registerRoute :: Interface interface -> Object interface -> EventRoute interface -> RegisteredEventRoute
registerRoute interface object (EventRoute decoder finalize handler) =
  RegisteredEventRoute interface object decoder finalize handler

installRoute :: Map Word32 RegisteredEventRoute -> RegisteredEventRoute -> Map Word32 RegisteredEventRoute
installRoute routes registered@(RegisteredEventRoute _ object _ _ _) =
  Map.insert (objectIdValue (objectId object)) registered routes

requireServerObjectRoutes ::
  PreparedEvent interface fd event ->
  event ->
  [ServerObjectRoute] ->
  Either ConnectionError ()
requireServerObjectRoutes prepared event routes
  | expectedIds == selectedIds = Right ()
  | otherwise = Left (ConnectionServerObjectRoutesMismatch expectedIds selectedIds)
 where
  expectedIds = normalizeObjectIds (preparedEventNewIds prepared event)
  selectedIds = normalizeObjectIds (map serverObjectRouteId routes)

normalizeObjectIds :: [ObjectId] -> [ObjectId]
normalizeObjectIds = sortOn objectIdValue

serverObjectRouteId :: ServerObjectRoute -> ObjectId
serverObjectRouteId (ServerObjectRoute newObject _ _ _) = newObjectId newObject

registerServerObjectRoute ::
  Word32 ->
  (ObjectTable, [RegisteredEventRoute], [IO ()]) ->
  ServerObjectRoute ->
  Either ConnectionError (ObjectTable, [RegisteredEventRoute], [IO ()])
registerServerObjectRoute parentVersion (table, routes, callbacks) (ServerObjectRoute newObject interface route callback) = do
  let childVersion = min parentVersion (interfaceMaximumVersion interface)
  (nextTable, object) <-
    first ConnectionObjectTableError $
      registerServerObject (newObjectId newObject) interface childVersion table
  pure
    ( nextTable
    , registerRoute interface object route : routes
    , callback object : callbacks
    )

-- | Receive, decode, finalize, and handle the next event in stream order.
dispatchNextEvent :: Connection -> IO (Either ConnectionError DispatchResult)
dispatchNextEvent connection =
  withMVar (connectionReceiveLock connection) $ \() -> do
    readMVar (connectionState connection) >>= \case
      ConnectionClosedState -> pure (Left ConnectionClosed)
      ConnectionFinished -> pure (Right DispatchFinished)
      ConnectionFailed errorValue -> pure (Left errorValue)
      ConnectionOpen _ -> do
        receiveNextFrame connection >>= \case
          Left errorValue -> failConnection connection errorValue
          Right Nothing -> finishConnection connection
          Right (Just frame) -> dispatchFrame connection frame

-- | Prepare and submit one request while holding the connection ordering lock.
submitRequest ::
  Connection ->
  Interface interface ->
  Object interface ->
  RequestEncoder OwnedFd ->
  IO (Either ConnectionError ())
submitRequest connection interface sender request =
  submitPrepared connection $ \table -> do
    (nextTable, prepared) <- prepareRequest interface sender request table
    pure (nextTable, prepared, [], ())

{- | Allocate a client object and publish it only after its creating request
has been sent successfully.
-}
submitNewObjectRequest ::
  Connection ->
  Interface parent ->
  Object parent ->
  Interface child ->
  Word32 ->
  (Object child -> RequestEncoder OwnedFd) ->
  IO (Either ConnectionError (Object child))
submitNewObjectRequest connection parentInterface parent childInterface childVersion makeRequest =
  submitNewObjectRequestInternal connection parentInterface parent childInterface childVersion makeRequest Nothing

{- | Allocate a client object and install its event route in the same
successful-send transaction.
-}
submitNewObjectRequestWithRoute ::
  Connection ->
  Interface parent ->
  Object parent ->
  Interface child ->
  Word32 ->
  (Object child -> RequestEncoder OwnedFd) ->
  EventRoute child ->
  IO (Either ConnectionError (Object child))
submitNewObjectRequestWithRoute connection parentInterface parent childInterface childVersion makeRequest route =
  submitNewObjectRequestInternal connection parentInterface parent childInterface childVersion makeRequest (Just route)

submitNewObjectRequestInternal ::
  Connection ->
  Interface parent ->
  Object parent ->
  Interface child ->
  Word32 ->
  (Object child -> RequestEncoder OwnedFd) ->
  Maybe (EventRoute child) ->
  IO (Either ConnectionError (Object child))
submitNewObjectRequestInternal connection parentInterface parent childInterface childVersion makeRequest route =
  submitPrepared connection $ \table -> do
    (nextTable, child, prepared) <-
      prepareNewObjectRequest parentInterface parent childInterface childVersion makeRequest table
    pure (nextTable, prepared, maybe [] (pure . registerRoute childInterface child) route, child)

-- | Submit an object-creating request with the parent-inherited child version.
submitInheritedObjectRequest ::
  Connection ->
  Interface parent ->
  Object parent ->
  Interface child ->
  (Object child -> RequestEncoder OwnedFd) ->
  IO (Either ConnectionError (Object child))
submitInheritedObjectRequest connection parentInterface parent childInterface makeRequest =
  submitInheritedObjectRequestInternal connection parentInterface parent childInterface makeRequest Nothing

{- | Submit an inherited-version object-creating request and atomically install
the child's event route.
-}
submitInheritedObjectRequestWithRoute ::
  Connection ->
  Interface parent ->
  Object parent ->
  Interface child ->
  (Object child -> RequestEncoder OwnedFd) ->
  EventRoute child ->
  IO (Either ConnectionError (Object child))
submitInheritedObjectRequestWithRoute connection parentInterface parent childInterface makeRequest route =
  submitInheritedObjectRequestInternal connection parentInterface parent childInterface makeRequest (Just route)

submitInheritedObjectRequestInternal ::
  Connection ->
  Interface parent ->
  Object parent ->
  Interface child ->
  (Object child -> RequestEncoder OwnedFd) ->
  Maybe (EventRoute child) ->
  IO (Either ConnectionError (Object child))
submitInheritedObjectRequestInternal connection parentInterface parent childInterface makeRequest route =
  submitPrepared connection $ \table -> do
    (nextTable, child, prepared) <-
      prepareInheritedObjectRequest parentInterface parent childInterface makeRequest table
    pure (nextTable, prepared, maybe [] (pure . registerRoute childInterface child) route, child)

submitPrepared ::
  Connection ->
  (ObjectTable -> Either RequestPreparationError (ObjectTable, PreparedRequest OwnedFd, [RegisteredEventRoute], value)) ->
  IO (Either ConnectionError value)
submitPrepared connection prepare =
  mask $ \restore -> do
    outcome <-
      modifyMVar (connectionState connection) $ \case
        ConnectionClosedState -> pure (ConnectionClosedState, SubmissionReturned (Left ConnectionClosed))
        ConnectionFinished -> pure (ConnectionFinished, SubmissionReturned (Left ConnectionClosed))
        failed@(ConnectionFailed errorValue) -> pure (failed, SubmissionReturned (Left errorValue))
        open@(ConnectionOpen connectionData) -> case prepare (connectionObjects connectionData) of
          Left errorValue ->
            pure (open, SubmissionReturned (Left (ConnectionRequestError errorValue)))
          Right (nextTable, prepared, newRoutes, value) -> do
            attempted <-
              try @SomeException
                ( restore
                    ( sendMessage
                        (connectionTransport connection)
                        (preparedRequestBytes prepared)
                        (preparedRequestFds prepared)
                    )
                )
            case attempted of
              Left exception -> do
                closeConnectionResources connection
                let terminalError = ConnectionUnexpectedError (displayException exception)
                pure (ConnectionFailed terminalError, SubmissionRaised exception)
              Right (Left errorValue) -> do
                closeConnectionResources connection
                let terminalError = ConnectionTransportError errorValue
                pure (ConnectionFailed terminalError, SubmissionReturned (Left terminalError))
              Right (Right ()) ->
                let nextData =
                      connectionData
                        { connectionObjects = nextTable
                        , connectionEventRoutes = foldl installRoute (pruneRoutes nextTable (connectionEventRoutes connectionData)) newRoutes
                        }
                 in pure (ConnectionOpen nextData, SubmissionReturned (Right value))
    case outcome of
      SubmissionReturned result -> pure result
      SubmissionRaised exception
        | Just (_ :: IOException) <- fromException exception ->
            pure (Left (ConnectionUnexpectedError (displayException exception)))
        | otherwise -> throwIO exception

receiveNextFrame :: Connection -> IO (Either ConnectionError (Maybe RawFrame))
receiveNextFrame connection =
  pollFrame (connectionReceiveBuffer connection) >>= \case
    Left errorValue -> pure (Left (ConnectionReceiveBufferError errorValue))
    Right (FrameReady frame) -> pure (Right (Just frame))
    Right ReceiveStreamFinished -> pure (Right Nothing)
    Right (AwaitingFrameBytes _) ->
      receiveIntoBuffer connection >>= \case
        Left errorValue -> pure (Left errorValue)
        Right () -> receiveNextFrame connection

receiveIntoBuffer :: Connection -> IO (Either ConnectionError ())
receiveIntoBuffer connection =
  receiveChunk (connectionTransport connection) receiveChunkSize >>= \case
    Left errorValue -> pure (Left (ConnectionTransportError errorValue))
    Right Nothing ->
      firstConnectionBufferError <$> markReceiveEof (connectionReceiveBuffer connection)
    Right (Just chunk) ->
      firstConnectionBufferError <$> appendReceivedChunk (connectionReceiveBuffer connection) chunk

decodePreparedEvent ::
  Connection ->
  PreparedEvent interface OwnedFd event ->
  IO (Either ConnectionError event)
decodePreparedEvent connection prepared =
  decodePendingArguments (connectionReceiveBuffer connection) (preparedEventArguments prepared) >>= \case
    Left errorValue -> pure (Left (ConnectionReceiveBufferError errorValue))
    Right (ArgumentsReady event) -> pure (Right event)
    Right (AwaitingArgumentFds _) ->
      receiveIntoBuffer connection >>= \case
        Left errorValue -> pure (Left errorValue)
        Right () -> decodePreparedEvent connection prepared

dispatchFrame :: Connection -> RawFrame -> IO (Either ConnectionError DispatchResult)
dispatchFrame connection frame =
  mask $ \restore -> do
    outcome <-
      modifyMVar (connectionState connection) $ \case
        ConnectionClosedState -> pure (ConnectionClosedState, DispatchReturned (Left ConnectionClosed))
        ConnectionFinished -> pure (ConnectionFinished, DispatchReturned (Right DispatchFinished))
        failed@(ConnectionFailed errorValue) -> pure (failed, DispatchReturned (Left errorValue))
        ConnectionOpen connectionData ->
          case Map.lookup (objectIdValue (rawFrameSender frame)) (connectionEventRoutes connectionData) of
            Nothing ->
              let errorValue = ConnectionEventRouteMissing (rawFrameSender frame)
               in pure (ConnectionFailed errorValue, DispatchFailed errorValue)
            Just (RegisteredEventRoute interface object decoder finalize handler) ->
              case prepareEvent interface object decoder frame (connectionObjects connectionData) of
                Left eventError ->
                  let errorValue = ConnectionEventError eventError
                   in pure (ConnectionFailed errorValue, DispatchFailed errorValue)
                Right prepared -> do
                  decoded <- try @SomeException (restore (decodePreparedEvent connection prepared))
                  case decoded of
                    Left exception -> do
                      let errorValue = ConnectionUnexpectedError (displayException exception)
                      pure (ConnectionFailed errorValue, DispatchRaised exception)
                    Right (Left errorValue) ->
                      pure (ConnectionFailed errorValue, DispatchFailed errorValue)
                    Right (Right event) ->
                      case finalize interface prepared event (connectionObjects connectionData) of
                        Left errorValue ->
                          pure (ConnectionFailed errorValue, DispatchFailed errorValue)
                        Right transition ->
                          let result =
                                EventDispatched
                                  DispatchedEvent
                                    { dispatchedEventSender = preparedEventSender prepared
                                    , dispatchedEventInterface = preparedEventInterface prepared
                                    , dispatchedEventName = preparedEventName prepared
                                    , dispatchedEventOpcode = preparedEventOpcode prepared
                                    }
                              nextData =
                                connectionData
                                  { connectionObjects = transitionObjects transition
                                  , connectionEventRoutes =
                                      foldl
                                        installRoute
                                        (pruneRoutes (transitionObjects transition) (connectionEventRoutes connectionData))
                                        (transitionRoutes transition)
                                  }
                              invocation =
                                EventInvocation
                                  result
                                  (sequence_ (transitionCallbacks transition) >> handler object event)
                           in pure (ConnectionOpen nextData, DispatchInvoke invocation)
    case outcome of
      DispatchReturned result -> pure result
      DispatchFailed errorValue -> closeConnectionResources connection >> pure (Left errorValue)
      DispatchRaised exception -> do
        closeConnectionResources connection
        rethrowOrReturn exception
      DispatchInvoke invocation -> runEventInvocation connection invocation

data EventInvocation = EventInvocation DispatchResult (IO ())

runEventInvocation :: Connection -> EventInvocation -> IO (Either ConnectionError DispatchResult)
runEventInvocation connection (EventInvocation result action) = do
  attempted <- try @SomeException action
  case attempted of
    Right () -> pure (Right result)
    Left exception -> do
      let errorValue = ConnectionUnexpectedError (displayException exception)
      markConnectionFailed connection errorValue
      closeConnectionResources connection
      rethrowOrReturn exception

data SubmissionOutcome value
  = SubmissionReturned (Either ConnectionError value)
  | SubmissionRaised SomeException

data DispatchOutcome
  = DispatchReturned (Either ConnectionError DispatchResult)
  | DispatchFailed ConnectionError
  | DispatchRaised SomeException
  | DispatchInvoke EventInvocation

finishConnection :: Connection -> IO (Either ConnectionError DispatchResult)
finishConnection connection = do
  result <-
    modifyMVar (connectionState connection) $ \case
      ConnectionOpen _ -> pure (ConnectionFinished, Right DispatchFinished)
      ConnectionFinished -> pure (ConnectionFinished, Right DispatchFinished)
      failed@(ConnectionFailed errorValue) -> pure (failed, Left errorValue)
      ConnectionClosedState -> pure (ConnectionClosedState, Left ConnectionClosed)
  closeConnectionResources connection
  pure result

failConnection :: Connection -> ConnectionError -> IO (Either ConnectionError value)
failConnection connection errorValue = do
  markConnectionFailed connection errorValue
  closeConnectionResources connection
  pure (Left errorValue)

markConnectionFailed :: Connection -> ConnectionError -> IO ()
markConnectionFailed connection errorValue =
  modifyMVar (connectionState connection) $ \case
    ConnectionOpen _ -> pure (ConnectionFailed errorValue, ())
    state -> pure (state, ())

pruneRoutes :: ObjectTable -> Map Word32 RegisteredEventRoute -> Map Word32 RegisteredEventRoute
pruneRoutes table = Map.filter routeIsActive
 where
  routeIsActive (RegisteredEventRoute interface object _ _ _) =
    case validateObject interface object table of
      Left _ -> False
      Right _ -> True

firstConnectionBufferError :: Either ReceiveBufferError value -> Either ConnectionError value
firstConnectionBufferError = either (Left . ConnectionReceiveBufferError) Right

rethrowOrReturn :: SomeException -> IO (Either ConnectionError value)
rethrowOrReturn exception
  | Just (_ :: IOException) <- fromException exception =
      pure (Left (ConnectionUnexpectedError (displayException exception)))
  | otherwise = throwIO exception

closeConnectionResources :: Connection -> IO ()
closeConnectionResources connection = do
  closeReceiveBuffer (connectionReceiveBuffer connection)
  closeTransportQuietly (connectionTransport connection)

closeTransportQuietly :: Transport -> IO ()
closeTransportQuietly transport = void (try @IOException (closeTransport transport))

receiveChunkSize :: Int
receiveChunkSize = 64 * 1024
