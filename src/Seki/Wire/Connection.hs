{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- | Serialized request submission and object state for one Wayland connection.

The connection publishes object-table changes only after the corresponding
frame has been sent completely. A transport failure is terminal because the
peer may have observed an arbitrary prefix of the frame.
-}
module Seki.Wire.Connection (
  Connection,
  ConnectionError (..),
  newConnection,
  closeConnection,
  submitRequest,
  submitNewObjectRequest,
  submitInheritedObjectRequest,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (IOException, SomeException, displayException, fromException, mask, mask_, throwIO, try)
import Control.Monad (void)
import Data.Word (Word32)
import Seki.Wire.Binding (Interface, RequestEncoder)
import Seki.Wire.Connection.ObjectTable (Object, ObjectLimit, ObjectTable, ObjectTableError, newObjectTable)
import Seki.Wire.Connection.Request (
  PreparedRequest (..),
  RequestPreparationError,
  prepareInheritedObjectRequest,
  prepareNewObjectRequest,
  prepareRequest,
 )
import Seki.Wire.Transport (OwnedFd, Transport, TransportError, closeTransport, sendMessage)

data Connection = Connection
  { connectionTransport :: Transport
  , connectionState :: MVar ConnectionState
  }

data ConnectionState
  = ConnectionOpen ObjectTable
  | ConnectionFailed ConnectionError
  | ConnectionClosedState

-- | Failure to initialize, prepare, or submit a request.
data ConnectionError
  = ConnectionObjectTableError ObjectTableError
  | ConnectionRequestError RequestPreparationError
  | ConnectionTransportError TransportError
  | ConnectionUnexpectedError String
  | ConnectionClosed
  deriving (Eq, Show)

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
      state <- newMVar (ConnectionOpen table)
      pure (Right (Connection transport state, display))

-- | Close the transport and reject future requests. This is idempotent.
closeConnection :: Connection -> IO ()
closeConnection connection =
  mask_ $ do
    modifyMVar (connectionState connection) $ \case
      ConnectionOpen _ -> do
        closeTransportQuietly (connectionTransport connection)
        pure (ConnectionClosedState, ())
      ConnectionFailed _ -> do
        closeTransportQuietly (connectionTransport connection)
        pure (ConnectionClosedState, ())
      ConnectionClosedState -> pure (ConnectionClosedState, ())

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
    pure (nextTable, prepared, ())

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
  submitPrepared connection $ \table -> do
    (nextTable, child, prepared) <-
      prepareNewObjectRequest parentInterface parent childInterface childVersion makeRequest table
    pure (nextTable, prepared, child)

-- | Submit an object-creating request with the parent-inherited child version.
submitInheritedObjectRequest ::
  Connection ->
  Interface parent ->
  Object parent ->
  Interface child ->
  (Object child -> RequestEncoder OwnedFd) ->
  IO (Either ConnectionError (Object child))
submitInheritedObjectRequest connection parentInterface parent childInterface makeRequest =
  submitPrepared connection $ \table -> do
    (nextTable, child, prepared) <-
      prepareInheritedObjectRequest parentInterface parent childInterface makeRequest table
    pure (nextTable, prepared, child)

submitPrepared ::
  Connection ->
  (ObjectTable -> Either RequestPreparationError (ObjectTable, PreparedRequest OwnedFd, value)) ->
  IO (Either ConnectionError value)
submitPrepared connection prepare =
  mask $ \restore -> do
    outcome <-
      modifyMVar (connectionState connection) $ \case
        ConnectionClosedState -> pure (ConnectionClosedState, SubmissionReturned (Left ConnectionClosed))
        failed@(ConnectionFailed errorValue) -> pure (failed, SubmissionReturned (Left errorValue))
        open@(ConnectionOpen table) -> case prepare table of
          Left errorValue ->
            pure (open, SubmissionReturned (Left (ConnectionRequestError errorValue)))
          Right (nextTable, prepared, value) -> do
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
                closeTransportQuietly (connectionTransport connection)
                let terminalError = ConnectionUnexpectedError (displayException exception)
                pure (ConnectionFailed terminalError, SubmissionRaised exception)
              Right (Left errorValue) -> do
                closeTransportQuietly (connectionTransport connection)
                let terminalError = ConnectionTransportError errorValue
                pure (ConnectionFailed terminalError, SubmissionReturned (Left terminalError))
              Right (Right ()) ->
                pure (ConnectionOpen nextTable, SubmissionReturned (Right value))
    case outcome of
      SubmissionReturned result -> pure result
      SubmissionRaised exception
        | Just (_ :: IOException) <- fromException exception ->
            pure (Left (ConnectionUnexpectedError (displayException exception)))
        | otherwise -> throwIO exception

data SubmissionOutcome value
  = SubmissionReturned (Either ConnectionError value)
  | SubmissionRaised SomeException

closeTransportQuietly :: Transport -> IO ()
closeTransportQuietly transport = void (try @IOException (closeTransport transport))
