{-# LANGUAGE LambdaCase #-}

{- | Foundations for the Wayland Unix transport.

This module resolves the standard Wayland connection environment, owns raw file
descriptors explicitly, and transfers bytes and ancillary descriptors over a
non-blocking Unix stream.
-}
module Seki.Wire.Transport (
  ConnectionTarget (..),
  ConnectionDiscoveryError (..),
  maxUnixSocketPathBytes,
  discoverConnectionTarget,
  resolveConnectionTarget,
  OwnedFd,
  OwnedFdError (..),
  adoptFd,
  withOwnedFd,
  releaseOwnedFd,
  closeOwnedFd,
  Transport,
  TransportOperation (..),
  AncillaryError (..),
  TransportError (..),
  ReceivedChunk (..),
  maxDescriptorsPerMessage,
  openTransport,
  closeTransport,
  sendMessage,
  receiveChunk,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (IOException, mask, mask_, onException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Foreign.C.Error (Errno (..), eAGAIN, eCONNRESET, eINTR, ePIPE, eWOULDBLOCK)
import Foreign.C.Types (CInt)
import GHC.Conc (threadWaitRead, threadWaitWrite)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  Socket,
  SocketType (Stream),
  close,
  connect,
  defaultProtocol,
  getPeerName,
  getSocketType,
  mkSocket,
  setCloseOnExecIfNeeded,
  setNonBlockIfNeeded,
  socket,
  withFdSocket,
 )
import Seki.Wire.Transport.Driver (
  ReceiveAttempt (..),
  SendAttempt (..),
  SendDriverError (..),
  driveReceive,
  driveSend,
 )
import Seki.Wire.Transport.Syscall (
  RawReceiveError (..),
  RawReceiveResult (..),
  receiveRaw,
  sendRaw,
 )
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, (</>))
import System.Posix.IO (FdOption (CloseOnExec), closeFd, setFdOption)
import System.Posix.Types (Fd (..))

-- | The source from which a Wayland connection should be opened.
data ConnectionTarget
  = InheritedSocket Fd
  | UnixSocketPath FilePath
  deriving (Eq, Show)

-- | A malformed or incomplete Wayland connection environment.
data ConnectionDiscoveryError
  = WaylandSocketEmpty
  | WaylandSocketNotDecimal String
  | WaylandSocketOutOfRange Integer
  | WaylandDisplayEmpty
  | WaylandDisplayContainsNul
  | XdgRuntimeDirMissing
  | XdgRuntimeDirNotAbsolute FilePath
  | UnixSocketPathContainsNul
  | UnixSocketPathTooLong Int
  deriving (Eq, Show)

-- | Maximum pathname bytes in Linux's @sockaddr_un.sun_path@, excluding NUL.
maxUnixSocketPathBytes :: Int
maxUnixSocketPathBytes = 107

-- | Read and resolve the standard Wayland connection environment variables.
discoverConnectionTarget :: IO (Either ConnectionDiscoveryError ConnectionTarget)
discoverConnectionTarget = do
  waylandSocket <- lookupEnv "WAYLAND_SOCKET"
  waylandDisplay <- lookupEnv "WAYLAND_DISPLAY"
  runtimeDirectory <- lookupEnv "XDG_RUNTIME_DIR"
  pure (resolveConnectionTarget waylandSocket waylandDisplay runtimeDirectory)

{- | Resolve connection variables using libwayland's precedence rules.

An inherited @WAYLAND_SOCKET@ takes precedence. Otherwise an absolute
@WAYLAND_DISPLAY@ is used directly, while a display name is resolved beneath
@XDG_RUNTIME_DIR@. An unset display defaults to @wayland-0@.
-}
resolveConnectionTarget :: Maybe String -> Maybe FilePath -> Maybe FilePath -> Either ConnectionDiscoveryError ConnectionTarget
resolveConnectionTarget (Just socketValue) _ _ = InheritedSocket <$> parseInheritedFd socketValue
resolveConnectionTarget Nothing displayValue runtimeDirectory = do
  display <- validateDisplay (fromMaybe "wayland-0" displayValue)
  if isAbsolute display
    then UnixSocketPath <$> validateSocketPath display
    else do
      runtime <- validateRuntimeDirectory runtimeDirectory
      UnixSocketPath <$> validateSocketPath (runtime </> display)

parseInheritedFd :: String -> Either ConnectionDiscoveryError Fd
parseInheritedFd value
  | null value = Left WaylandSocketEmpty
  | not (all isDigit value) = Left (WaylandSocketNotDecimal value)
  | parsed > toInteger (maxBound :: CInt) = Left (WaylandSocketOutOfRange parsed)
  | otherwise = Right (Fd (fromInteger parsed))
 where
  parsed = read value

validateDisplay :: FilePath -> Either ConnectionDiscoveryError FilePath
validateDisplay display
  | null display = Left WaylandDisplayEmpty
  | '\0' `elem` display = Left WaylandDisplayContainsNul
  | otherwise = Right display

validateRuntimeDirectory :: Maybe FilePath -> Either ConnectionDiscoveryError FilePath
validateRuntimeDirectory Nothing = Left XdgRuntimeDirMissing
validateRuntimeDirectory (Just runtime)
  | null runtime = Left XdgRuntimeDirMissing
  | not (isAbsolute runtime) = Left (XdgRuntimeDirNotAbsolute runtime)
  | otherwise = Right runtime

validateSocketPath :: FilePath -> Either ConnectionDiscoveryError FilePath
validateSocketPath path
  | '\0' `elem` path = Left UnixSocketPathContainsNul
  | pathBytes > maxUnixSocketPathBytes = Left (UnixSocketPathTooLong pathBytes)
  | otherwise = Right path
 where
  pathBytes = ByteString.length (TextEncoding.encodeUtf8 (Text.pack path))

-- | A descriptor that is either owned exactly once or has been released.
newtype OwnedFd = OwnedFd
  { ownedFdState :: MVar (Maybe Fd)
  }

-- | An operation attempted after descriptor ownership was transferred or closed.
data OwnedFdError = OwnedFdAlreadyReleased
  deriving (Eq, Show)

{- | Take ownership of a descriptor and mark it close-on-exec.

If setting close-on-exec fails, the descriptor is closed before the exception
is rethrown.
-}
adoptFd :: Fd -> IO OwnedFd
adoptFd descriptor =
  mask $ \restore -> do
    restore (setFdOption descriptor CloseOnExec True) `onException` closeFd descriptor
    OwnedFd <$> newMVar (Just descriptor)

{- | Borrow the descriptor for an action without transferring ownership.

The borrow holds the ownership lock, so a concurrent close or release waits for
the action. The action must not close the borrowed descriptor itself.
-}
withOwnedFd :: OwnedFd -> (Fd -> IO value) -> IO (Either OwnedFdError value)
withOwnedFd owned action =
  withMVar (ownedFdState owned) $ \case
    Nothing -> pure (Left OwnedFdAlreadyReleased)
    Just descriptor -> Right <$> action descriptor

-- | Transfer ownership to the caller. A second transfer fails.
releaseOwnedFd :: OwnedFd -> IO (Either OwnedFdError Fd)
releaseOwnedFd owned =
  modifyMVar (ownedFdState owned) $ \case
    Nothing -> pure (Nothing, Left OwnedFdAlreadyReleased)
    Just descriptor -> pure (Nothing, Right descriptor)

{- | Close an owned descriptor.

Returns 'False' when ownership had already been transferred or closed. The
state is cleared before calling @close(2)@ so a close error cannot later cause a
reused descriptor number to be closed accidentally.
-}
closeOwnedFd :: OwnedFd -> IO Bool
closeOwnedFd owned = mask_ $ do
  descriptor <-
    modifyMVar (ownedFdState owned) $ \state ->
      pure (Nothing, state)
  case descriptor of
    Nothing -> pure False
    Just value -> closeFd value >> pure True

-- | A connected, non-blocking Unix stream used for Wayland traffic.
data Transport = Transport Socket (MVar ())

-- | The syscall that produced a transport error.
data TransportOperation
  = SendOperation
  | ReceiveOperation
  deriving (Eq, Show)

-- | A rejected ancillary-data layout or descriptor setup failure.
data AncillaryError
  = ControlTruncated
  | MalformedControl
  | UnsupportedControl
  | TooManyReceivedFds
  | CloseOnExecFailed CInt
  | DataTruncated
  | DescriptorsWithoutBytes
  | UnknownReceiveStatus CInt
  deriving (Eq, Show)

-- | A connection, ownership, or ancillary-data failure.
data TransportError
  = TransportOpenFailed String
  | TransportSocketIsNotUnixStream
  | TransportDescriptorAlreadyReleased
  | TransportDescriptorLimitExceeded Int
  | TransportEmptyPayloadWithDescriptors
  | TransportInvalidReceiveCapacity Int
  | TransportNoProgress
  | TransportInvalidSendProgress Int Int
  | TransportPeerClosed
  | TransportSystemError TransportOperation CInt
  | TransportReceiveRejected AncillaryError
  deriving (Eq, Show)

-- | One stream read and the descriptors attached to it.
data ReceivedChunk = ReceivedChunk
  { receivedBytes :: ByteString
  , receivedFds :: [OwnedFd]
  }

-- | Per-message and per-read descriptor limit enforced by the transport.
maxDescriptorsPerMessage :: Int
maxDescriptorsPerMessage = 64

-- | Open and validate a connected non-blocking Unix stream.
openTransport :: ConnectionTarget -> IO (Either TransportError Transport)
openTransport target =
  mask $ \restore -> do
    opened <- try @IOException (openTarget restore target)
    case opened of
      Left exception -> pure (Left (TransportOpenFailed (show exception)))
      Right connection -> validateTransport restore connection

openTarget :: (forall value. IO value -> IO value) -> ConnectionTarget -> IO Socket
openTarget restore target = case target of
  InheritedSocket (Fd descriptor) -> do
    connection <- mkSocket descriptor
    restore
      ( do
          setCloseOnExecIfNeeded descriptor
          setNonBlockIfNeeded descriptor
      )
      `onException` close connection
    pure connection
  UnixSocketPath path -> do
    connection <- socket AF_UNIX Stream defaultProtocol
    restore (withFdSocket connection setCloseOnExecIfNeeded >> connect connection (SockAddrUnix path)) `onException` close connection
    pure connection

validateTransport :: (forall value. IO value -> IO value) -> Socket -> IO (Either TransportError Transport)
validateTransport restore connection = do
  validation <-
    try @IOException . restore $ do
      socketType <- getSocketType connection
      peer <- getPeerName connection
      pure (socketType, peer)
  case validation of
    Left exception -> do
      close connection
      pure (Left (TransportOpenFailed (show exception)))
    Right (Stream, SockAddrUnix _) -> do
      writeLock <- newMVar ()
      pure (Right (Transport connection writeLock))
    Right _ -> do
      close connection
      pure (Left TransportSocketIsNotUnixStream)

-- | Close the transport socket. Network's socket close is idempotent.
closeTransport :: Transport -> IO ()
closeTransport (Transport connection _) = close connection

{- | Send an entire byte message and its descriptors in order.

Descriptor ownership transfers to the transport immediately. Descriptors are
attached until the first successful byte write, then closed locally; any
remaining bytes are sent without ancillary data.
-}
sendMessage :: Transport -> ByteString -> [OwnedFd] -> IO (Either TransportError ())
sendMessage transport@(Transport _ writeLock) bytes ownedDescriptors =
  withMVar writeLock $ \() ->
    mask_ $ do
      let validation = validateOutgoing bytes ownedDescriptors
      case validation of
        Left errorValue -> do
          closeOwnedDescriptors ownedDescriptors
          pure (Left errorValue)
        Right () -> do
          claimed <- claimDescriptors ownedDescriptors
          case claimed of
            Left errorValue -> pure (Left errorValue)
            Right descriptors -> do
              result <-
                driveSend
                  bytes
                  descriptors
                  (sendAttempt transport)
                  (waitForWrite transport)
                  closeRawDescriptors
              pure (firstSendError result)

validateOutgoing :: ByteString -> [OwnedFd] -> Either TransportError ()
validateOutgoing bytes descriptors
  | ByteString.null bytes && not (null descriptors) = Left TransportEmptyPayloadWithDescriptors
  | descriptorCount > maxDescriptorsPerMessage = Left (TransportDescriptorLimitExceeded descriptorCount)
  | otherwise = Right ()
 where
  descriptorCount = length descriptors

claimDescriptors :: [OwnedFd] -> IO (Either TransportError [Fd])
claimDescriptors = go []
 where
  go claimed [] = pure (Right (reverse claimed))
  go claimed remaining@(owned : rest) = do
    releaseOwnedFd owned >>= \case
      Left OwnedFdAlreadyReleased -> do
        closeRawDescriptors claimed
        closeOwnedDescriptors remaining
        pure (Left TransportDescriptorAlreadyReleased)
      Right descriptor -> go (descriptor : claimed) rest

sendAttempt :: Transport -> ByteString -> [Fd] -> IO (SendAttempt CInt)
sendAttempt transport bytes descriptors = do
  result <- withTransportFd transport $ \descriptor -> sendRaw descriptor bytes descriptors
  case result of
    Left errorNumber
      | errorNumber == errnoCode eINTR -> pure SendInterrupted
      | wouldBlock errorNumber -> pure SendWouldBlock
      | otherwise -> pure (SendFailed errorNumber)
    Right byteCount -> pure (SendProgress byteCount)

firstSendError :: Either (SendDriverError CInt) () -> Either TransportError ()
firstSendError result = case result of
  Left SendDescriptorsWithoutBytes -> Left TransportEmptyPayloadWithDescriptors
  Left SendNoProgress -> Left TransportNoProgress
  Left (SendInvalidProgress progress available) -> Left (TransportInvalidSendProgress progress available)
  Left (SendDriverFailed errorNumber)
    | errorNumber == errnoCode ePIPE -> Left TransportPeerClosed
    | otherwise -> Left (TransportSystemError SendOperation errorNumber)
  Right () -> Right ()

-- | Receive one stream chunk, waiting through interruption and backpressure.
receiveChunk :: Transport -> Int -> IO (Either TransportError (Maybe ReceivedChunk))
receiveChunk transport byteCapacity
  | byteCapacity <= 0 = pure (Left (TransportInvalidReceiveCapacity byteCapacity))
  | otherwise = mask $ \restore -> do
      result <- driveReceive (receiveAttempt transport byteCapacity) (restore (waitForRead transport))
      case result of
        Left errorValue -> pure (Left errorValue)
        Right (bytes, descriptors)
          | ByteString.null bytes && null descriptors -> pure (Right Nothing)
          | ByteString.null bytes -> do
              closeRawDescriptors descriptors
              pure (Left (TransportReceiveRejected DescriptorsWithoutBytes))
          | otherwise -> do
              owned <- mapM ownReceivedFd descriptors `onException` closeRawDescriptors descriptors
              pure (Right (Just (ReceivedChunk bytes owned)))

receiveAttempt :: Transport -> Int -> IO (ReceiveAttempt (ByteString, [Fd]) TransportError)
receiveAttempt transport byteCapacity = do
  result <-
    withTransportFd transport $ \descriptor ->
      receiveRaw descriptor byteCapacity maxDescriptorsPerMessage
  pure $ case result of
    RawReceiveSystemError errorNumber
      | errorNumber == errnoCode eINTR -> ReceiveInterrupted
      | wouldBlock errorNumber -> ReceiveWouldBlock
      -- Linux reports a peer that closed with our bytes unread as ECONNRESET,
      -- but only after it has delivered every byte the peer sent.
      | errorNumber == errnoCode eCONNRESET -> ReceiveProgress (ByteString.empty, [])
      | otherwise -> ReceiveFailed (TransportSystemError ReceiveOperation errorNumber)
    RawReceiveRejected errorValue -> ReceiveFailed (TransportReceiveRejected (ancillaryError errorValue))
    RawReceived bytes descriptors -> ReceiveProgress (bytes, descriptors)

ownReceivedFd :: Fd -> IO OwnedFd
ownReceivedFd descriptor = OwnedFd <$> newMVar (Just descriptor)

withTransportFd :: Transport -> (Fd -> IO value) -> IO value
withTransportFd (Transport connection _) action =
  withFdSocket connection (action . Fd)

waitForRead :: Transport -> IO ()
waitForRead transport = withTransportFd transport threadWaitRead

waitForWrite :: Transport -> IO ()
waitForWrite transport = withTransportFd transport threadWaitWrite

wouldBlock :: CInt -> Bool
wouldBlock errorNumber = errorNumber == errnoCode eAGAIN || errorNumber == errnoCode eWOULDBLOCK

errnoCode :: Errno -> CInt
errnoCode (Errno value) = value

closeOwnedDescriptors :: [OwnedFd] -> IO ()
closeOwnedDescriptors = mapM_ (try @IOException . closeOwnedFd)

closeRawDescriptors :: [Fd] -> IO ()
closeRawDescriptors = mapM_ closeIgnoringErrors

closeIgnoringErrors :: Fd -> IO ()
closeIgnoringErrors descriptor = void (try @IOException (closeFd descriptor))

ancillaryError :: RawReceiveError -> AncillaryError
ancillaryError rawError = case rawError of
  RawControlTruncated -> ControlTruncated
  RawMalformedControl -> MalformedControl
  RawUnsupportedControl -> UnsupportedControl
  RawTooManyFds -> TooManyReceivedFds
  RawCloseOnExecFailed errorNumber -> CloseOnExecFailed errorNumber
  RawDataTruncated -> DataTruncated
  RawUnknownReceiveStatus status -> UnknownReceiveStatus status
