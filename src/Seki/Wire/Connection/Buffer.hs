-- | Buffered frame and descriptor input for a Wayland connection.
module Seki.Wire.Connection.Buffer (
  BufferLimits,
  makeBufferLimits,
  bufferedByteLimit,
  queuedFdLimit,
  defaultBufferLimits,
  ReceiveBuffer,
  ReceiveBufferError (..),
  FramePoll (..),
  ArgumentPoll (..),
  newReceiveBuffer,
  appendReceivedChunk,
  markReceiveEof,
  pollFrame,
  decodePendingArguments,
  bufferedByteCount,
  queuedFdCount,
  closeReceiveBuffer,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Seki.Wire.Arguments (
  ArgumentDecoder,
  ArgumentError,
  DecodeResult (..),
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Codec (decodeFrame, frameHeaderSize)
import Seki.Wire.Transport (OwnedFd, ReceivedChunk (..), closeOwnedFd)
import Seki.Wire.Types (FrameDecode (..), FrameError, RawFrame (..))

-- | Resource limits for undispatched connection input.
data BufferLimits = BufferLimits
  { bufferedByteLimit :: Int
  , queuedFdLimit :: Int
  }
  deriving (Eq, Show)

-- | Validate receive-buffer resource limits.
makeBufferLimits :: Int -> Int -> Either ReceiveBufferError BufferLimits
makeBufferLimits byteLimit fdLimit
  | byteLimit <= 0 = Left (InvalidBufferedByteLimit byteLimit)
  | fdLimit <= 0 = Left (InvalidQueuedFdLimit fdLimit)
  | otherwise = Right (BufferLimits byteLimit fdLimit)

-- | Conservative defaults for one Wayland connection.
defaultBufferLimits :: BufferLimits
defaultBufferLimits = BufferLimits (1024 * 1024) 256

-- | A fatal input-buffer or frame-decoding failure.
data ReceiveBufferError
  = InvalidBufferedByteLimit Int
  | InvalidQueuedFdLimit Int
  | BufferedByteLimitExceeded Int
  | QueuedFdLimitExceeded Int
  | ReceivedChunkAfterEof
  | FrameDecodeFailed FrameError
  | TruncatedFrameAtEof Int Int
  | DescriptorsAtEof Int
  | NoPendingFrame
  | PendingArgumentsNeedBytes Int
  | PendingArgumentsNeedFdsAtEof Int
  | PendingArgumentDecodeFailed ArgumentError
  | ReceiveBufferClosed
  deriving (Eq, Show)

-- | Result of polling for the next complete frame.
data FramePoll
  = FrameReady RawFrame
  | AwaitingFrameBytes Int
  | ReceiveStreamFinished
  deriving (Eq, Show)

-- | Result of decoding the arguments of the pending frame.
data ArgumentPoll value
  = ArgumentsReady value
  | AwaitingArgumentFds Int
  deriving (Eq, Show)

data BufferPhase
  = Receiving
  | ReceivedEof
  | BufferFailed ReceiveBufferError
  | BufferClosed

data BufferState = BufferState
  { bufferPhase :: BufferPhase
  , bufferBytes :: ByteString
  , bufferFds :: [OwnedFd]
  , pendingFrame :: Maybe RawFrame
  }

data ReceiveBuffer = ReceiveBuffer BufferLimits (MVar BufferState)

-- | Create an empty open receive buffer.
newReceiveBuffer :: BufferLimits -> IO ReceiveBuffer
newReceiveBuffer limits =
  ReceiveBuffer limits
    <$> newMVar
      BufferState
        { bufferPhase = Receiving
        , bufferBytes = ByteString.empty
        , bufferFds = []
        , pendingFrame = Nothing
        }

{- | Append bytes and descriptors from one transport read.

Ownership of every descriptor transfers to the buffer, including when the
append exceeds a limit and closes the buffer.
-}
appendReceivedChunk :: ReceiveBuffer -> ReceivedChunk -> IO (Either ReceiveBufferError ())
appendReceivedChunk (ReceiveBuffer limits stateVariable) chunk =
  modifyMVar stateVariable $ \state ->
    case bufferPhase state of
      Receiving -> appendToOpenBuffer limits state chunk
      ReceivedEof -> failWithChunk state chunk ReceivedChunkAfterEof
      BufferFailed errorValue -> do
        closeDescriptors (receivedFds chunk)
        pure (state, Left errorValue)
      BufferClosed -> do
        closeDescriptors (receivedFds chunk)
        pure (state, Left ReceiveBufferClosed)

appendToOpenBuffer :: BufferLimits -> BufferState -> ReceivedChunk -> IO (BufferState, Either ReceiveBufferError ())
appendToOpenBuffer limits state chunk
  | totalBytes > bufferedByteLimit limits =
      failWithChunk state chunk (BufferedByteLimitExceeded totalBytes)
  | totalFds > queuedFdLimit limits =
      failWithChunk state chunk (QueuedFdLimitExceeded totalFds)
  | otherwise =
      pure
        ( state
            { bufferBytes = bufferBytes state <> receivedBytes chunk
            , bufferFds = bufferFds state <> receivedFds chunk
            }
        , Right ()
        )
 where
  totalBytes = bufferedBytesInState state + ByteString.length (receivedBytes chunk)
  totalFds = length (bufferFds state) + length (receivedFds chunk)

-- | Record transport EOF. Buffered complete frames may still be drained.
markReceiveEof :: ReceiveBuffer -> IO (Either ReceiveBufferError ())
markReceiveEof (ReceiveBuffer _ stateVariable) =
  modifyMVar stateVariable $ \state ->
    case bufferPhase state of
      Receiving -> pure (state{bufferPhase = ReceivedEof}, Right ())
      ReceivedEof -> pure (state, Right ())
      BufferFailed errorValue -> pure (state, Left errorValue)
      BufferClosed -> pure (state, Left ReceiveBufferClosed)

{- | Poll for one frame without permitting a later frame to overtake it.

Once ready, the same pending frame is returned until its arguments decode
successfully or the buffer fails.
-}
pollFrame :: ReceiveBuffer -> IO (Either ReceiveBufferError FramePoll)
pollFrame (ReceiveBuffer _ stateVariable) =
  modifyMVar stateVariable $ \state ->
    case bufferPhase state of
      BufferFailed errorValue -> pure (state, Left errorValue)
      BufferClosed -> pure (state, Left ReceiveBufferClosed)
      _ -> pollOpenState state

pollOpenState :: BufferState -> IO (BufferState, Either ReceiveBufferError FramePoll)
pollOpenState state = case pendingFrame state of
  Just frame -> pure (state, Right (FrameReady frame))
  Nothing ->
    case decodeFrame (bufferBytes state) of
      Left frameError -> failState state (FrameDecodeFailed frameError)
      Right (DecodedFrame frame suffix) ->
        pure
          ( state
              { bufferBytes = suffix
              , pendingFrame = Just frame
              }
          , Right (FrameReady frame)
          )
      Right (NeedMoreBytes count) -> incompleteFrame state count

incompleteFrame :: BufferState -> Int -> IO (BufferState, Either ReceiveBufferError FramePoll)
incompleteFrame state needed = case bufferPhase state of
  Receiving -> pure (state, Right (AwaitingFrameBytes needed))
  ReceivedEof
    | ByteString.null (bufferBytes state) && null (bufferFds state) ->
        pure (state, Right ReceiveStreamFinished)
    | ByteString.null (bufferBytes state) ->
        failState state (DescriptorsAtEof (length (bufferFds state)))
    | otherwise ->
        failState state (TruncatedFrameAtEof (ByteString.length (bufferBytes state)) needed)
  BufferFailed errorValue -> pure (state, Left errorValue)
  BufferClosed -> pure (state, Left ReceiveBufferClosed)

{- | Decode the pending frame's declared arguments against the FD queue.

Missing bytes are fatal because the complete frame payload is already known.
Missing descriptors wait while receiving and become fatal at EOF. Successful
decoding consumes exactly the descriptors used by the decoder.
-}
decodePendingArguments :: ReceiveBuffer -> ArgumentDecoder OwnedFd value -> IO (Either ReceiveBufferError (ArgumentPoll value))
decodePendingArguments buffer = consumePendingArguments buffer (const (pure ()))

consumePendingArguments ::
  ReceiveBuffer ->
  ([OwnedFd] -> IO ()) ->
  ArgumentDecoder OwnedFd value ->
  IO (Either ReceiveBufferError (ArgumentPoll value))
consumePendingArguments (ReceiveBuffer _ stateVariable) consumed decoder =
  modifyMVar stateVariable $ \state ->
    case bufferPhase state of
      BufferFailed errorValue -> pure (state, Left errorValue)
      BufferClosed -> pure (state, Left ReceiveBufferClosed)
      _ -> decodePending state consumed decoder

decodePending :: BufferState -> ([OwnedFd] -> IO ()) -> ArgumentDecoder OwnedFd value -> IO (BufferState, Either ReceiveBufferError (ArgumentPoll value))
decodePending state consumed decoder = case pendingFrame state of
  Nothing -> pure (state, Left NoPendingFrame)
  Just frame ->
    case runCompleteArgumentDecoder decoder (rawFramePayload frame) (bufferFds state) of
      Done value _ remainingDescriptors -> do
        consumed (take (length (bufferFds state) - length remainingDescriptors) (bufferFds state))
        pure
          ( state
              { bufferFds = remainingDescriptors
              , pendingFrame = Nothing
              }
          , Right (ArgumentsReady value)
          )
      NeedBytes count -> failState state (PendingArgumentsNeedBytes count)
      NeedFds count -> case bufferPhase state of
        Receiving -> pure (state, Right (AwaitingArgumentFds count))
        ReceivedEof -> failState state (PendingArgumentsNeedFdsAtEof count)
        BufferFailed errorValue -> pure (state, Left errorValue)
        BufferClosed -> pure (state, Left ReceiveBufferClosed)
      DecodeFailed argumentError -> failState state (PendingArgumentDecodeFailed argumentError)

-- | Current number of undispatched bytes.
bufferedByteCount :: ReceiveBuffer -> IO Int
bufferedByteCount (ReceiveBuffer _ stateVariable) =
  bufferedBytesInState <$> readMVar stateVariable

-- | Current number of unconsumed descriptors.
queuedFdCount :: ReceiveBuffer -> IO Int
queuedFdCount (ReceiveBuffer _ stateVariable) =
  length . bufferFds <$> readMVar stateVariable

-- | Close every queued descriptor and make the buffer unusable.
closeReceiveBuffer :: ReceiveBuffer -> IO ()
closeReceiveBuffer (ReceiveBuffer _ stateVariable) =
  modifyMVar stateVariable $ \state -> do
    closeDescriptors (bufferFds state)
    pure
      ( state
          { bufferPhase = BufferClosed
          , bufferBytes = ByteString.empty
          , bufferFds = []
          , pendingFrame = Nothing
          }
      , ()
      )

failWithChunk :: BufferState -> ReceivedChunk -> ReceiveBufferError -> IO (BufferState, Either ReceiveBufferError value)
failWithChunk state chunk errorValue = do
  closeDescriptors (bufferFds state <> receivedFds chunk)
  pure (failedState state errorValue, Left errorValue)

failState :: BufferState -> ReceiveBufferError -> IO (BufferState, Either ReceiveBufferError value)
failState state errorValue = do
  closeDescriptors (bufferFds state)
  pure (failedState state errorValue, Left errorValue)

failedState :: BufferState -> ReceiveBufferError -> BufferState
failedState state errorValue =
  state
    { bufferPhase = BufferFailed errorValue
    , bufferBytes = ByteString.empty
    , bufferFds = []
    , pendingFrame = Nothing
    }

closeDescriptors :: [OwnedFd] -> IO ()
closeDescriptors = mapM_ (try @IOException . closeOwnedFd)

bufferedBytesInState :: BufferState -> Int
bufferedBytesInState state =
  ByteString.length (bufferBytes state)
    + maybe 0 pendingSize (pendingFrame state)
 where
  pendingSize frame = frameHeaderSize + ByteString.length (rawFramePayload frame)
