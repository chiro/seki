{- | Deterministic retry drivers for non-blocking transport syscalls.

The drivers contain no socket or descriptor representation details. This keeps
partial-progress and cleanup policy testable without relying on kernel timing.
-}
module Seki.Wire.Transport.Driver (
  SendAttempt (..),
  SendDriverError (..),
  driveSend,
  ReceiveAttempt (..),
  driveReceive,
)
where

import Control.Exception (mask, onException)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (atomicModifyIORef', newIORef)

-- | Result of one non-blocking send attempt.
data SendAttempt errorValue
  = SendProgress Int
  | SendInterrupted
  | SendWouldBlock
  | SendFailed errorValue
  deriving (Eq, Show)

-- | Terminal failure of the send driver.
data SendDriverError errorValue
  = SendDescriptorsWithoutBytes
  | SendNoProgress
  | SendInvalidProgress Int Int
  | SendDriverFailed errorValue
  deriving (Eq, Show)

{- | Send all bytes while disposing attached descriptors exactly once.

The attempt callback receives descriptors until its first positive byte count.
After that progress, descriptors have crossed the kernel boundary and are
disposed locally before the remaining bytes continue without ancillary data.
They are also disposed on failure or exception before any progress.
-}
driveSend ::
  ByteString ->
  [descriptor] ->
  (ByteString -> [descriptor] -> IO (SendAttempt errorValue)) ->
  IO () ->
  ([descriptor] -> IO ()) ->
  IO (Either (SendDriverError errorValue) ())
driveSend bytes descriptors attempt waitForWritable dispose =
  mask $ \restore -> do
    pendingDescriptors <- newIORef (if null descriptors then Nothing else Just descriptors)
    let disposePending = do
          pending <- atomicModifyIORef' pendingDescriptors (Nothing,)
          maybe (pure ()) dispose pending
        run = restore (sendLoop bytes descriptors attempt waitForWritable disposePending)
    result <- run `onException` disposePending
    case result of
      Left errorValue -> disposePending >> pure (Left errorValue)
      Right () -> pure (Right ())

sendLoop ::
  ByteString ->
  [descriptor] ->
  (ByteString -> [descriptor] -> IO (SendAttempt errorValue)) ->
  IO () ->
  IO () ->
  IO (Either (SendDriverError errorValue) ())
sendLoop bytes descriptors attempt waitForWritable disposePending
  | ByteString.null bytes = do
      if null descriptors
        then pure (Right ())
        else pure (Left SendDescriptorsWithoutBytes)
  | otherwise = do
      outcome <- attempt bytes descriptors
      case outcome of
        SendInterrupted -> sendLoop bytes descriptors attempt waitForWritable disposePending
        SendWouldBlock -> waitForWritable >> sendLoop bytes descriptors attempt waitForWritable disposePending
        SendFailed errorValue -> pure (Left (SendDriverFailed errorValue))
        SendProgress byteCount
          | byteCount == 0 -> pure (Left SendNoProgress)
          | byteCount < 0 || byteCount > available -> pure (Left (SendInvalidProgress byteCount available))
          | otherwise -> do
              unless (null descriptors) disposePending
              sendLoop (ByteString.drop byteCount bytes) [] attempt waitForWritable disposePending
 where
  available = ByteString.length bytes

-- | Result of one non-blocking receive attempt.
data ReceiveAttempt value errorValue
  = ReceiveProgress value
  | ReceiveInterrupted
  | ReceiveWouldBlock
  | ReceiveFailed errorValue
  deriving (Eq, Show)

-- | Retry interrupted and blocked receives until progress or failure.
driveReceive ::
  IO (ReceiveAttempt value errorValue) ->
  IO () ->
  IO (Either errorValue value)
driveReceive attempt waitForReadable = do
  outcome <- attempt
  case outcome of
    ReceiveProgress value -> pure (Right value)
    ReceiveInterrupted -> driveReceive attempt waitForReadable
    ReceiveWouldBlock -> waitForReadable >> driveReceive attempt waitForReadable
    ReceiveFailed errorValue -> pure (Left errorValue)
