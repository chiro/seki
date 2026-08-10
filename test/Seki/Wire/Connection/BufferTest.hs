module Seki.Wire.Connection.BufferTest (tests) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Seki.Wire.Arguments (ArgumentEncoder, getFd, getUint, putUint, runArgumentEncoder)
import Seki.Wire.Codec (encodeFrame, frameHeaderSize)
import Seki.Wire.Connection.Buffer (
  ArgumentPoll (..),
  FramePoll (..),
  ReceiveBuffer,
  ReceiveBufferError (..),
  appendReceivedChunk,
  bufferedByteCount,
  closeReceiveBuffer,
  decodePendingArguments,
  defaultBufferLimits,
  makeBufferLimits,
  markReceiveEof,
  newReceiveBuffer,
  pollFrame,
 )
import Seki.Wire.Transport (OwnedFd, OwnedFdError (..), ReceivedChunk (..), adoptFd, closeOwnedFd, withOwnedFd)
import Seki.Wire.Types (Opcode (..), RawFrame (..), mkObjectId)
import System.Posix.IO (closeFd, createPipe, dup)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Types (Fd)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland connection receive buffer"
    [ testCase "assembles a frame from fragmented chunks" testFragmentedFrame
    , testCase "retains coalesced frames in order" testCoalescedFrames
    , testCase "keeps a frame pending until its arguments decode" testPendingFrame
    , testCase "waits for descriptors independently from bytes" testDelayedDescriptor
    , testCase "rejects argument bytes beyond the frame" testTruncatedArguments
    , testCase "rejects truncated frames at EOF" testTruncatedEof
    , testCase "drains complete frames before clean EOF" testCleanEof
    , testCase "rejects missing frame descriptors at EOF" testMissingDescriptorAtEof
    , testCase "rejects descriptors left at EOF" testDescriptorsAtEof
    , testCase "enforces byte and descriptor limits with cleanup" testLimits
    , testCase "closes queued descriptors idempotently" testCloseBuffer
    ]

testFragmentedFrame :: IO ()
testFragmentedFrame = withBuffer $ \buffer -> do
  frame <- encodedFrame 1 2 (wordPayload 17)
  assertEqual "first fragment" (Right ()) =<< appendBytes buffer (ByteString.take 5 frame)
  assertEqual
    "header remainder"
    (Right (AwaitingFrameBytes (frameHeaderSize - 5)))
    =<< pollFrame buffer
  assertEqual "second fragment" (Right ()) =<< appendBytes buffer (ByteString.drop 5 frame)
  assertFrameReady 1 2 =<< pollFrame buffer
  assertEqual
    "argument"
    (Right (ArgumentsReady 17))
    =<< decodePendingArguments buffer getUint

testCoalescedFrames :: IO ()
testCoalescedFrames = withBuffer $ \buffer -> do
  first <- encodedFrame 1 2 ByteString.empty
  second <- encodedFrame 3 4 ByteString.empty
  assertEqual "append" (Right ()) =<< appendBytes buffer (first <> second)
  assertFrameReady 1 2 =<< pollFrame buffer
  assertEqual "first arguments" (Right (ArgumentsReady ())) =<< decodePendingArguments buffer (pure ())
  assertFrameReady 3 4 =<< pollFrame buffer
  assertEqual "second arguments" (Right (ArgumentsReady ())) =<< decodePendingArguments buffer (pure ())
  assertEqual "all bytes consumed" 0 =<< bufferedByteCount buffer

testPendingFrame :: IO ()
testPendingFrame = withBuffer $ \buffer -> do
  frame <- encodedFrame 8 9 ByteString.empty
  assertEqual "append" (Right ()) =<< appendBytes buffer frame
  firstPoll <- pollFrame buffer
  secondPoll <- pollFrame buffer
  assertEqual "same pending frame" firstPoll secondPoll
  assertEqual "arguments" (Right (ArgumentsReady ())) =<< decodePendingArguments buffer (pure ())
  assertEqual "no next frame" (Right (AwaitingFrameBytes frameHeaderSize)) =<< pollFrame buffer

testDelayedDescriptor :: IO ()
testDelayedDescriptor =
  withPipe $ \readDescriptor writeDescriptor ->
    withBuffer $ \buffer -> do
      frame <- encodedFrame 1 0 ByteString.empty
      assertEqual "frame bytes" (Right ()) =<< appendBytes buffer frame
      assertFrameReady 1 0 =<< pollFrame buffer
      waiting <- decodePendingArguments buffer getFd
      case waiting of
        Right (AwaitingArgumentFds 1) -> pure ()
        other -> fail ("unexpected descriptor wait: " <> showArgumentResult other)
      transferred <- dup readDescriptor >>= adoptFd
      assertEqual
        "descriptor arrives"
        (Right ())
        =<< appendReceivedChunk buffer (ReceivedChunk ByteString.empty [transferred])
      decoded <- decodePendingArguments buffer getFd
      case decoded of
        Right (ArgumentsReady owned) -> do
          void (fdWrite writeDescriptor (ByteString.singleton 0x5a))
          assertEqual
            "received descriptor"
            (Right (ByteString.singleton 0x5a))
            =<< withOwnedFd owned (`fdRead` 1)
          void (closeOwnedFd owned)
        other -> fail ("unexpected argument result: " <> showArgumentResult other)

testTruncatedArguments :: IO ()
testTruncatedArguments = withBuffer $ \buffer -> do
  frame <- encodedFrame 1 0 ByteString.empty
  assertEqual "append" (Right ()) =<< appendBytes buffer frame
  assertFrameReady 1 0 =<< pollFrame buffer
  assertEqual
    "uint cannot extend beyond frame"
    (Left (PendingArgumentsNeedBytes 4))
    =<< decodePendingArguments buffer getUint

testTruncatedEof :: IO ()
testTruncatedEof = withBuffer $ \buffer -> do
  assertEqual "partial header" (Right ()) =<< appendBytes buffer (ByteString.replicate 3 0)
  assertEqual "EOF" (Right ()) =<< markReceiveEof buffer
  assertEqual
    "truncated header"
    (Left (TruncatedFrameAtEof 3 5))
    =<< pollFrame buffer

testCleanEof :: IO ()
testCleanEof = withBuffer $ \buffer -> do
  frame <- encodedFrame 1 0 ByteString.empty
  assertEqual "frame" (Right ()) =<< appendBytes buffer frame
  assertEqual "EOF" (Right ()) =<< markReceiveEof buffer
  assertFrameReady 1 0 =<< pollFrame buffer
  assertEqual "arguments" (Right (ArgumentsReady ())) =<< decodePendingArguments buffer (pure ())
  assertEqual "clean end" (Right ReceiveStreamFinished) =<< pollFrame buffer

testMissingDescriptorAtEof :: IO ()
testMissingDescriptorAtEof = withBuffer $ \buffer -> do
  frame <- encodedFrame 1 0 ByteString.empty
  assertEqual "frame" (Right ()) =<< appendBytes buffer frame
  assertFrameReady 1 0 =<< pollFrame buffer
  assertEqual "EOF" (Right ()) =<< markReceiveEof buffer
  result <- decodePendingArguments buffer getFd
  case result of
    Left (PendingArgumentsNeedFdsAtEof 1) -> pure ()
    other -> fail ("unexpected missing-descriptor result: " <> showArgumentResult other)

testDescriptorsAtEof :: IO ()
testDescriptorsAtEof =
  withPipe $ \readDescriptor _ ->
    withBuffer $ \buffer -> do
      owned <- dup readDescriptor >>= adoptFd
      assertEqual
        "descriptor"
        (Right ())
        =<< appendReceivedChunk buffer (ReceivedChunk ByteString.empty [owned])
      assertEqual "EOF" (Right ()) =<< markReceiveEof buffer
      assertEqual "orphan descriptor" (Left (DescriptorsAtEof 1)) =<< pollFrame buffer
      assertEqual "descriptor closed" (Left OwnedFdAlreadyReleased) =<< withOwnedFd owned pure

testLimits :: IO ()
testLimits = do
  limits <- either (fail . show) pure (makeBufferLimits 8 1)
  bracket (newReceiveBuffer limits) closeReceiveBuffer $ \buffer -> do
    assertEqual "initial bytes" (Right ()) =<< appendBytes buffer (ByteString.replicate 8 0)
    assertEqual
      "byte limit"
      (Left (BufferedByteLimitExceeded 9))
      =<< appendBytes buffer (ByteString.singleton 0)
  withPipe $ \readDescriptor _ -> do
    firstOwned <- dup readDescriptor >>= adoptFd
    secondOwned <- dup readDescriptor >>= adoptFd
    bracket (newReceiveBuffer limits) closeReceiveBuffer $ \buffer -> do
      assertEqual
        "first descriptor"
        (Right ())
        =<< appendReceivedChunk buffer (ReceivedChunk ByteString.empty [firstOwned])
      assertEqual
        "descriptor limit"
        (Left (QueuedFdLimitExceeded 2))
        =<< appendReceivedChunk buffer (ReceivedChunk ByteString.empty [secondOwned])
      assertEqual "first closed" (Left OwnedFdAlreadyReleased) =<< withOwnedFd firstOwned pure
      assertEqual "second closed" (Left OwnedFdAlreadyReleased) =<< withOwnedFd secondOwned pure

testCloseBuffer :: IO ()
testCloseBuffer =
  withPipe $ \readDescriptor _ -> do
    owned <- dup readDescriptor >>= adoptFd
    buffer <- newReceiveBuffer defaultBufferLimits
    assertEqual
      "append descriptor"
      (Right ())
      =<< appendReceivedChunk buffer (ReceivedChunk ByteString.empty [owned])
    closeReceiveBuffer buffer
    closeReceiveBuffer buffer
    assertEqual "descriptor closed" (Left OwnedFdAlreadyReleased) =<< withOwnedFd owned pure
    assertEqual "closed buffer" (Left ReceiveBufferClosed) =<< pollFrame buffer

withBuffer :: (ReceiveBuffer -> IO value) -> IO value
withBuffer = bracket (newReceiveBuffer defaultBufferLimits) closeReceiveBuffer

appendBytes :: ReceiveBuffer -> ByteString.ByteString -> IO (Either ReceiveBufferError ())
appendBytes buffer bytes = appendReceivedChunk buffer (ReceivedChunk bytes [])

wordPayload :: Word -> ByteString.ByteString
wordPayload value =
  case runArgumentEncoder (putUint (fromIntegral value) :: ArgumentEncoder () ()) of
    Left errorValue -> error (show errorValue)
    Right ((), bytes, []) -> bytes
    Right _ -> error "unexpected descriptors"

encodedFrame :: Word -> Word -> ByteString.ByteString -> IO ByteString.ByteString
encodedFrame sender opcode payload = do
  senderId <- maybe (fail "invalid test sender") pure (mkObjectId (fromIntegral sender))
  case encodeFrame (RawFrame senderId (Opcode (fromIntegral opcode)) payload) of
    Left errorValue -> fail (show errorValue)
    Right bytes -> pure bytes

assertFrameReady :: Word -> Word -> Either ReceiveBufferError FramePoll -> IO ()
assertFrameReady expectedSender expectedOpcode result = case result of
  Right (FrameReady frame) -> do
    assertEqual "sender" (Just (rawFrameSender frame)) (mkObjectId (fromIntegral expectedSender))
    assertEqual "opcode" (Opcode (fromIntegral expectedOpcode)) (rawFrameOpcode frame)
  other -> fail ("unexpected frame poll: " <> show other)

showArgumentResult :: Either ReceiveBufferError (ArgumentPoll OwnedFd) -> String
showArgumentResult result = case result of
  Left errorValue -> show errorValue
  Right (AwaitingArgumentFds count) -> "waiting for " <> show count <> " descriptors"
  Right (ArgumentsReady _) -> "arguments ready"

withPipe :: (Fd -> Fd -> IO value) -> IO value
withPipe action = bracket createPipe closePipe (uncurry action)

closePipe :: (Fd, Fd) -> IO ()
closePipe (readDescriptor, writeDescriptor) = do
  closeIgnoringErrors readDescriptor
  closeIgnoringErrors writeDescriptor

closeIgnoringErrors :: Fd -> IO ()
closeIgnoringErrors descriptor = void (try @IOException (closeFd descriptor))
