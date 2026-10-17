{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A scripted river compositor on a socket pair, for runtime tests.
module Seki.Run.MockRiver (
  FrameReader,
  newFrameReader,
  receiveFrame,
  receiveStartupRequests,
  decodeNewId,
  receiveRuntimeBinds,
  receiveRuntimeBindsAtManagerVersion,
  receiveGlobalBind,
  receiveKeyBindingRequest,
  expectNoBootstrapBind,
  receiveSequenceCompletion,
  receiveExpectedRequest,
  receiveNewIdRequest,
  registryGlobalFrame,
  callbackDoneFrame,
  encodedFrame,
  encodedPayload,
  sendFrames,
  runMock,
  expectMockSuccess,
  withRuntimePair,
  openRuntimePair,
  duplicateSocket,
  requireObjectId,
  expectTransport,
  expectRight,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, onException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word32)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  getNewId,
  getObject,
  getUint,
  putString,
  putUint,
  runArgumentEncoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Binding (DynamicNewId (..), getDynamicNewId)
import Seki.Wire.Codec (decodeFrame, encodeFrame)
import Seki.Wire.Transport (
  ConnectionTarget (InheritedSocket),
  ReceivedChunk (..),
  Transport,
  closeTransport,
  openTransport,
  receiveChunk,
  sendMessage,
 )
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..), mkObjectId)
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure)

newtype FrameReader = FrameReader (IORef ByteString)

newFrameReader :: IO FrameReader
newFrameReader = FrameReader <$> newIORef ByteString.empty

receiveFrame :: FrameReader -> Transport -> IO RawFrame
receiveFrame (FrameReader buffered) peer = do
  bytes <- readIORef buffered
  case decodeFrame bytes of
    Right (DecodedFrame frame suffix) -> writeIORef buffered suffix >> pure frame
    Right (NeedMoreBytes needed) -> do
      receiveChunk peer (max 8 needed) >>= \case
        Right (Just chunk)
          | null (receivedFds chunk) -> do
              writeIORef buffered (bytes <> receivedBytes chunk)
              receiveFrame (FrameReader buffered) peer
          | otherwise -> assertFailure "unexpected descriptors"
        _ -> assertFailure "unexpected mock receive result"
    Left errorValue -> assertFailure ("unexpected frame error: " <> show errorValue)

receiveStartupRequests :: FrameReader -> Transport -> IO (ObjectId, ObjectId)
receiveStartupRequests reader peer = do
  registryFrame <- receiveFrame reader peer
  callbackFrame <- receiveFrame reader peer
  assertEqual "get_registry opcode" (Opcode 1) (rawFrameOpcode registryFrame)
  assertEqual "sync opcode" (Opcode 0) (rawFrameOpcode callbackFrame)
  (,) <$> decodeNewId registryFrame <*> decodeNewId callbackFrame

decodeNewId :: RawFrame -> IO ObjectId
decodeNewId frame = case runCompleteArgumentDecoder getNewId (rawFramePayload frame) ([] :: [()]) of
  Done identifier remaining []
    | ByteString.null remaining -> pure identifier
  result -> assertFailure ("unexpected new_id payload: " <> show result)

-- | Receive the manager, XKB, and layer-shell binds, returning their IDs in that order.
receiveRuntimeBinds :: FrameReader -> Transport -> IO (ObjectId, ObjectId, ObjectId)
receiveRuntimeBinds reader peer =
  receiveRuntimeBindsAtManagerVersion reader peer 4

receiveRuntimeBindsAtManagerVersion ::
  FrameReader ->
  Transport ->
  Word32 ->
  IO (ObjectId, ObjectId, ObjectId)
receiveRuntimeBindsAtManagerVersion reader peer managerVersion = do
  managerId <-
    receiveGlobalBind
      reader
      peer
      "manager"
      7
      "river_window_manager_v1"
      managerVersion
  xkbBindingsId <-
    receiveGlobalBind
      reader
      peer
      "XKB"
      8
      "river_xkb_bindings_v1"
      3
  layerShellId <-
    receiveGlobalBind
      reader
      peer
      "layer shell"
      9
      "river_layer_shell_v1"
      1
  pure (managerId, xkbBindingsId, layerShellId)

receiveGlobalBind ::
  FrameReader ->
  Transport ->
  String ->
  Word32 ->
  Text ->
  Word32 ->
  IO ObjectId
receiveGlobalBind reader peer label expectedName expectedInterface expectedVersion = do
  frame <- receiveFrame reader peer
  assertEqual (label <> " bind opcode") (Opcode 0) (rawFrameOpcode frame)
  case runCompleteArgumentDecoder ((,) <$> getUint <*> getDynamicNewId) (rawFramePayload frame) ([] :: [()]) of
    Done (name, dynamicId) remaining []
      | ByteString.null remaining -> do
          assertEqual (label <> " global name") expectedName name
          assertEqual (label <> " interface") expectedInterface (dynamicNewIdInterface dynamicId)
          assertEqual (label <> " version") expectedVersion (dynamicNewIdVersion dynamicId)
          pure (dynamicNewIdObject dynamicId)
    result -> assertFailure ("unexpected " <> label <> " bind payload: " <> show result)

receiveKeyBindingRequest ::
  FrameReader ->
  Transport ->
  ObjectId ->
  ObjectId ->
  RiverKeyBinding ->
  IO ObjectId
receiveKeyBindingRequest reader peer xkbBindingsId seatId definition = do
  frame <- receiveFrame reader peer
  assertEqual "binding request sender" xkbBindingsId (rawFrameSender frame)
  assertEqual "get_xkb_binding opcode" (Opcode 1) (rawFrameOpcode frame)
  let decoded =
        runCompleteArgumentDecoder
          ((,,,) <$> getObject <*> getNewId <*> getUint <*> getUint)
          (rawFramePayload frame)
          ([] :: [()])
  case decoded of
    Done (actualSeatId, bindingId, keysym, modifiers) remaining []
      | ByteString.null remaining -> do
          assertEqual "binding seat" seatId actualSeatId
          assertEqual "binding keysym" (riverKeyBindingKeysym definition) keysym
          assertEqual
            "binding modifiers"
            (River.riverSeatV1ModifiersFlagsValue (riverKeyBindingModifiers definition))
            modifiers
          pure bindingId
    result -> assertFailure ("unexpected key binding request: " <> show result)

expectNoBootstrapBind :: Transport -> Assertion
expectNoBootstrapBind peer =
  receiveChunk peer 8 >>= \case
    Right Nothing -> pure ()
    Right (Just _) -> assertFailure "unexpected bind before bootstrap validation"
    Left errorValue -> assertFailure ("unexpected mock receive error: " <> show errorValue)

receiveSequenceCompletion :: FrameReader -> Transport -> ObjectId -> Opcode -> IO ()
receiveSequenceCompletion reader peer managerId opcode = do
  receiveExpectedRequest reader peer managerId opcode (pure ())

receiveExpectedRequest ::
  FrameReader ->
  Transport ->
  ObjectId ->
  Opcode ->
  ArgumentEncoder () () ->
  Assertion
receiveExpectedRequest reader peer sender opcode arguments = do
  frame <- receiveFrame reader peer
  assertEqual "request sender" sender (rawFrameSender frame)
  assertEqual "request opcode" opcode (rawFrameOpcode frame)
  payload <- encodedPayload arguments
  assertEqual "request payload" payload (rawFramePayload frame)

receiveNewIdRequest :: FrameReader -> Transport -> ObjectId -> Opcode -> IO ObjectId
receiveNewIdRequest reader peer sender opcode = do
  frame <- receiveFrame reader peer
  assertEqual "new-ID request sender" sender (rawFrameSender frame)
  assertEqual "new-ID request opcode" opcode (rawFrameOpcode frame)
  decodeNewId frame

registryGlobalFrame :: ObjectId -> Word32 -> Text -> Word32 -> IO ByteString
registryGlobalFrame registryId name interfaceName version =
  encodedFrame registryId (Opcode 0) $ do
    putUint name
    putString interfaceName
    putUint version

callbackDoneFrame :: ObjectId -> Word32 -> IO ByteString
callbackDoneFrame callbackId serial =
  encodedFrame callbackId (Opcode 0) (putUint serial)

encodedFrame :: ObjectId -> Opcode -> ArgumentEncoder () () -> IO ByteString
encodedFrame sender opcode arguments = do
  payload <- encodedPayload arguments
  expectRight (encodeFrame (RawFrame sender opcode payload))

encodedPayload :: ArgumentEncoder () () -> IO ByteString
encodedPayload encoder = case runArgumentEncoder encoder of
  Left errorValue -> assertFailure ("unexpected argument error: " <> show errorValue)
  Right ((), payload, []) -> pure payload
  Right (_, _, descriptors) -> assertFailure ("unexpected descriptors: " <> show (length descriptors))

sendFrames :: Transport -> [IO ByteString] -> IO ()
sendFrames peer frameActions = do
  bytes <- ByteString.concat <$> sequence frameActions
  assertEqual "send mock events" (Right ()) =<< sendMessage peer bytes []

runMock :: IO value -> IO (MVar (Either SomeException value))
runMock action = do
  result <- newEmptyMVar
  void (forkIO (try action >>= putMVar result))
  pure result

expectMockSuccess :: MVar (Either SomeException value) -> IO value
expectMockSuccess result =
  takeMVar result >>= either (assertFailure . ("mock compositor failed: " <>) . show) pure

withRuntimePair :: (ConnectionTarget -> Transport -> IO value) -> IO value
withRuntimePair action =
  bracket openRuntimePair (closeTransport . snd) (uncurry action)

openRuntimePair :: IO (ConnectionTarget, Transport)
openRuntimePair = do
  (firstSocket, secondSocket) <- socketPair AF_UNIX Stream defaultProtocol
  firstDescriptor <- duplicateSocket firstSocket
  secondDescriptor <- duplicateSocket secondSocket `onException` closeFd firstDescriptor
  close firstSocket
  close secondSocket
  peer <- (expectTransport =<< openTransport (InheritedSocket secondDescriptor)) `onException` closeFd firstDescriptor
  pure (InheritedSocket firstDescriptor, peer)

duplicateSocket :: Socket -> IO Fd
duplicateSocket connection = withFdSocket connection (dup . Fd)

requireObjectId :: Word32 -> IO ObjectId
requireObjectId rawId = maybe (assertFailure ("invalid object ID: " <> show rawId)) pure (mkObjectId rawId)

expectTransport :: (Show errorValue) => Either errorValue value -> IO value
expectTransport = either (assertFailure . ("unexpected transport error: " <>) . show) pure

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure
