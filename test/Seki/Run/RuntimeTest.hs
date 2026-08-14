{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.RuntimeTest (tests) where

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
import Seki.Run.River (RiverBootstrapError (..), RiverWindowManagerHandlers (..))
import Seki.Run.Runtime (
  RiverRuntimeError (..),
  closeRiverSession,
  openRiverSessionAt,
  riverWaylandSession,
  riverWindowManager,
  runRiverSession,
 )
import Seki.Run.Startup (waylandConnection)
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  getNewId,
  getUint,
  putString,
  putUint,
  runArgumentEncoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Binding (DynamicNewId (..), getDynamicNewId)
import Seki.Wire.Codec (decodeFrame, encodeFrame)
import Seki.Wire.Connection (ConnectionError (..), dispatchNextEvent)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry (RegistryError (..), globalNameFromWord)
import Seki.Wire.Transport (
  ConnectionTarget (InheritedSocket),
  OwnedFd,
  ReceivedChunk (..),
  Transport,
  closeTransport,
  openTransport,
  receiveChunk,
  sendMessage,
 )
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..))
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river runtime"
    [ testCase "opens, binds, dispatches, and closes an owned runtime" testOwnedRuntime
    , testCase "closes startup when the manager global is missing" testMissingManager
    , testCase "promotes live registry failures from the event loop" testLiveRegistryFailure
    ]

testOwnedRuntime :: Assertion
testOwnedRuntime =
  withRuntimePair $ \target peer -> do
    managerEvent <- newEmptyMVar
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , callbackDoneFrame callbackId 91
        ]
      managerId <- receiveManagerBind reader peer
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      closeTransport peer
      pure managerId
    session <-
      expectRight
        =<< openRiverSessionAt
          target
          defaultObjectLimit
          (runtimeHandlers (putMVar managerEvent))
    managerId <- expectMockSuccess mockDone
    assertEqual "bound manager object" managerId (objectId (riverWindowManager session))
    assertEqual "clean event-loop EOF" (Right ()) =<< runRiverSession session
    assertEqual "typed manager event" River.RiverWindowManagerV1ManageStart =<< takeMVar managerEvent
    assertEqual
      "runtime owns session close"
      (Left ConnectionClosed)
      =<< dispatchNextEvent (waylandConnection (riverWaylandSession session))
    assertEqual
      "event loop is one-shot"
      (Left RiverRuntimeEventLoopAlreadyOwned)
      =<< runRiverSession session
    closeRiverSession session

testMissingManager :: Assertion
testMissingManager =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 3 "wl_output" 4
        , callbackDoneFrame callbackId 1
        ]
    expectRuntimeLeft
      (RiverRuntimeBootstrapError RiverWindowManagerGlobalMissing)
      =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (\_ -> pure ()))
    expectMockSuccess mockDone

testLiveRegistryFailure :: Assertion
testLiveRegistryFailure =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , callbackDoneFrame callbackId 2
        ]
      _ <- receiveManagerBind reader peer
      sendFrames peer [registryGlobalFrame registryId 7 "river_window_manager_v1" 4]
    session <-
      expectRight
        =<< openRiverSessionAt
          target
          defaultObjectLimit
          (runtimeHandlers (\_ -> pure ()))
    expectMockSuccess mockDone
    let expectedRegistryError =
          DuplicateGlobalName
            (globalNameFromWord 7)
            "river_window_manager_v1"
            "river_window_manager_v1"
    assertEqual
      "live registry error"
      (Left (RiverRuntimeRegistryError expectedRegistryError))
      =<< runRiverSession session

runtimeHandlers ::
  (River.RiverWindowManagerV1Event OwnedFd -> IO ()) ->
  RiverWindowManagerHandlers
runtimeHandlers managerHandler =
  RiverWindowManagerHandlers
    { handleRiverWindowManagerEvent = \_ event -> managerHandler event
    , handleRiverWindowCreated = \_ -> pure ()
    , handleRiverWindowEvent = \_ _ -> pure ()
    , handleRiverOutputCreated = \_ -> pure ()
    , handleRiverOutputEvent = \_ _ -> pure ()
    , handleRiverSeatCreated = \_ -> pure ()
    , handleRiverSeatEvent = \_ _ -> pure ()
    }

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

receiveManagerBind :: FrameReader -> Transport -> IO ObjectId
receiveManagerBind reader peer = do
  frame <- receiveFrame reader peer
  assertEqual "manager bind opcode" (Opcode 0) (rawFrameOpcode frame)
  case runCompleteArgumentDecoder ((,) <$> getUint <*> getDynamicNewId) (rawFramePayload frame) ([] :: [()]) of
    Done (name, dynamicId) remaining []
      | ByteString.null remaining -> do
          assertEqual "manager global name" 7 name
          assertEqual "manager interface" "river_window_manager_v1" (dynamicNewIdInterface dynamicId)
          assertEqual "manager version" 4 (dynamicNewIdVersion dynamicId)
          pure (dynamicNewIdObject dynamicId)
    result -> assertFailure ("unexpected manager bind payload: " <> show result)

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

expectTransport :: (Show errorValue) => Either errorValue value -> IO value
expectTransport = either (assertFailure . ("unexpected transport error: " <>) . show) pure

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

expectRuntimeLeft :: RiverRuntimeError -> Either RiverRuntimeError session -> Assertion
expectRuntimeLeft expected result = case result of
  Left actual -> assertEqual "runtime error" expected actual
  Right _ -> assertFailure "expected runtime error"
