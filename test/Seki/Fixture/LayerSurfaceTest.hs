{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Fixture.LayerSurfaceTest (tests) where

import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Fixture.LayerSurface (
  FixtureConfig (..),
  FixtureControl,
  FixtureError (..),
  FixtureReady (..),
  FixtureStop (..),
  newFixtureControl,
  runLayerSurfaceFixture,
 )
import Seki.Run.MockRiver (
  callbackDoneFrame,
  decodeNewId,
  encodedFrame,
  encodedPayload,
  expectMockSuccess,
  expectRight,
  registryGlobalFrame,
  requireObjectId,
  runMock,
  sendFrames,
  withRuntimePair,
 )
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  getNewId,
  getUint,
  putInt,
  putNewId,
  putNullableObject,
  putObject,
  putString,
  putUint,
  runArgumentDecoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Binding (DynamicNewId (..), getDynamicNewId)
import Seki.Wire.Codec (decodeFrame)
import Seki.Wire.Transport (
  ConnectionTarget,
  OwnedFd,
  ReceivedChunk (..),
  Transport,
  closeOwnedFd,
  receiveChunk,
  withOwnedFd,
 )
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..))
import System.Posix.Files (fileSize, getFdStatus)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "layer-surface fixture"
    [ testCase "binds the compositor, shm, and layer-shell globals at version 1" testBindsGlobals
    , testCase "fails clearly when a required global is missing" testMissingGlobal
    , testCase "requests a top-anchored exclusive-zone layer surface" testSurfaceSetup
    , testCase "acks the configure serial before attaching a buffer" testConfigureAck
    , testCase "attaches a shared-memory buffer matching the configure" testBufferAttachment
    , testCase "withholds readiness until the sync round trip completes" testNoReadyBeforeSync
    , testCase "reports readiness once the sync round trip completes" testReadyAfterSync
    ]

testBindsGlobals :: Assertion
testBindsGlobals = do
  (result, _) <-
    withFixture defaultConfig $ \mock -> do
      registryId <- answerStartup mock standardGlobals
      mapM_
        (uncurry (expectBind mock registryId))
        [(1, "wl_compositor"), (2, "wl_shm"), (3, "zwlr_layer_shell_v1")]
  assertStoppedByConnection result

testMissingGlobal :: Assertion
testMissingGlobal = do
  (result, ()) <-
    withFixture defaultConfig $ \mock -> do
      _ <- answerStartup mock (filter (\(_, name, _) -> name /= "wl_shm") standardGlobals)
      expectEndOfRequests mock
  assertEqual "missing global" (Left (FixtureMissingGlobal "wl_shm")) (fixtureOutcome result)

testSurfaceSetup :: Assertion
testSurfaceSetup = do
  (result, _) <- withFixture FixtureConfig{fixtureHeight = 40} (expectSurfaceSetup 40)
  assertStoppedByConnection result

data Objects = Objects
  { objectsShm :: ObjectId
  , objectsSurface :: ObjectId
  , objectsLayerSurface :: ObjectId
  }

expectSurfaceSetup :: Word32 -> Mock -> IO Objects
expectSurfaceSetup height mock = do
  registryId <- answerStartup mock standardGlobals
  compositorId <- expectBind mock registryId 1 "wl_compositor"
  shmId <- expectBind mock registryId 2 "wl_shm"
  layerShellId <- expectBind mock registryId 3 "zwlr_layer_shell_v1"
  surfaceId <- receiveNewIdRequest mock compositorId (Opcode 0)
  layerSurfaceFrame <- receiveRequest mock
  assertEqual "get_layer_surface sender" layerShellId (rawFrameSender layerSurfaceFrame)
  assertEqual "get_layer_surface opcode" (Opcode 0) (rawFrameOpcode layerSurfaceFrame)
  layerSurfaceId <- decodeLeadingNewId layerSurfaceFrame
  expectPayload "get_layer_surface" layerSurfaceFrame $ do
    putNewId layerSurfaceId
    putObject surfaceId
    putNullableObject Nothing
    putUint 2
    putString "seki-layer-surface-fixture"
  expectRequest mock "set_size" layerSurfaceId (Opcode 0) (putUint 0 >> putUint height)
  expectRequest mock "set_anchor" layerSurfaceId (Opcode 1) (putUint (1 + 4 + 8))
  expectRequest mock "set_exclusive_zone" layerSurfaceId (Opcode 2) (putInt (fromIntegral height))
  expectRequest mock "initial commit" surfaceId (Opcode 6) (pure ())
  pure (Objects shmId surfaceId layerSurfaceId)

testConfigureAck :: Assertion
testConfigureAck = do
  (result, _) <-
    withFixture defaultConfig $ \mock -> do
      objects <- expectSurfaceSetup 32 mock
      sendConfigure mock objects 77 1280 32
      expectRequest mock "ack_configure" (objectsLayerSurface objects) (Opcode 6) (putUint 77)
      frame <- receiveRequest mock
      assertEqual "first buffer request follows the ack" (objectsShm objects) (rawFrameSender frame)
  assertStoppedByConnection result

testBufferAttachment :: Assertion
testBufferAttachment = do
  (result, _) <-
    withFixture FixtureConfig{fixtureHeight = 40} $ \mock -> do
      objects <- expectSurfaceSetup 40 mock
      sendConfigure mock objects 5 1280 40
      expectRequest mock "ack_configure" (objectsLayerSurface objects) (Opcode 6) (putUint 5)
      expectBufferCommit mock objects 1280 40
  assertStoppedByConnection result

testNoReadyBeforeSync :: Assertion
testNoReadyBeforeSync = do
  (result, _) <- withFixture defaultConfig (mapFixtureSurface 32)
  assertStoppedByConnection result
  assertEqual "no readiness without the sync reply" [] (fixtureReadies result)

testReadyAfterSync :: Assertion
testReadyAfterSync = do
  (result, _) <-
    withFixture defaultConfig $ \mock -> do
      (_, buffers) <- mapFixtureSurface 32 mock
      sendFrames (mockPeer mock) [callbackDoneFrame (bufferSyncCallback buffers) 2]
  assertStoppedByConnection result
  assertEqual "readiness after the sync reply" [FixtureReady 1280 32] (fixtureReadies result)

mapFixtureSurface :: Word32 -> Mock -> IO (Objects, BufferObjects)
mapFixtureSurface height mock = do
  objects <- expectSurfaceSetup height mock
  sendConfigure mock objects 9 1280 height
  expectRequest mock "ack_configure" (objectsLayerSurface objects) (Opcode 6) (putUint 9)
  buffers <- expectBufferCommit mock objects 1280 height
  pure (objects, buffers)

data BufferObjects = BufferObjects
  { bufferPool :: ObjectId
  , bufferBuffer :: ObjectId
  , bufferSyncCallback :: ObjectId
  }

expectBufferCommit :: Mock -> Objects -> Word32 -> Word32 -> IO BufferObjects
expectBufferCommit mock objects width height = do
  let stride = width * 4
      poolSize = stride * height
  poolFrame <- receiveRequest mock
  assertEqual "create_pool sender" (objectsShm objects) (rawFrameSender poolFrame)
  assertEqual "create_pool opcode" (Opcode 0) (rawFrameOpcode poolFrame)
  poolId <- decodeLeadingNewId poolFrame
  expectPayload "create_pool" poolFrame (putNewId poolId >> putInt (fromIntegral poolSize))
  poolFd <- takePassedFd mock
  descriptorSize <- withOwnedFd poolFd (fmap fileSize . getFdStatus) >>= expectRight
  _ <- closeOwnedFd poolFd
  assertEqual "passed pool descriptor size" (fromIntegral poolSize) descriptorSize
  bufferRequest@(bufferId, _) <- receiveNewIdRequestWithArguments mock poolId (Opcode 0)
  expectRequestArguments "create_buffer" bufferRequest $ do
    putInt 0
    putInt (fromIntegral width)
    putInt (fromIntegral height)
    putInt (fromIntegral stride)
    putUint 1
  expectRequest mock "attach" (objectsSurface objects) (Opcode 1) (putObject bufferId >> putInt 0 >> putInt 0)
  expectRequest mock "damage" (objectsSurface objects) (Opcode 2) $ do
    putInt 0
    putInt 0
    putInt (fromIntegral width)
    putInt (fromIntegral height)
  expectRequest mock "buffer commit" (objectsSurface objects) (Opcode 6) (pure ())
  display <- requireObjectId 1
  callbackId <- receiveNewIdRequest mock display (Opcode 0)
  pure (BufferObjects poolId bufferId callbackId)

sendConfigure :: Mock -> Objects -> Word32 -> Word32 -> Word32 -> IO ()
sendConfigure mock objects serial width height =
  sendFrames
    (mockPeer mock)
    [encodedFrame (objectsLayerSurface objects) (Opcode 0) (putUint serial >> putUint width >> putUint height)]

defaultConfig :: FixtureConfig
defaultConfig = FixtureConfig{fixtureHeight = 32}

standardGlobals :: [(Word32, Text, Word32)]
standardGlobals =
  [ (1, "wl_compositor", 6)
  , (2, "wl_shm", 2)
  , (3, "zwlr_layer_shell_v1", 5)
  , (4, "wl_seat", 9)
  ]

data FixtureResult = FixtureResult
  { fixtureOutcome :: Either FixtureError FixtureStop
  , fixtureReadies :: [FixtureReady]
  }

assertStoppedByConnection :: FixtureResult -> Assertion
assertStoppedByConnection result = case fixtureOutcome result of
  Left _ -> pure ()
  Right stop -> assertFailure ("expected the closed mock to fail the fixture, got: " <> show stop)

withFixture :: FixtureConfig -> (Mock -> IO value) -> IO (FixtureResult, value)
withFixture config script = withFixtureControl config (const script)

withFixtureControl :: FixtureConfig -> (FixtureControl -> Mock -> IO value) -> IO (FixtureResult, value)
withFixtureControl config script =
  timeout scriptTimeoutMicroseconds (runScript config script) >>= \case
    Nothing -> assertFailure "the fixture script timed out"
    Just result -> pure result

scriptTimeoutMicroseconds :: Int
scriptTimeoutMicroseconds = 10 * 1000 * 1000

runScript :: FixtureConfig -> (FixtureControl -> Mock -> IO value) -> IO (FixtureResult, value)
runScript config script = do
  control <- newFixtureControl
  readies <- newIORef []
  (outcomeVar, value) <- withRuntimePair $ \target peer -> do
    outcomeVar <- startFixture control config target readies
    mock <- newMock peer
    value <- script control mock
    pure (outcomeVar, value)
  outcome <- expectMockSuccess outcomeVar
  readyValues <- reverse <$> readIORef readies
  pure (FixtureResult outcome readyValues, value)

startFixture ::
  FixtureControl ->
  FixtureConfig ->
  ConnectionTarget ->
  IORef [FixtureReady] ->
  IO (MVar (Either SomeException (Either FixtureError FixtureStop)))
startFixture control config target readies =
  runMock (runLayerSurfaceFixture control config target (\ready -> modifyIORef' readies (ready :)))

data Mock = Mock
  { mockPeer :: Transport
  , mockBytes :: IORef ByteString
  , mockFds :: IORef [OwnedFd]
  }

newMock :: Transport -> IO Mock
newMock peer = Mock peer <$> newIORef ByteString.empty <*> newIORef []

receiveRequest :: Mock -> IO RawFrame
receiveRequest mock = do
  bytes <- readIORef (mockBytes mock)
  case decodeFrame bytes of
    Right (DecodedFrame frame suffix) -> writeIORef (mockBytes mock) suffix >> pure frame
    Right (NeedMoreBytes needed) ->
      receiveChunk (mockPeer mock) (max 8 needed) >>= \case
        Right (Just chunk) -> do
          writeIORef (mockBytes mock) (bytes <> receivedBytes chunk)
          modifyIORef' (mockFds mock) (<> receivedFds chunk)
          receiveRequest mock
        Right Nothing -> assertFailure "fixture closed the connection while a request was expected"
        Left errorValue -> assertFailure ("unexpected mock receive error: " <> show errorValue)
    Left errorValue -> assertFailure ("unexpected frame error: " <> show errorValue)

expectEndOfRequests :: Mock -> Assertion
expectEndOfRequests mock = do
  bytes <- readIORef (mockBytes mock)
  assertEqual "no buffered request bytes" ByteString.empty bytes
  receiveChunk (mockPeer mock) 8 >>= \case
    Right Nothing -> pure ()
    Right (Just chunk) -> assertFailure ("unexpected request bytes: " <> show (receivedBytes chunk))
    Left errorValue -> assertFailure ("unexpected mock receive error: " <> show errorValue)

answerStartup :: Mock -> [(Word32, Text, Word32)] -> IO ObjectId
answerStartup mock globals = do
  registryFrame <- receiveRequest mock
  callbackFrame <- receiveRequest mock
  assertEqual "get_registry opcode" (Opcode 1) (rawFrameOpcode registryFrame)
  assertEqual "sync opcode" (Opcode 0) (rawFrameOpcode callbackFrame)
  registryId <- decodeNewId registryFrame
  callbackId <- decodeNewId callbackFrame
  sendFrames
    (mockPeer mock)
    ( [registryGlobalFrame registryId name interface version | (name, interface, version) <- globals]
        <> [callbackDoneFrame callbackId 1]
    )
  pure registryId

takePassedFd :: Mock -> IO OwnedFd
takePassedFd mock =
  readIORef (mockFds mock) >>= \case
    descriptor : remaining -> writeIORef (mockFds mock) remaining >> pure descriptor
    [] -> assertFailure "expected a passed descriptor"

receiveNewIdRequestWithArguments :: Mock -> ObjectId -> Opcode -> IO (ObjectId, RawFrame)
receiveNewIdRequestWithArguments mock sender opcode = do
  frame <- receiveRequest mock
  assertEqual "new-ID request sender" sender (rawFrameSender frame)
  assertEqual "new-ID request opcode" opcode (rawFrameOpcode frame)
  identifier <- decodeLeadingNewId frame
  pure (identifier, frame)

expectRequestArguments :: String -> (ObjectId, RawFrame) -> ArgumentEncoder () () -> Assertion
expectRequestArguments label (identifier, frame) arguments =
  expectPayload label frame (putNewId identifier >> arguments)

receiveNewIdRequest :: Mock -> ObjectId -> Opcode -> IO ObjectId
receiveNewIdRequest mock sender opcode = do
  frame <- receiveRequest mock
  assertEqual "new-ID request sender" sender (rawFrameSender frame)
  assertEqual "new-ID request opcode" opcode (rawFrameOpcode frame)
  decodeNewId frame

decodeLeadingNewId :: RawFrame -> IO ObjectId
decodeLeadingNewId frame = case runArgumentDecoder getNewId (rawFramePayload frame) ([] :: [()]) of
  Done identifier _ _ -> pure identifier
  other -> assertFailure ("missing leading new_id: " <> show other)

expectRequest :: Mock -> String -> ObjectId -> Opcode -> ArgumentEncoder () () -> Assertion
expectRequest mock label sender opcode arguments = do
  frame <- receiveRequest mock
  assertEqual (label <> " sender") sender (rawFrameSender frame)
  assertEqual (label <> " opcode") opcode (rawFrameOpcode frame)
  expectPayload label frame arguments

expectPayload :: String -> RawFrame -> ArgumentEncoder () () -> Assertion
expectPayload label frame arguments = do
  payload <- encodedPayload arguments
  assertEqual (label <> " payload") payload (rawFramePayload frame)

expectBind :: Mock -> ObjectId -> Word32 -> Text -> IO ObjectId
expectBind mock registryId expectedName expectedInterface = do
  frame <- receiveRequest mock
  assertEqual (label <> " bind sender") registryId (rawFrameSender frame)
  assertEqual (label <> " bind opcode") (Opcode 0) (rawFrameOpcode frame)
  case runCompleteArgumentDecoder ((,) <$> getUint <*> getDynamicNewId) (rawFramePayload frame) ([] :: [()]) of
    Done (name, dynamicId) remaining []
      | ByteString.null remaining -> do
          assertEqual (label <> " global name") expectedName name
          assertEqual (label <> " interface") expectedInterface (dynamicNewIdInterface dynamicId)
          assertEqual (label <> " version") 1 (dynamicNewIdVersion dynamicId)
          pure (dynamicNewIdObject dynamicId)
    other -> assertFailure ("unexpected " <> label <> " bind payload: " <> show other)
 where
  label = show expectedInterface
