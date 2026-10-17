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
  encodedPayload,
  expectMockSuccess,
  registryGlobalFrame,
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
import Seki.Wire.Transport (ConnectionTarget, OwnedFd, ReceivedChunk (..), Transport, receiveChunk)
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..))
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
  (result, _) <- withFixture FixtureConfig{fixtureHeight = 40} expectSurfaceSetup
  assertStoppedByConnection result

expectSurfaceSetup :: Mock -> IO ()
expectSurfaceSetup mock = do
  registryId <- answerStartup mock standardGlobals
  compositorId <- expectBind mock registryId 1 "wl_compositor"
  _ <- expectBind mock registryId 2 "wl_shm"
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
  expectRequest mock "set_size" layerSurfaceId (Opcode 0) (putUint 0 >> putUint 40)
  expectRequest mock "set_anchor" layerSurfaceId (Opcode 1) (putUint (1 + 4 + 8))
  expectRequest mock "set_exclusive_zone" layerSurfaceId (Opcode 2) (putInt 40)
  expectRequest mock "initial commit" surfaceId (Opcode 6) (pure ())

defaultConfig :: FixtureConfig
defaultConfig = FixtureConfig{fixtureHeight = 32}

standardGlobals :: [(Word32, Text, Word32)]
standardGlobals =
  [ (1, "wl_compositor", 6)
  , (2, "wl_shm", 2)
  , (3, "zwlr_layer_shell_v1", 5)
  , (4, "wl_seat", 9)
  ]

newtype FixtureResult = FixtureResult
  { fixtureOutcome :: Either FixtureError FixtureStop
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
  pure (FixtureResult outcome, value)

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
