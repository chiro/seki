{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.StartupTest (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, onException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Word (Word32)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Run.Startup (
  WaylandStartupError (..),
  closeWaylandSession,
  openWaylandSessionAt,
  readWaylandGlobalCatalog,
  waylandConnection,
  waylandDisplay,
  waylandRegistry,
 )
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  getNewId,
  putString,
  putUint,
  runArgumentEncoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Codec (decodeFrame, encodeFrame)
import Seki.Wire.Connection (DispatchResult (..), dispatchNextEvent)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry (
  GlobalAnnouncement (..),
  RegistryError (..),
  announcedGlobals,
  globalNameFromWord,
 )
import Seki.Wire.Transport (
  ConnectionTarget (InheritedSocket),
  ReceivedChunk (..),
  Transport,
  closeTransport,
  openTransport,
  receiveChunk,
  sendMessage,
 )
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..), objectIdValue)
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland startup"
    [ testCase "synchronizes the initial registry and retains live updates" testRegistrySync
    , testCase "rejects an invalid registry announcement" testInvalidRegistryEvent
    , testCase "rejects EOF before the sync barrier" testEofBeforeSync
    ]

testRegistrySync :: Assertion
testRegistrySync =
  withStartupPair $ \target peer -> do
    mockDone <- runMock $ do
      (registryId, callbackId) <- receiveStartupRequests peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 3 "wl_output" 4
        , callbackDoneFrame callbackId 91
        ]
    session <- expectRight =<< openWaylandSessionAt target defaultObjectLimit
    expectMockSuccess mockDone
    assertEqual "display ID" 1 (objectIdValue (objectId (waylandDisplay session)))
    assertEqual "registry ID" 2 (objectIdValue (objectId (waylandRegistry session)))
    catalog <- expectRight =<< readWaylandGlobalCatalog session
    assertEqual
      "initial registry snapshot"
      [ GlobalAnnouncement (globalNameFromWord 3) "wl_output" 4
      , GlobalAnnouncement (globalNameFromWord 7) "river_window_manager_v1" 4
      ]
      (announcedGlobals catalog)

    sendFrames peer [registryGlobalFrame (objectId (waylandRegistry session)) 9 "wl_seat" 8]
    expectDispatched =<< dispatchNextEvent (waylandConnection session)
    updated <- expectRight =<< readWaylandGlobalCatalog session
    assertEqual
      "live catalog update"
      [ GlobalAnnouncement (globalNameFromWord 3) "wl_output" 4
      , GlobalAnnouncement (globalNameFromWord 7) "river_window_manager_v1" 4
      , GlobalAnnouncement (globalNameFromWord 9) "wl_seat" 8
      ]
      (announcedGlobals updated)
    closeWaylandSession session
    closeWaylandSession session

testInvalidRegistryEvent :: Assertion
testInvalidRegistryEvent =
  withStartupPair $ \target peer -> do
    mockDone <- runMock $ do
      (registryId, callbackId) <- receiveStartupRequests peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 5 "broken" 0
        , callbackDoneFrame callbackId 1
        ]
    let expected =
          WaylandRegistrySyncError
            (InvalidAnnouncedGlobalVersion (globalNameFromWord 5) "broken" 0)
    expectStartupLeft expected =<< openWaylandSessionAt target defaultObjectLimit
    expectMockSuccess mockDone

testEofBeforeSync :: Assertion
testEofBeforeSync =
  withStartupPair $ \target peer -> do
    mockDone <- runMock $ do
      _ <- receiveStartupRequests peer
      closeTransport peer
    expectStartupLeft WaylandRegistrySyncFinished =<< openWaylandSessionAt target defaultObjectLimit
    expectMockSuccess mockDone

receiveStartupRequests :: Transport -> IO (ObjectId, ObjectId)
receiveStartupRequests peer = do
  bytes <- receiveBytes peer 24
  frames <- expectFrames 2 bytes
  case frames of
    [registryFrame, callbackFrame] -> do
      assertEqual "get_registry sender" 1 (objectIdValue (rawFrameSender registryFrame))
      assertEqual "get_registry opcode" (Opcode 1) (rawFrameOpcode registryFrame)
      assertEqual "sync sender" 1 (objectIdValue (rawFrameSender callbackFrame))
      assertEqual "sync opcode" (Opcode 0) (rawFrameOpcode callbackFrame)
      registryId <- decodeNewId registryFrame
      callbackId <- decodeNewId callbackFrame
      pure (registryId, callbackId)
    _ -> assertFailure "unexpected startup request count"

decodeNewId :: RawFrame -> IO ObjectId
decodeNewId frame = case runCompleteArgumentDecoder getNewId (rawFramePayload frame) ([] :: [()]) of
  Done identifier remaining []
    | ByteString.null remaining -> pure identifier
  result -> assertFailure ("unexpected new_id payload: " <> show result)

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

receiveBytes :: Transport -> Int -> IO ByteString
receiveBytes peer expected = go ByteString.empty
 where
  go bytes
    | ByteString.length bytes >= expected = pure bytes
    | otherwise = do
        receiveChunk peer (expected - ByteString.length bytes) >>= \case
          Right (Just chunk)
            | null (receivedFds chunk) -> go (bytes <> receivedBytes chunk)
            | otherwise -> assertFailure "unexpected descriptors"
          _ -> assertFailure "unexpected mock receive result"

expectFrames :: Int -> ByteString -> IO [RawFrame]
expectFrames expected = go []
 where
  go frames bytes
    | length frames == expected && ByteString.null bytes = pure (reverse frames)
    | length frames == expected = assertFailure "unexpected trailing startup bytes"
    | otherwise = case decodeFrame bytes of
        Right (DecodedFrame frame suffix) -> go (frame : frames) suffix
        result -> assertFailure ("unexpected startup frame: " <> show result)

expectDispatched :: Either errorValue DispatchResult -> IO ()
expectDispatched result = case result of
  Right (EventDispatched _) -> pure ()
  _ -> assertFailure "expected dispatched event"

runMock :: IO () -> IO (MVar (Either SomeException ()))
runMock action = do
  result <- newEmptyMVar
  void (forkIO (try action >>= putMVar result))
  pure result

expectMockSuccess :: MVar (Either SomeException ()) -> IO ()
expectMockSuccess result =
  takeMVar result >>= either (assertFailure . ("mock compositor failed: " <>) . show) pure

withStartupPair :: (ConnectionTarget -> Transport -> IO value) -> IO value
withStartupPair action =
  bracket openStartupPair (closeTransport . snd) (uncurry action)

openStartupPair :: IO (ConnectionTarget, Transport)
openStartupPair = do
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

expectStartupLeft :: WaylandStartupError -> Either WaylandStartupError session -> Assertion
expectStartupLeft expected result = case result of
  Left actual -> assertEqual "startup error" expected actual
  Right _ -> assertFailure "expected startup error"
