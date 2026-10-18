{-# LANGUAGE OverloadedStrings #-}

module Seki.Wire.ConnectionTest (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, onException)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sort)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Arguments (DecodeResult (..), getNewId, putFd, runCompleteArgumentDecoder)
import Seki.Wire.Binding (RequestEncoder (..))
import Seki.Wire.Codec (decodeFrame)
import Seki.Wire.Connection (
  Connection,
  ConnectionError (..),
  closeConnection,
  newConnection,
  submitNewObjectRequest,
  submitRequest,
 )
import Seki.Wire.Connection.ObjectTable (Object, ObjectTableError (..), defaultObjectLimit, objectId)
import Seki.Wire.Connection.Request (RequestPreparationError (..))
import Seki.Wire.Transport (
  ConnectionTarget (InheritedSocket),
  OwnedFdError (..),
  ReceivedChunk (..),
  Transport,
  TransportError (..),
  adoptFd,
  closeOwnedFd,
  closeTransport,
  openTransport,
  receiveChunk,
  withOwnedFd,
 )
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..), objectIdValue)
import System.Posix.IO (closeFd, createPipe, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland connection submission"
    [ testCase "submits and publishes a new object" testNewObject
    , testCase "transfers request descriptors" testDescriptorTransfer
    , testCase "publishes destructor state after sending" testDestructor
    , testCase "keeps preparation failures non-fatal" testPreparationFailure
    , testCase "makes transport failures terminal" testTransportFailure
    , testCase "serializes concurrent child allocation and frames" testConcurrentChildren
    , testCase "rejects requests after explicit close" testExplicitClose
    ]

testNewObject :: Assertion
testNewObject =
  withConnectionPair $ \connection display receiver -> do
    callback <- expectRight =<< submitSync connection display
    frame <- receiveOneFrame receiver 12
    assertEqual "sender" (objectId display) (rawFrameSender frame)
    assertEqual "opcode" (Opcode 0) (rawFrameOpcode frame)
    assertEqual "callback payload" (Right (objectId callback)) (decodeNewId frame)

testDescriptorTransfer :: Assertion
testDescriptorTransfer =
  withPipe $ \readDescriptor _ ->
    withConnectionPair $ \connection display receiver -> do
      owned <- dup readDescriptor >>= adoptFd
      assertEqual
        "submission"
        (Right ())
        =<< submitRequest
          connection
          Wayland.wlDisplayInterface
          display
          (RequestEncoder "descriptor" (Opcode 2) 1 False (putFd owned))
      assertEqual "sender ownership transferred" (Left OwnedFdAlreadyReleased) =<< withOwnedFd owned pure
      received <- receiveChunk receiver 1024
      case received of
        Right (Just chunk) -> case receivedFds chunk of
          [descriptor] -> void (closeOwnedFd descriptor)
          descriptors -> assertFailure ("unexpected descriptor count: " <> show (length descriptors))
        other -> assertFailure ("unexpected receive result: " <> showReceiveResult other)

testDestructor :: Assertion
testDestructor =
  withConnectionPair $ \connection display receiver -> do
    callback <- expectRight =<< submitSync connection display
    void (receiveOneFrame receiver 12)
    assertEqual
      "destructor"
      (Right ())
      =<< submitRequest
        connection
        Wayland.wlCallbackInterface
        callback
        (RequestEncoder "destroy" (Opcode 0) 1 True (pure ()))
    void (receiveOneFrame receiver 8)
    assertEqual
      "retired object rejected"
      ( Left
          ( ConnectionRequestError
              (RequestObjectError (ObjectNotActive (objectId callback)))
          )
      )
      =<< submitRequest
        connection
        Wayland.wlCallbackInterface
        callback
        (RequestEncoder "again" (Opcode 0) 1 False (pure ()))

testPreparationFailure :: Assertion
testPreparationFailure =
  withConnectionPair $ \connection display receiver -> do
    assertEqual
      "unsupported request"
      (Left (ConnectionRequestError (UnsupportedRequestVersion "wl_display" "future" 2 1)))
      =<< submitRequest
        connection
        Wayland.wlDisplayInterface
        display
        (RequestEncoder "future" (Opcode 2) 2 False (pure ()))
    callback <- expectRight =<< submitSync connection display
    frame <- receiveOneFrame receiver 12
    assertEqual "connection remains usable" (Right (objectId callback)) (decodeNewId frame)

testTransportFailure :: Assertion
testTransportFailure =
  withPipe $ \readDescriptor _ ->
    withConnectionPair $ \connection display _ -> do
      released <- dup readDescriptor >>= adoptFd
      void (closeOwnedFd released)
      let terminalError = Left (ConnectionTransportError TransportDescriptorAlreadyReleased)
      assertEqual
        "transport failure"
        terminalError
        =<< submitRequest
          connection
          Wayland.wlDisplayInterface
          display
          (RequestEncoder "descriptor" (Opcode 2) 1 False (putFd released))
      assertEqual "same terminal failure" terminalError . void =<< submitSync connection display

testConcurrentChildren :: Assertion
testConcurrentChildren =
  withConnectionPair $ \connection display receiver -> do
    firstResult <- newEmptyMVar
    secondResult <- newEmptyMVar
    void (forkIO (submitSync connection display >>= putMVar firstResult))
    void (forkIO (submitSync connection display >>= putMVar secondResult))
    first <- expectRight =<< takeMVar firstResult
    second <- expectRight =<< takeMVar secondResult
    bytes <- receiveBytes receiver 24
    frames <- expectFrames bytes
    decodedIds <- traverse (either (assertFailure . show) pure . decodeNewId) frames
    assertEqual "unique allocated IDs" [2, 3] (sort (map (objectIdValue . objectId) [first, second]))
    assertEqual "complete ordered frames" [2, 3] (sort (map objectIdValue decodedIds))

testExplicitClose :: Assertion
testExplicitClose =
  withConnectionPair $ \connection display _ -> do
    closeConnection connection
    closeConnection connection
    assertEqual "closed" (Left ConnectionClosed) =<< submitSync connection display

submitSync :: Connection -> Object Wayland.WlDisplay -> IO (Either ConnectionError (Object Wayland.WlCallback))
submitSync connection display =
  submitNewObjectRequest
    connection
    Wayland.wlDisplayInterface
    display
    Wayland.wlCallbackInterface
    1
    (Wayland.encodeWlDisplaySync . objectId)

decodeNewId :: RawFrame -> Either String ObjectId
decodeNewId frame = case runCompleteArgumentDecoder getNewId (rawFramePayload frame) ([] :: [()]) of
  Done callback _ [] -> Right callback
  result -> Left ("unexpected callback payload: " <> show result)

receiveOneFrame :: Transport -> Int -> IO RawFrame
receiveOneFrame receiver frameSize = do
  bytes <- receiveBytes receiver frameSize
  case decodeFrame bytes of
    Right (DecodedFrame frame suffix)
      | ByteString.null suffix -> pure frame
    result -> assertFailure ("unexpected frame: " <> show result)

receiveBytes :: Transport -> Int -> IO ByteString
receiveBytes receiver expected = go ByteString.empty
 where
  go accumulated
    | ByteString.length accumulated >= expected = pure accumulated
    | otherwise = do
        received <- receiveChunk receiver (expected - ByteString.length accumulated)
        case received of
          Right (Just chunk)
            | null (receivedFds chunk) -> go (accumulated <> receivedBytes chunk)
            | otherwise -> assertFailure "unexpected descriptors"
          other -> assertFailure ("unexpected receive result: " <> showReceiveResult other)

expectFrames :: ByteString -> IO [RawFrame]
expectFrames bytes = case decodeFrame bytes of
  Right (DecodedFrame firstFrame remaining) -> case decodeFrame remaining of
    Right (DecodedFrame secondFrame suffix)
      | ByteString.null suffix -> pure [firstFrame, secondFrame]
    result -> assertFailure ("unexpected second frame: " <> show result)
  result -> assertFailure ("unexpected first frame: " <> show result)

withConnectionPair :: (Connection -> Object Wayland.WlDisplay -> Transport -> IO value) -> IO value
withConnectionPair action =
  bracket openConnectionPair closeConnectionPair $ \(connection, display, receiver) ->
    action connection display receiver

openConnectionPair :: IO (Connection, Object Wayland.WlDisplay, Transport)
openConnectionPair = do
  (firstSocket, secondSocket) <- socketPair AF_UNIX Stream defaultProtocol
  firstDescriptor <- duplicateSocket firstSocket
  secondDescriptor <- duplicateSocket secondSocket `onException` closeFd firstDescriptor
  close firstSocket
  close secondSocket
  sender <- expectTransport =<< openTransport (InheritedSocket firstDescriptor)
  receiver <- (expectTransport =<< openTransport (InheritedSocket secondDescriptor)) `onException` closeTransport sender
  (connection, display) <-
    (expectRight =<< newConnection sender defaultObjectLimit Wayland.wlDisplayInterface)
      `onException` (closeTransport sender >> closeTransport receiver)
  pure (connection, display, receiver)

closeConnectionPair :: (Connection, Object Wayland.WlDisplay, Transport) -> IO ()
closeConnectionPair (connection, _, receiver) = do
  closeConnection connection
  closeTransport receiver

duplicateSocket :: Socket -> IO Fd
duplicateSocket connection = withFdSocket connection (dup . Fd)

expectTransport :: Either errorValue value -> IO value
expectTransport = either (const (assertFailure "unexpected transport error")) pure

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

withPipe :: (Fd -> Fd -> IO value) -> IO value
withPipe action = bracket createPipe closePipe (uncurry action)

closePipe :: (Fd, Fd) -> IO ()
closePipe (readDescriptor, writeDescriptor) = do
  closeFd readDescriptor
  closeFd writeDescriptor

showReceiveResult :: Either errorValue (Maybe ReceivedChunk) -> String
showReceiveResult result = case result of
  Left _ -> "error"
  Right Nothing -> "EOF"
  Right (Just chunk) ->
    "chunk with " <> show (ByteString.length (receivedBytes chunk)) <> " bytes and " <> show (length (receivedFds chunk)) <> " descriptors"
