{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Wire.Connection.DispatchTest (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, onException)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Arguments (ArgumentEncoder, getFd, putNewId, putObject, putString, putUint, runArgumentEncoder)
import Seki.Wire.Binding (EventDecoder (..), Interface (..), RequestEncoder (..))
import Seki.Wire.Codec (encodeFrame)
import Seki.Wire.Connection (
  Connection,
  ConnectionError (..),
  DispatchResult (..),
  DispatchedEvent (..),
  closeConnection,
  dispatchNextEvent,
  eventRoute,
  newConnection,
  registerEventRoute,
  submitNewObjectRequest,
  submitRequest,
  wlDisplayEventRoute,
 )
import Seki.Wire.Connection.ObjectTable (Object, ObjectTableError (..), defaultObjectLimit, objectId)
import Seki.Wire.Connection.Request (RequestPreparationError (..))
import Seki.Wire.Transport (
  ConnectionTarget (InheritedSocket),
  OwnedFd,
  ReceivedChunk (..),
  Transport,
  adoptFd,
  closeOwnedFd,
  closeTransport,
  openTransport,
  receiveChunk,
  sendMessage,
  withOwnedFd,
 )
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..), objectIdValue)
import System.Posix.IO (closeFd, createPipe, dup)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

data DescriptorTarget

tests :: TestTree
tests =
  testGroup
    "Wayland connection dispatch"
    [ testCase "dispatches a typed callback and retires its sender" testCallbackDispatch
    , testCase "handles delete_id and reuses the released ID" testDeleteIdDispatch
    , testCase "makes wl_display.error terminal" testDisplayError
    , testCase "rejects an event without a registered route" testMissingRoute
    , testCase "drains coalesced events in stream order" testCoalescedEvents
    , testCase "transfers an incoming descriptor to its handler" testIncomingDescriptor
    , testCase "allows a handler to submit a follow-up request" testHandlerSubmission
    , testCase "reports clean EOF idempotently" testCleanEof
    ]

testCallbackDispatch :: Assertion
testCallbackDispatch =
  withConnectionPair $ \connection display peer -> do
    callback <- createCallback connection display peer
    handled <- newEmptyMVar
    assertEqual
      "register callback"
      (Right ())
      =<< registerEventRoute
        connection
        Wayland.wlCallbackInterface
        callback
        (eventRoute Wayland.decodeWlCallbackEvent (\_ event -> putMVar handled event))
    sendEvent peer (objectId callback) (Opcode 0) (putUint 41) []
    assertEqual
      "dispatch metadata"
      (Right (EventDispatched (DispatchedEvent (objectId callback) "wl_callback" "done" (Opcode 0))))
      =<< dispatchNextEvent connection
    assertEqual "typed callback" (Wayland.WlCallbackDone 41) =<< takeMVar handled
    assertEqual
      "destructor event retired callback"
      (Left (ConnectionRequestError (RequestObjectError (ObjectNotActive (objectId callback)))))
      =<< submitRequest
        connection
        Wayland.wlCallbackInterface
        callback
        (RequestEncoder "invalid" (Opcode 0) 1 False (pure ()))

testDeleteIdDispatch :: Assertion
testDeleteIdDispatch =
  withConnectionPair $ \connection display peer -> do
    displayEvent <- newEmptyMVar
    assertEqual
      "register display"
      (Right ())
      =<< registerEventRoute connection Wayland.wlDisplayInterface display (wlDisplayEventRoute (\_ event -> putMVar displayEvent event))
    callback <- createCallback connection display peer
    assertEqual
      "register callback"
      (Right ())
      =<< registerEventRoute connection Wayland.wlCallbackInterface callback (eventRoute Wayland.decodeWlCallbackEvent (\_ _ -> pure ()))
    sendEvent peer (objectId callback) (Opcode 0) (putUint 0) []
    expectDispatched =<< dispatchNextEvent connection
    let deletedId = objectIdValue (objectId callback)
    sendEvent peer (objectId display) (Opcode 1) (putUint deletedId) []
    expectDispatched =<< dispatchNextEvent connection
    assertEqual "typed delete_id" (Wayland.WlDisplayDeleteId deletedId) =<< takeMVar displayEvent
    reused <- createCallback connection display peer
    assertEqual "released ID reused" (objectId callback) (objectId reused)

testDisplayError :: Assertion
testDisplayError =
  withConnectionPair $ \connection display peer -> do
    handlerCalled <- newIORef False
    assertEqual
      "register display"
      (Right ())
      =<< registerEventRoute connection Wayland.wlDisplayInterface display (wlDisplayEventRoute (\_ _ -> writeIORef handlerCalled True))
    sendEvent peer (objectId display) (Opcode 0) (putObject (objectId display) >> putUint 3 >> putString "fatal") []
    let expected = ConnectionDisplayError (objectId display) 3 (Just "wl_display") "fatal"
    assertEqual "fatal display error" (Left expected) =<< dispatchNextEvent connection
    assertEqual "handler not invoked" False =<< readIORef handlerCalled
    assertEqual
      "display failure is terminal"
      (Left expected)
      =<< submitRequest
        connection
        Wayland.wlDisplayInterface
        display
        (RequestEncoder "after_error" (Opcode 0) 1 False (pure ()))

testMissingRoute :: Assertion
testMissingRoute =
  withConnectionPair $ \connection display peer -> do
    callback <- createCallback connection display peer
    sendEvent peer (objectId callback) (Opcode 0) (putUint 0) []
    let expected = ConnectionEventRouteMissing (objectId callback)
    assertEqual "missing route" (Left expected) =<< dispatchNextEvent connection
    assertEqual
      "failure is terminal"
      (Left expected)
      =<< submitRequest
        connection
        Wayland.wlDisplayInterface
        display
        (RequestEncoder "after_failure" (Opcode 0) 1 False (pure ()))

testCoalescedEvents :: Assertion
testCoalescedEvents =
  withConnectionPair $ \connection display peer -> do
    registry <-
      expectRight
        =<< submitNewObjectRequest
          connection
          Wayland.wlDisplayInterface
          display
          Wayland.wlRegistryInterface
          1
          (Wayland.encodeWlDisplayGetRegistry . objectId)
    drainClientFrame peer 12
    handled <- newIORef []
    assertEqual
      "register registry"
      (Right ())
      =<< registerEventRoute
        connection
        Wayland.wlRegistryInterface
        registry
        (eventRoute Wayland.decodeWlRegistryEvent (\_ event -> modifyIORef' handled (<> [event])))
    first <- encodedFrame (objectId registry) (Opcode 0) (putUint 7 >> putString "first" >> putUint 1)
    second <- encodedFrame (objectId registry) (Opcode 1) (putUint 7)
    assertEqual "send coalesced events" (Right ()) =<< sendMessage peer (first <> second) []
    expectDispatched =<< dispatchNextEvent connection
    expectDispatched =<< dispatchNextEvent connection
    assertEqual
      "handler order"
      [Wayland.WlRegistryGlobal 7 "first" 1, Wayland.WlRegistryGlobalRemove 7]
      =<< readIORef handled

testIncomingDescriptor :: Assertion
testIncomingDescriptor =
  withPipe $ \readDescriptor writeDescriptor ->
    withConnectionPair $ \connection display peer -> do
      target <-
        expectRight
          =<< submitNewObjectRequest
            connection
            Wayland.wlDisplayInterface
            display
            descriptorTargetInterface
            1
            (RequestEncoder "create_target" (Opcode 1) 1 False . putNewId . objectId)
      drainClientFrame peer 12
      handled <- newEmptyMVar
      assertEqual
        "register descriptor target"
        (Right ())
        =<< registerEventRoute
          connection
          descriptorTargetInterface
          target
          ( eventRoute
              decodeDescriptorEvent
              ( \_ descriptor -> do
                  value <- withOwnedFd descriptor (`fdRead` 1)
                  void (closeOwnedFd descriptor)
                  putMVar handled value
              )
          )
      void (fdWrite writeDescriptor (ByteString.singleton 0x5a))
      outgoing <- dup readDescriptor >>= adoptFd
      sendEvent peer (objectId target) (Opcode 0) (pure ()) [outgoing]
      expectDispatched =<< dispatchNextEvent connection
      assertEqual "descriptor contents" (Right (ByteString.singleton 0x5a)) =<< takeMVar handled

testHandlerSubmission :: Assertion
testHandlerSubmission =
  withConnectionPair $ \connection display peer -> do
    callback <- createCallback connection display peer
    submitted <- newEmptyMVar
    assertEqual
      "register callback"
      (Right ())
      =<< registerEventRoute
        connection
        Wayland.wlCallbackInterface
        callback
        ( eventRoute
            Wayland.decodeWlCallbackEvent
            ( \_ _ ->
                submitRequest
                  connection
                  Wayland.wlDisplayInterface
                  display
                  (RequestEncoder "follow_up" (Opcode 0) 1 False (pure ()))
                  >>= putMVar submitted
            )
        )
    sendEvent peer (objectId callback) (Opcode 0) (putUint 0) []
    expectDispatched =<< dispatchNextEvent connection
    assertEqual "handler submission" (Right ()) =<< takeMVar submitted
    drainClientFrame peer 8

testCleanEof :: Assertion
testCleanEof =
  withConnectionPair $ \connection display peer -> do
    closeTransport peer
    assertEqual "first EOF" (Right DispatchFinished) =<< dispatchNextEvent connection
    assertEqual "repeated EOF" (Right DispatchFinished) =<< dispatchNextEvent connection
    assertEqual
      "requests rejected"
      (Left ConnectionClosed)
      =<< submitRequest
        connection
        Wayland.wlDisplayInterface
        display
        (RequestEncoder "after_eof" (Opcode 0) 1 False (pure ()))

createCallback :: Connection -> Object Wayland.WlDisplay -> Transport -> IO (Object Wayland.WlCallback)
createCallback connection display peer = do
  callback <-
    expectRight
      =<< submitNewObjectRequest
        connection
        Wayland.wlDisplayInterface
        display
        Wayland.wlCallbackInterface
        1
        (Wayland.encodeWlDisplaySync . objectId)
  drainClientFrame peer 12
  pure callback

descriptorTargetInterface :: Interface DescriptorTarget
descriptorTargetInterface = Interface "descriptor_target" 1

decodeDescriptorEvent :: Opcode -> Maybe (EventDecoder OwnedFd OwnedFd)
decodeDescriptorEvent (Opcode 0) = Just (EventDecoder "descriptor" 1 False getFd)
decodeDescriptorEvent _ = Nothing

sendEvent :: Transport -> ObjectId -> Opcode -> ArgumentEncoder OwnedFd () -> [OwnedFd] -> IO ()
sendEvent peer sender opcode arguments descriptors = do
  bytes <- encodedFrame sender opcode arguments
  assertEqual "send event" (Right ()) =<< sendMessage peer bytes descriptors

encodedFrame :: ObjectId -> Opcode -> ArgumentEncoder fd () -> IO ByteString
encodedFrame sender opcode arguments = do
  payload <- case runArgumentEncoder arguments of
    Left errorValue -> assertFailure ("unexpected argument error: " <> show errorValue)
    Right ((), bytes, encodedDescriptors)
      | null encodedDescriptors -> pure bytes
      | otherwise -> assertFailure "encodedFrame expects descriptors separately"
  expectRight (encodeFrame (RawFrame sender opcode payload))

drainClientFrame :: Transport -> Int -> IO ()
drainClientFrame peer expectedBytes = go 0
 where
  go byteCount
    | byteCount >= expectedBytes = pure ()
    | otherwise = do
        received <- receiveChunk peer (expectedBytes - byteCount)
        case received of
          Right (Just chunk)
            | null (receivedFds chunk) ->
                go (byteCount + ByteString.length (receivedBytes chunk))
            | otherwise -> assertFailure "unexpected client descriptors"
          _ -> assertFailure "unexpected client receive result"

expectDispatched :: Either ConnectionError DispatchResult -> IO ()
expectDispatched result = case result of
  Right (EventDispatched _) -> pure ()
  other -> assertFailure ("expected dispatched event, got: " <> show other)

withConnectionPair :: (Connection -> Object Wayland.WlDisplay -> Transport -> IO value) -> IO value
withConnectionPair action =
  bracket openConnectionPair closeConnectionPair $ \(connection, display, peer) ->
    action connection display peer

openConnectionPair :: IO (Connection, Object Wayland.WlDisplay, Transport)
openConnectionPair = do
  (firstSocket, secondSocket) <- socketPair AF_UNIX Stream defaultProtocol
  firstDescriptor <- duplicateSocket firstSocket
  secondDescriptor <- duplicateSocket secondSocket `onException` closeFd firstDescriptor
  close firstSocket
  close secondSocket
  client <- expectTransport =<< openTransport (InheritedSocket firstDescriptor)
  peer <- (expectTransport =<< openTransport (InheritedSocket secondDescriptor)) `onException` closeTransport client
  (connection, display) <-
    (expectRight =<< newConnection client defaultObjectLimit Wayland.wlDisplayInterface)
      `onException` (closeTransport client >> closeTransport peer)
  pure (connection, display, peer)

closeConnectionPair :: (Connection, Object Wayland.WlDisplay, Transport) -> IO ()
closeConnectionPair (connection, _, peer) = do
  closeConnection connection
  closeTransport peer

duplicateSocket :: Socket -> IO Fd
duplicateSocket connection = withFdSocket connection (dup . Fd)

expectTransport :: (Show errorValue) => Either errorValue value -> IO value
expectTransport = either (assertFailure . ("unexpected transport error: " <>) . show) pure

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

withPipe :: (Fd -> Fd -> IO value) -> IO value
withPipe action = bracket createPipe closePipe (uncurry action)

closePipe :: (Fd, Fd) -> IO ()
closePipe (readDescriptor, writeDescriptor) = do
  closeFd readDescriptor
  closeFd writeDescriptor
