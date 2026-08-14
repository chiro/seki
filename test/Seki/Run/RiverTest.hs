{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.RiverTest (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, onException)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Word (Word32)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.River (
  RiverBootstrapError (..),
  RiverWindowManagerHandlers (..),
  bindRiverWindowManager,
  riverWindowManagerRequiredVersion,
  selectRiverWindowManagerGlobal,
 )
import Seki.Run.Xkb (
  RiverXkbBootstrapError (..),
  bindRiverXkbBindings,
  riverXkbBindingsRequiredVersion,
  selectRiverXkbBindingsGlobal,
 )
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  getUint,
  putInt,
  putNewId,
  putString,
  runArgumentEncoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Binding (DynamicNewId (..), getDynamicNewId)
import Seki.Wire.Codec (decodeFrame, encodeFrame)
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  DispatchResult (..),
  closeConnection,
  dispatchNextEvent,
  newConnection,
  submitNewObjectRequest,
 )
import Seki.Wire.Connection.ObjectTable (Object, defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry (
  GlobalCatalog,
  applyRegistryEvent,
  emptyGlobalCatalog,
  globalNameFromWord,
  globalVersion,
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
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..), mkObjectId)
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river bootstrap"
    [ testCase "selects the unique manager at the generated maximum" testManagerSelection
    , testCase "rejects missing, ambiguous, and obsolete manager globals" testManagerSelectionErrors
    , testCase "binds and routes manager-created windows, outputs, and seats" testManagerBinding
    , testCase "selects and binds the unique XKB bindings global" testXkbBinding
    , testCase "rejects missing, ambiguous, and obsolete XKB globals" testXkbSelectionErrors
    ]

testManagerSelection :: Assertion
testManagerSelection = do
  catalog <- catalogWith [(7, "river_window_manager_v1", 9)]
  manager <- expectRight (selectRiverWindowManagerGlobal catalog)
  assertEqual "generated version cap" 4 (globalVersion manager)
  assertEqual "required feature version" 3 riverWindowManagerRequiredVersion

testManagerSelectionErrors :: Assertion
testManagerSelectionErrors = do
  expectLeft RiverWindowManagerGlobalMissing (selectRiverWindowManagerGlobal emptyGlobalCatalog)
  obsolete <- catalogWith [(4, "river_window_manager_v1", 2)]
  expectLeft
    (RiverWindowManagerVersionTooLow 3 2)
    (selectRiverWindowManagerGlobal obsolete)
  ambiguous <-
    catalogWith
      [ (8, "river_window_manager_v1", 4)
      , (3, "river_window_manager_v1", 4)
      ]
  expectLeft
    ( RiverWindowManagerGlobalsAmbiguous
        [globalNameFromWord 3, globalNameFromWord 8]
    )
    (selectRiverWindowManagerGlobal ambiguous)

testManagerBinding :: Assertion
testManagerBinding =
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
    _ <- receiveFrame peer
    catalog <- catalogWith [(21, "river_window_manager_v1", 4)]
    managerEvents <- newIORef (0 :: Int)
    createdWindow <- newEmptyMVar
    windowEvent <- newEmptyMVar
    createdOutput <- newEmptyMVar
    outputEvent <- newEmptyMVar
    createdSeat <- newEmptyMVar
    seatEvent <- newEmptyMVar
    let handlers =
          RiverWindowManagerHandlers
            { handleRiverWindowManagerEvent = \_ _ -> modifyIORef' managerEvents (+ 1)
            , handleRiverWindowCreated = putMVar createdWindow
            , handleRiverWindowEvent = \_ event -> putMVar windowEvent event
            , handleRiverOutputCreated = putMVar createdOutput
            , handleRiverOutputEvent = \_ event -> putMVar outputEvent event
            , handleRiverSeatCreated = putMVar createdSeat
            , handleRiverSeatEvent = \_ event -> putMVar seatEvent event
            }
    manager <- expectRight =<< bindRiverWindowManager connection registry catalog handlers
    bindFrame <- receiveFrame peer
    assertEqual "registry bind sender" (objectId registry) (rawFrameSender bindFrame)
    assertEqual "registry bind opcode" (Opcode 0) (rawFrameOpcode bindFrame)
    case runCompleteArgumentDecoder ((,) <$> getUint <*> getDynamicNewId) (rawFramePayload bindFrame) ([] :: [()]) of
      Done (name, dynamicId) remaining [] -> do
        assertEqual "manager global name" 21 name
        assertEqual
          "typed manager bind"
          DynamicNewId
            { dynamicNewIdInterface = "river_window_manager_v1"
            , dynamicNewIdVersion = 4
            , dynamicNewIdObject = objectId manager
            }
          dynamicId
        assertEqual "complete bind payload" ByteString.empty remaining
      result -> assertFailure ("unexpected manager bind payload: " <> show result)

    windowId <- requireObjectId 0xff000001
    outputId <- requireObjectId 0xff000002
    seatId <- requireObjectId 0xff000003
    sendEvent peer (objectId manager) (Opcode 6) (putNewId windowId)
    expectDispatched =<< dispatchNextEvent connection
    window <- takeMVar createdWindow
    assertEqual "created window ID" windowId (objectId window)
    sendEvent peer (objectId manager) (Opcode 7) (putNewId outputId)
    expectDispatched =<< dispatchNextEvent connection
    output <- takeMVar createdOutput
    assertEqual "created output ID" outputId (objectId output)
    sendEvent peer (objectId manager) (Opcode 8) (putNewId seatId)
    expectDispatched =<< dispatchNextEvent connection
    seat <- takeMVar createdSeat
    assertEqual "created seat ID" seatId (objectId seat)
    assertEqual "manager handlers" 3 =<< readIORef managerEvents

    sendEvent peer windowId (Opcode 17) (putString "window-identifier")
    expectDispatched =<< dispatchNextEvent connection
    assertEqual
      "window route"
      (River.RiverWindowV1Identifier "window-identifier")
      =<< takeMVar windowEvent
    sendEvent peer outputId (Opcode 2) (putInt (-10) >> putInt 20)
    expectDispatched =<< dispatchNextEvent connection
    assertEqual "output route" (River.RiverOutputV1Position (-10) 20) =<< takeMVar outputEvent
    sendEvent peer seatId (Opcode 8) (putInt 30 >> putInt 40)
    expectDispatched =<< dispatchNextEvent connection
    assertEqual "seat route" (River.RiverSeatV1PointerPosition 30 40) =<< takeMVar seatEvent

testXkbBinding :: Assertion
testXkbBinding =
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
    _ <- receiveFrame peer
    catalog <- catalogWith [(22, "river_xkb_bindings_v1", 9)]
    selected <- expectRight (selectRiverXkbBindingsGlobal catalog)
    assertEqual "generated version cap" 3 (globalVersion selected)
    assertEqual "required feature version" 3 riverXkbBindingsRequiredVersion

    bindings <- expectRight =<< bindRiverXkbBindings connection registry catalog
    bindFrame <- receiveFrame peer
    assertEqual "registry bind sender" (objectId registry) (rawFrameSender bindFrame)
    assertEqual "registry bind opcode" (Opcode 0) (rawFrameOpcode bindFrame)
    case runCompleteArgumentDecoder ((,) <$> getUint <*> getDynamicNewId) (rawFramePayload bindFrame) ([] :: [()]) of
      Done (name, dynamicId) remaining [] -> do
        assertEqual "XKB global name" 22 name
        assertEqual
          "typed XKB bind"
          DynamicNewId
            { dynamicNewIdInterface = "river_xkb_bindings_v1"
            , dynamicNewIdVersion = 3
            , dynamicNewIdObject = objectId bindings
            }
          dynamicId
        assertEqual "complete bind payload" ByteString.empty remaining
      result -> assertFailure ("unexpected XKB bind payload: " <> show result)

testXkbSelectionErrors :: Assertion
testXkbSelectionErrors = do
  expectLeft RiverXkbBindingsGlobalMissing (selectRiverXkbBindingsGlobal emptyGlobalCatalog)
  obsolete <- catalogWith [(4, "river_xkb_bindings_v1", 2)]
  expectLeft
    (RiverXkbBindingsVersionTooLow 3 2)
    (selectRiverXkbBindingsGlobal obsolete)
  ambiguous <-
    catalogWith
      [ (8, "river_xkb_bindings_v1", 3)
      , (3, "river_xkb_bindings_v1", 3)
      ]
  expectLeft
    ( RiverXkbBindingsGlobalsAmbiguous
        [globalNameFromWord 3, globalNameFromWord 8]
    )
    (selectRiverXkbBindingsGlobal ambiguous)

catalogWith :: [(Word32, Text, Word32)] -> IO GlobalCatalog
catalogWith = go emptyGlobalCatalog
 where
  go catalog [] = pure catalog
  go catalog ((name, interfaceName, version) : rest) = do
    next <- expectRight (applyRegistryEvent (Wayland.WlRegistryGlobal name interfaceName version) catalog)
    go next rest

sendEvent :: Transport -> ObjectId -> Opcode -> ArgumentEncoder () () -> IO ()
sendEvent peer sender opcode arguments = do
  payload <- encodedPayload arguments
  bytes <- expectRight (encodeFrame (RawFrame sender opcode payload))
  assertEqual "send event" (Right ()) =<< sendMessage peer bytes []

encodedPayload :: ArgumentEncoder () () -> IO ByteString
encodedPayload encoder = case runArgumentEncoder encoder of
  Left errorValue -> assertFailure ("unexpected argument error: " <> show errorValue)
  Right ((), payload, []) -> pure payload
  Right (_, _, descriptors) -> assertFailure ("unexpected descriptors: " <> show (length descriptors))

receiveFrame :: Transport -> IO RawFrame
receiveFrame peer = go ByteString.empty
 where
  go bytes = case decodeFrame bytes of
    Right (DecodedFrame frame suffix)
      | ByteString.null suffix -> pure frame
      | otherwise -> assertFailure "unexpected trailing frame"
    Right (NeedMoreBytes needed) -> do
      received <- receiveChunk peer (max 8 needed)
      case received of
        Right (Just chunk)
          | null (receivedFds chunk) -> go (bytes <> receivedBytes chunk)
          | otherwise -> assertFailure "unexpected descriptors"
        _ -> assertFailure "unexpected receive result"
    Left errorValue -> assertFailure ("unexpected frame error: " <> show errorValue)

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

requireObjectId :: Word32 -> IO ObjectId
requireObjectId rawId = maybe (assertFailure ("invalid object ID: " <> show rawId)) pure (mkObjectId rawId)

expectTransport :: (Show errorValue) => Either errorValue value -> IO value
expectTransport = either (assertFailure . ("unexpected transport error: " <>) . show) pure

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

expectLeft :: (Eq errorValue, Show errorValue) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result = case result of
  Left actual -> assertEqual "error" expected actual
  Right _ -> assertFailure "expected error"
