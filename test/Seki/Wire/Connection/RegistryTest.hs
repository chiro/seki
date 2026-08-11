{-# LANGUAGE OverloadedStrings #-}

module Seki.Wire.Connection.RegistryTest (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, onException)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Word (Word32)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Arguments (ArgumentEncoder, DecodeResult (..), getUint, putString, runArgumentEncoder, runCompleteArgumentDecoder)
import Seki.Wire.Binding (DynamicNewId (..), Interface (..), getDynamicNewId)
import Seki.Wire.Codec (decodeFrame, encodeFrame)
import Seki.Wire.Connection (
  Connection,
  DispatchResult (..),
  closeConnection,
  dispatchNextEvent,
  eventRoute,
  newConnection,
  submitNewObjectRequest,
 )
import Seki.Wire.Connection.ObjectTable (Object, defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry (
  GlobalAnnouncement (..),
  RegistryError (..),
  announcedGlobals,
  applyRegistryEvent,
  bindGlobalWithRoute,
  emptyGlobalCatalog,
  globalInterface,
  globalNameFromWord,
  globalVersion,
  selectGlobal,
  selectGlobals,
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
import Seki.Wire.Types (FrameDecode (..), Opcode (..), RawFrame (..))
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland global registry"
    [ testCase "tracks announcements and removals" testCatalogLifecycle
    , testCase "negotiates typed global versions" testVersionNegotiation
    , testCase "rejects invalid catalog operations" testCatalogErrors
    , testCase "binds a typed global with its event route" testTypedBind
    ]

testCatalogLifecycle :: Assertion
testCatalogLifecycle = do
  catalog1 <- expectRight (applyRegistryEvent (globalEvent 7 "wl_seat" 8) emptyGlobalCatalog)
  catalog2 <- expectRight (applyRegistryEvent (globalEvent 3 "wl_compositor" 6) catalog1)
  assertEqual
    "numeric order"
    [ GlobalAnnouncement (globalNameFromWord 3) "wl_compositor" 6
    , GlobalAnnouncement (globalNameFromWord 7) "wl_seat" 8
    ]
    (announcedGlobals catalog2)
  catalog3 <- expectRight (applyRegistryEvent (removeEvent 7) catalog2)
  assertEqual
    "removed"
    [GlobalAnnouncement (globalNameFromWord 3) "wl_compositor" 6]
    (announcedGlobals catalog3)
  catalog4 <- expectRight (applyRegistryEvent (globalEvent 7 "wl_output" 4) catalog3)
  assertEqual "name reusable after removal" 2 (length (announcedGlobals catalog4))

testVersionNegotiation :: Assertion
testVersionNegotiation = do
  catalog1 <- expectRight (applyRegistryEvent (globalEvent 9 "wl_seat" 8) emptyGlobalCatalog)
  catalog2 <- expectRight (applyRegistryEvent (globalEvent 4 "wl_seat" 3) catalog1)
  capped <- expectRight (selectGlobal Wayland.wlSeatInterface 6 (globalNameFromWord 9) catalog2)
  assertEqual "caller cap" 6 (globalVersion capped)
  assertEqual "typed interface" Wayland.wlSeatInterface (globalInterface capped)
  allSeats <- expectRight (selectGlobals Wayland.wlSeatInterface 10 catalog2)
  assertEqual "advertised versions" [3, 8] (map globalVersion allSeats)

testCatalogErrors :: Assertion
testCatalogErrors = do
  let name = globalNameFromWord 5
  expectLeft
    (InvalidAnnouncedGlobalVersion name "wl_seat" 0)
    (applyRegistryEvent (globalEvent 5 "wl_seat" 0) emptyGlobalCatalog)
  catalog <- expectRight (applyRegistryEvent (globalEvent 5 "wl_seat" 4) emptyGlobalCatalog)
  expectLeft
    (DuplicateGlobalName name "wl_seat" "wl_output")
    (applyRegistryEvent (globalEvent 5 "wl_output" 3) catalog)
  expectLeft
    (GlobalInterfaceMismatch name "wl_output" "wl_seat")
    (selectGlobal Wayland.wlOutputInterface 2 name catalog)
  expectLeft
    (InvalidRequestedGlobalVersion "wl_seat" 0)
    (selectGlobal Wayland.wlSeatInterface 0 name catalog)
  expectLeft
    (InvalidRequestedGlobalVersion "wl_output" 0)
    (selectGlobals Wayland.wlOutputInterface 0 emptyGlobalCatalog)
  expectLeft
    (InvalidGeneratedGlobalVersion "broken" 0)
    (selectGlobal (Interface "broken" 0) 1 name catalog)
  expectLeft
    (UnknownGlobalName (globalNameFromWord 99))
    (applyRegistryEvent (removeEvent 99) catalog)

testTypedBind :: Assertion
testTypedBind =
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
    catalog <- expectRight (applyRegistryEvent (globalEvent 21 "wl_seat" 8) emptyGlobalCatalog)
    global <- expectRight (selectGlobal Wayland.wlSeatInterface 6 (globalNameFromWord 21) catalog)
    handled <- newEmptyMVar
    seat <-
      expectRight
        =<< bindGlobalWithRoute
          connection
          registry
          global
          (eventRoute Wayland.decodeWlSeatEvent (\_ event -> putMVar handled event))
    bindFrame <- receiveFrame peer
    assertEqual "registry sender" (objectId registry) (rawFrameSender bindFrame)
    assertEqual "bind opcode" (Opcode 0) (rawFrameOpcode bindFrame)
    case runCompleteArgumentDecoder ((,) <$> getUint <*> getDynamicNewId) (rawFramePayload bindFrame) ([] :: [()]) of
      Done (name, dynamicId) remaining [] -> do
        assertEqual "global name" 21 name
        assertEqual
          "dynamic new_id"
          DynamicNewId
            { dynamicNewIdInterface = "wl_seat"
            , dynamicNewIdVersion = 6
            , dynamicNewIdObject = objectId seat
            }
          dynamicId
        assertEqual "complete bind payload" ByteString.empty remaining
      result -> assertFailure ("unexpected bind payload: " <> show result)
    payload <- encodedPayload (putString "seat0")
    frameBytes <- expectRight (encodeFrame (RawFrame (objectId seat) (Opcode 1) payload))
    assertEqual "send seat event" (Right ()) =<< sendMessage peer frameBytes []
    dispatchResult <- dispatchNextEvent connection
    case dispatchResult of
      Right (EventDispatched _) -> pure ()
      result -> assertFailure ("unexpected dispatch result: " <> show result)
    assertEqual "typed seat event" (Wayland.WlSeatName "seat0") =<< takeMVar handled

globalEvent :: Word32 -> Text -> Word32 -> Wayland.WlRegistryEvent ()
globalEvent = Wayland.WlRegistryGlobal

removeEvent :: Word32 -> Wayland.WlRegistryEvent ()
removeEvent = Wayland.WlRegistryGlobalRemove

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

expectLeft :: (Eq errorValue, Show errorValue) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result = case result of
  Left actual -> assertEqual "error" expected actual
  Right _ -> assertFailure "expected error"
