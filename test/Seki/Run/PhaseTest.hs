{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.PhaseTest (tests) where

import Control.Exception (bracket, onException)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Protocol.RiverLayerShellV1 qualified as LayerShell
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Phase (
  closeWindow,
  disableKeyBinding,
  enableKeyBinding,
  enablePointerBinding,
  endPointerOp,
  exitWindowFullscreen,
  hideWindow,
  informResizeEnd,
  informResizeStart,
  informWindowFullscreen,
  informWindowNotFullscreen,
  placeNodeAbove,
  placeNodeBelow,
  placeNodeBottom,
  placeNodeTop,
  proposeWindowDimensions,
  runManageM,
  runRenderM,
  setDefaultLayerShellOutput,
  setKeyBindingLayoutOverride,
  setNodePosition,
  setWindowBorders,
  setWindowCapabilities,
  setWindowContentClipBox,
  setWindowTiled,
  showWindow,
  startPointerOp,
  useWindowSsd,
 )
import Seki.Wire.Arguments (ArgumentEncoder, putInt, putObject, putUint, runArgumentEncoder)
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Codec (decodeFrame)
import Seki.Wire.Connection (Connection, closeConnection, newConnection)
import Seki.Wire.Connection.ObjectTable (Object, defaultObjectLimit, objectId)
import Seki.Wire.Transport (
  ConnectionTarget (InheritedSocket),
  ReceivedChunk (..),
  Transport,
  closeTransport,
  openTransport,
  receiveChunk,
 )
import Seki.Wire.Types (FrameDecode (..), Opcode (..), RawFrame (..))
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river phase plans"
    [ testCase "submits manage requests in plan order" testManagePlan
    , testCase "submits XKB binding configuration in plan order" testKeyBindingManagePlan
    , testCase "submits the default layer-shell output in a manage plan" testLayerShellManagePlan
    , testCase "submits pointer op and binding requests in a manage plan" testPointerOpManagePlan
    , testCase "submits window render requests in plan order" testWindowRenderPlan
    , testCase "submits node render requests in plan order" testNodeRenderPlan
    , testCase "stops a plan at the first request failure" testPlanFailure
    ]

testManagePlan :: Assertion
testManagePlan =
  withInterfaceConnection River.riverWindowV1Interface $ \connection window peer -> do
    result <-
      runManageM connection $ do
        proposeWindowDimensions window 1920 1080
        useWindowSsd window
        setWindowTiled window (River.RiverWindowV1EdgesFlags 15)
        setWindowCapabilities window (River.RiverWindowV1CapabilitiesFlags 4)
        informWindowFullscreen window
        exitWindowFullscreen window
        informWindowNotFullscreen window
        closeWindow window
        pure (42 :: Int)
    assertEqual "plan result" (Right 42) result
    reader <- newFrameReader
    expectRequest reader peer window (Opcode 3) (putInt 1920 >> putInt 1080)
    expectRequest reader peer window (Opcode 7) (pure ())
    expectRequest reader peer window (Opcode 9) (putUint 15)
    expectRequest reader peer window (Opcode 14) (putUint 4)
    expectRequest reader peer window (Opcode 17) (pure ())
    expectRequest reader peer window (Opcode 20) (pure ())
    expectRequest reader peer window (Opcode 18) (pure ())
    expectRequest reader peer window (Opcode 1) (pure ())

testKeyBindingManagePlan :: Assertion
testKeyBindingManagePlan =
  withInterfaceConnection Xkb.riverXkbBindingV1Interface $ \connection binding peer -> do
    result <-
      runManageM connection $ do
        setKeyBindingLayoutOverride binding 0
        enableKeyBinding binding
        disableKeyBinding binding
    assertEqual "binding plan" (Right ()) result
    reader <- newFrameReader
    expectRequest reader peer binding (Opcode 1) (putUint 0)
    expectRequest reader peer binding (Opcode 2) (pure ())
    expectRequest reader peer binding (Opcode 3) (pure ())

testPointerOpManagePlan :: Assertion
testPointerOpManagePlan = do
  withInterfaceConnection River.riverSeatV1Interface $ \connection seat peer -> do
    assertEqual "seat op plan" (Right ()) =<< runManageM connection (startPointerOp seat >> endPointerOp seat)
    reader <- newFrameReader
    expectRequest reader peer seat (Opcode 4) (pure ())
    expectRequest reader peer seat (Opcode 5) (pure ())
  withInterfaceConnection River.riverWindowV1Interface $ \connection window peer -> do
    assertEqual "window resize plan" (Right ()) =<< runManageM connection (informResizeStart window >> informResizeEnd window)
    reader <- newFrameReader
    expectRequest reader peer window (Opcode 12) (pure ())
    expectRequest reader peer window (Opcode 13) (pure ())
  withInterfaceConnection River.riverPointerBindingV1Interface $ \connection binding peer -> do
    assertEqual "pointer binding plan" (Right ()) =<< runManageM connection (enablePointerBinding binding)
    reader <- newFrameReader
    expectRequest reader peer binding (Opcode 1) (pure ())

testLayerShellManagePlan :: Assertion
testLayerShellManagePlan =
  withInterfaceConnection LayerShell.riverLayerShellOutputV1Interface $ \connection layerShellOutput peer -> do
    assertEqual "layer-shell plan" (Right ()) =<< runManageM connection (setDefaultLayerShellOutput layerShellOutput)
    reader <- newFrameReader
    expectRequest reader peer layerShellOutput (Opcode 1) (pure ())

testWindowRenderPlan :: Assertion
testWindowRenderPlan =
  withInterfaceConnection River.riverWindowV1Interface $ \connection window peer -> do
    result <-
      runRenderM connection $ do
        hideWindow window
        setWindowBorders window (River.RiverWindowV1EdgesFlags 15) 2 1 2 3 4
        setWindowContentClipBox window 5 6 700 800
        showWindow window
    assertEqual "render plan" (Right ()) result
    reader <- newFrameReader
    expectRequest reader peer window (Opcode 4) (pure ())
    expectRequest reader peer window (Opcode 8) $ do
      putUint 15
      putInt 2
      putUint 1
      putUint 2
      putUint 3
      putUint 4
    expectRequest reader peer window (Opcode 22) $ do
      putInt 5
      putInt 6
      putInt 700
      putInt 800
    expectRequest reader peer window (Opcode 5) (pure ())

testNodeRenderPlan :: Assertion
testNodeRenderPlan =
  withInterfaceConnection River.riverNodeV1Interface $ \connection node peer -> do
    result <-
      runRenderM connection $ do
        setNodePosition node (-20) 30
        placeNodeTop node
        placeNodeBottom node
        placeNodeAbove node node
        placeNodeBelow node node
    assertEqual "node plan" (Right ()) result
    reader <- newFrameReader
    expectRequest reader peer node (Opcode 1) (putInt (-20) >> putInt 30)
    expectRequest reader peer node (Opcode 2) (pure ())
    expectRequest reader peer node (Opcode 3) (pure ())
    expectRequest reader peer node (Opcode 4) (putObject (objectId node))
    expectRequest reader peer node (Opcode 5) (putObject (objectId node))

testPlanFailure :: Assertion
testPlanFailure =
  withInterfaceConnection lowVersionWindowInterface $ \connection window peer -> do
    failed <-
      runRenderM connection $ do
        setWindowContentClipBox window 0 0 100 100
        showWindow window
    case failed of
      Left _ -> pure ()
      Right () -> assertFailure "expected unsupported request version"

    assertEqual "connection remains usable" (Right ()) =<< runRenderM connection (hideWindow window)
    reader <- newFrameReader
    expectRequest reader peer window (Opcode 4) (pure ())
 where
  lowVersionWindowInterface =
    Interface
      { interfaceName = "river_window_v1"
      , interfaceMaximumVersion = 1
      }

expectRequest ::
  FrameReader ->
  Transport ->
  Object interface ->
  Opcode ->
  ArgumentEncoder () () ->
  Assertion
expectRequest reader peer sender opcode arguments = do
  frame <- receiveFrame reader peer
  assertEqual "request sender" (objectId sender) (rawFrameSender frame)
  assertEqual "request opcode" opcode (rawFrameOpcode frame)
  expectedPayload <- encodedPayload arguments
  assertEqual "request payload" expectedPayload (rawFramePayload frame)

newtype FrameReader = FrameReader (IORef ByteString)

newFrameReader :: IO FrameReader
newFrameReader = FrameReader <$> newIORef ByteString.empty

receiveFrame :: FrameReader -> Transport -> IO RawFrame
receiveFrame reader@(FrameReader buffered) peer = do
  bytes <- readIORef buffered
  case decodeFrame bytes of
    Right (DecodedFrame frame suffix) -> writeIORef buffered suffix >> pure frame
    Right (NeedMoreBytes needed) ->
      receiveChunk peer (max 8 needed) >>= \case
        Right (Just chunk)
          | null (receivedFds chunk) -> do
              writeIORef buffered (bytes <> receivedBytes chunk)
              receiveFrame reader peer
          | otherwise -> assertFailure "unexpected descriptors"
        _ -> assertFailure "unexpected receive result"
    Left errorValue -> assertFailure ("unexpected frame error: " <> show errorValue)

encodedPayload :: ArgumentEncoder () () -> IO ByteString
encodedPayload encoder = case runArgumentEncoder encoder of
  Left errorValue -> assertFailure ("unexpected argument error: " <> show errorValue)
  Right ((), payload, []) -> pure payload
  Right (_, _, descriptors) -> assertFailure ("unexpected descriptors: " <> show (length descriptors))

withInterfaceConnection ::
  Interface interface ->
  (Connection -> Object interface -> Transport -> IO value) ->
  IO value
withInterfaceConnection interface action =
  bracket (openConnectionPair interface) closeConnectionPair $ \(connection, object, peer) ->
    action connection object peer

openConnectionPair :: Interface interface -> IO (Connection, Object interface, Transport)
openConnectionPair interface = do
  (firstSocket, secondSocket) <- socketPair AF_UNIX Stream defaultProtocol
  firstDescriptor <- duplicateSocket firstSocket
  secondDescriptor <- duplicateSocket secondSocket `onException` closeFd firstDescriptor
  close firstSocket
  close secondSocket
  client <- expectTransport =<< openTransport (InheritedSocket firstDescriptor)
  peer <- (expectTransport =<< openTransport (InheritedSocket secondDescriptor)) `onException` closeTransport client
  (connection, object) <-
    (expectRight =<< newConnection client defaultObjectLimit interface)
      `onException` (closeTransport client >> closeTransport peer)
  pure (connection, object, peer)

closeConnectionPair :: (Connection, Object interface, Transport) -> IO ()
closeConnectionPair (connection, _, peer) = do
  closeConnection connection
  closeTransport peer

duplicateSocket :: Socket -> IO Fd
duplicateSocket connection = withFdSocket connection (dup . Fd)

expectTransport :: (Show errorValue) => Either errorValue value -> IO value
expectTransport = either (assertFailure . ("unexpected transport error: " <>) . show) pure

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure
