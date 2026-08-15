{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.RuntimeTest (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, onException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word32)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), close, defaultProtocol, socketPair, withFdSocket)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Default (DefaultConfig (..), defaultConfig, defaultRuntimeHandlers)
import Seki.Run.Full (prepareFullManagePlan, prepareFullRenderPlan)
import Seki.Run.Inventory (
  RiverInventoryError (..),
  RiverRectangle (..),
  RiverSize (..),
  RiverWindowState (..),
  riverInventoryActiveWindow,
  riverInventoryOutputs,
  riverInventoryWindows,
  riverOutputRectangle,
 )
import Seki.Run.Phase (disableKeyBinding, setWindowDimensionBounds)
import Seki.Run.River (RiverBootstrapError (..), RiverWindowManagerHandlers (..))
import Seki.Run.Runtime (
  RiverManageAction (..),
  RiverRuntimeError (..),
  RiverRuntimeHandlers (..),
  RiverRuntimeSnapshot (..),
  closeRiverSession,
  noRiverManageAction,
  openRiverSessionAt,
  readRiverInventory,
  readRiverSeatBindings,
  riverWaylandSession,
  riverWindowManager,
  riverXkbBindings,
  runRiverSession,
 )
import Seki.Run.Sequence (
  RiverSequenceError (..),
  RiverSequenceOperation (..),
  RiverSequencePhase (..),
 )
import Seki.Run.Startup (waylandConnection)
import Seki.Run.Xkb (RiverKeyBinding (..), RiverXkbBootstrapError (..))
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  getNewId,
  getObject,
  getUint,
  putInt,
  putNewId,
  putObject,
  putString,
  putUint,
  runArgumentEncoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Binding (DynamicNewId (..), getDynamicNewId)
import Seki.Wire.Codec (decodeFrame, encodeFrame)
import Seki.Wire.Connection (ConnectionError (..), dispatchNextEvent)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit, makeObjectLimit, objectId)
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
import Seki.Wire.Types (FrameDecode (..), ObjectId, Opcode (..), RawFrame (..), mkObjectId)
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river runtime"
    [ testCase "opens, binds, dispatches, and closes an owned runtime" testOwnedRuntime
    , testCase "executes phase plans before sequence completion" testPhasePlans
    , testCase "Full placement hides windows without an output" testFullWithoutOutput
    , testCase "runs the default focus and close policy" testDefaultPolicy
    , testCase "tracks the window selected by seat interaction" testSeatInteractionFocus
    , testCase "retires removed inventory objects and their nodes" testInventoryRemoval
    , testCase "does not finish a sequence after a phase-plan failure" testPhasePlanFailure
    , testCase "promotes inventory failures before user event delivery" testInventoryFailure
    , testCase "promotes window-node acquisition failures" testNodeAcquisitionFailure
    , testCase "owns per-seat XKB binding objects" testSeatBindingLifecycle
    , testCase "rejects an out-of-order manager sequence event" testSequenceOrderError
    , testCase "closes startup when the manager global is missing" testMissingManager
    , testCase "closes startup when the XKB bindings global is missing" testMissingXkb
    , testCase "promotes live registry failures from the event loop" testLiveRegistryFailure
    ]

testOwnedRuntime :: Assertion
testOwnedRuntime =
  withRuntimePair $ \target peer -> do
    managerEvents <- newIORef []
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 91
        ]
      (managerId, xkbBindingsId) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      closeTransport peer
      pure (managerId, xkbBindingsId)
    session <-
      expectRight
        =<< openRiverSessionAt
          target
          defaultObjectLimit
          (runtimeHandlers (\event -> modifyIORef' managerEvents (<> [event])))
    assertEqual "clean event-loop EOF" (Right ()) =<< runRiverSession session
    (managerId, xkbBindingsId) <- expectMockSuccess mockDone
    assertEqual "bound manager object" managerId (objectId (riverWindowManager session))
    assertEqual "bound XKB object" xkbBindingsId (objectId (riverXkbBindings session))
    assertEqual
      "typed manager events"
      [ River.RiverWindowManagerV1ManageStart
      , River.RiverWindowManagerV1RenderStart
      , River.RiverWindowManagerV1RenderStart
      , River.RiverWindowManagerV1ManageStart
      , River.RiverWindowManagerV1RenderStart
      ]
      =<< readIORef managerEvents
    assertEqual
      "runtime owns session close"
      (Left ConnectionClosed)
      =<< dispatchNextEvent (waylandConnection (riverWaylandSession session))
    assertEqual
      "event loop is one-shot"
      (Left RiverRuntimeEventLoopAlreadyOwned)
      =<< runRiverSession session
    closeRiverSession session

testPhasePlans :: Assertion
testPhasePlans =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 93
        ]
      (managerId, _) <- receiveRuntimeBinds reader peer
      windowId <- requireObjectId 0xff000001
      outputId <- requireObjectId 0xff000002
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 6) (putNewId windowId)
        , encodedFrame managerId (Opcode 7) (putNewId outputId)
        , encodedFrame outputId (Opcode 2) (putInt (-1920) >> putInt 30)
        , encodedFrame outputId (Opcode 3) (putInt 1280 >> putInt 720)
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      nodeId <- receiveNewIdRequest reader peer windowId (Opcode 2)
      receiveExpectedRequest reader peer windowId (Opcode 7) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 9) (putUint 15)
      receiveExpectedRequest reader peer windowId (Opcode 3) (putInt 1280 >> putInt 720)
      receiveExpectedRequest reader peer managerId (Opcode 2) (pure ())
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveExpectedRequest reader peer nodeId (Opcode 1) (putInt (-1920) >> putInt 30)
      receiveExpectedRequest reader peer windowId (Opcode 5) (pure ())
      receiveExpectedRequest reader peer managerId (Opcode 4) (pure ())
      closeTransport peer
    let handlers =
          RiverRuntimeHandlers
            { riverProtocolHandlers = protocolHandlers (\_ -> pure ())
            , riverKeyBindings = []
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = prepareFullManagePlan . riverRuntimeInventory
            , prepareRiverRenderPlan = prepareFullRenderPlan . riverRuntimeInventory
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "phase-plan lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    inventory <- readRiverInventory session
    case (riverInventoryWindows inventory, riverInventoryOutputs inventory) of
      ([window], [output]) -> do
        case riverWindowNode window of
          Nothing -> assertFailure "window node missing"
          Just _ -> pure ()
        assertEqual
          "tracked output rectangle"
          (Just (RiverRectangle (-1920) 30 1280 720))
          (riverOutputRectangle output)
        assertEqual
          "new window active"
          (Just (objectId (riverWindowObject window)))
          (objectId . riverWindowObject <$> riverInventoryActiveWindow inventory)
      _ -> assertFailure "unexpected runtime inventory"

testFullWithoutOutput :: Assertion
testFullWithoutOutput =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 95
        ]
      (managerId, _) <- receiveRuntimeBinds reader peer
      windowId <- requireObjectId 0xff000004
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 6) (putNewId windowId)
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      _ <- receiveNewIdRequest reader peer windowId (Opcode 2)
      receiveExpectedRequest reader peer managerId (Opcode 2) (pure ())
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveExpectedRequest reader peer windowId (Opcode 4) (pure ())
      receiveExpectedRequest reader peer managerId (Opcode 4) (pure ())
      closeTransport peer
    let handlers =
          RiverRuntimeHandlers
            { riverProtocolHandlers = protocolHandlers (\_ -> pure ())
            , riverKeyBindings = []
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = prepareFullManagePlan . riverRuntimeInventory
            , prepareRiverRenderPlan = prepareFullRenderPlan . riverRuntimeInventory
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "headless Full lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone

testDefaultPolicy :: Assertion
testDefaultPolicy =
  withRuntimePair $ \target peer -> do
    let config = defaultConfig{defaultTerminalProcess = pure ()}
        terminalBinding = defaultTerminalBinding config
        closeBinding = defaultCloseBinding config
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 100
        ]
      (managerId, xkbBindingsId) <- receiveRuntimeBinds reader peer
      windowId <- requireObjectId 0xff000010
      outputId <- requireObjectId 0xff000011
      seatId <- requireObjectId 0xff000012
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 6) (putNewId windowId)
        , encodedFrame managerId (Opcode 7) (putNewId outputId)
        , encodedFrame outputId (Opcode 2) (putInt 40 >> putInt 20)
        , encodedFrame outputId (Opcode 3) (putInt 1600 >> putInt 900)
        , encodedFrame managerId (Opcode 8) (putNewId seatId)
        ]
      nodeId <- receiveNewIdRequest reader peer windowId (Opcode 2)
      terminalId <- receiveKeyBindingRequest reader peer xkbBindingsId seatId terminalBinding
      closeId <- receiveKeyBindingRequest reader peer xkbBindingsId seatId closeBinding
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveExpectedRequest reader peer terminalId (Opcode 1) (putUint 0)
      receiveExpectedRequest reader peer terminalId (Opcode 2) (pure ())
      receiveExpectedRequest reader peer closeId (Opcode 1) (putUint 0)
      receiveExpectedRequest reader peer closeId (Opcode 2) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 7) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 9) (putUint 15)
      receiveExpectedRequest reader peer windowId (Opcode 3) (putInt 1600 >> putInt 900)
      receiveExpectedRequest reader peer seatId (Opcode 1) (putObject windowId)
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveExpectedRequest reader peer nodeId (Opcode 1) (putInt 40 >> putInt 20)
      receiveExpectedRequest reader peer windowId (Opcode 5) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames
        peer
        [ encodedFrame closeId (Opcode 0) (pure ())
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveExpectedRequest reader peer windowId (Opcode 1) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 7) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 9) (putUint 15)
      receiveExpectedRequest reader peer windowId (Opcode 3) (putInt 1600 >> putInt 900)
      receiveExpectedRequest reader peer seatId (Opcode 1) (putObject windowId)
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      closeTransport peer
    handlers <- expectRight (defaultRuntimeHandlers config)
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "default policy lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone

testSeatInteractionFocus :: Assertion
testSeatInteractionFocus =
  withRuntimePair $ \target peer -> do
    firstWindowId <- requireObjectId 0xff000020
    secondWindowId <- requireObjectId 0xff000021
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 99
        ]
      (managerId, _) <- receiveRuntimeBinds reader peer
      seatId <- requireObjectId 0xff000022
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 6) (putNewId firstWindowId)
        , encodedFrame managerId (Opcode 6) (putNewId secondWindowId)
        , encodedFrame managerId (Opcode 8) (putNewId seatId)
        ]
      _ <- receiveNewIdRequest reader peer firstWindowId (Opcode 2)
      _ <- receiveNewIdRequest reader peer secondWindowId (Opcode 2)
      sendFrames
        peer
        [ encodedFrame seatId (Opcode 4) (putObject firstWindowId)
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      closeTransport peer
    session <-
      expectRight
        =<< openRiverSessionAt
          target
          defaultObjectLimit
          (runtimeHandlers (\_ -> pure ()))
    assertEqual "interaction lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    inventory <- readRiverInventory session
    assertEqual
      "interacted window active"
      (Just firstWindowId)
      (objectId . riverWindowObject <$> riverInventoryActiveWindow inventory)

testInventoryRemoval :: Assertion
testInventoryRemoval =
  withRuntimePair $ \target peer -> do
    deliveredRemovals <- newIORef (0 :: Int)
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 97
        ]
      (managerId, _) <- receiveRuntimeBinds reader peer
      windowId <- requireObjectId 0xff000005
      outputId <- requireObjectId 0xff000006
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 6) (putNewId windowId)
        , encodedFrame managerId (Opcode 7) (putNewId outputId)
        ]
      nodeId <- receiveNewIdRequest reader peer windowId (Opcode 2)
      sendFrames
        peer
        [ encodedFrame windowId (Opcode 0) (pure ())
        , encodedFrame outputId (Opcode 0) (pure ())
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveExpectedRequest reader peer nodeId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer outputId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer managerId (Opcode 2) (pure ())
      closeTransport peer
    let handlers =
          RiverRuntimeHandlers
            { riverProtocolHandlers =
                (protocolHandlers (\_ -> pure ()))
                  { handleRiverWindowEvent =
                      \_ event -> case event of
                        River.RiverWindowV1Closed -> modifyIORef' deliveredRemovals (+ 1)
                        _ -> pure ()
                  , handleRiverOutputEvent =
                      \_ event -> case event of
                        River.RiverOutputV1Removed -> modifyIORef' deliveredRemovals (+ 1)
                        _ -> pure ()
                  }
            , riverKeyBindings = []
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \_ -> pure ()
            , prepareRiverRenderPlan = \_ -> pure ()
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "removal lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    assertEqual "removal callbacks" 2 =<< readIORef deliveredRemovals
    inventory <- readRiverInventory session
    assertEqual "no live windows" [] (riverInventoryWindows inventory)
    assertEqual "no live outputs" [] (riverInventoryOutputs inventory)

testPhasePlanFailure :: Assertion
testPhasePlanFailure =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 3
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 94
        ]
      (managerId, _) <- receiveRuntimeBindsAtManagerVersion reader peer 3
      windowId <- requireObjectId 0xff000002
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 6) (putNewId windowId)
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      _ <- receiveNewIdRequest reader peer windowId (Opcode 2)
      receiveChunk peer 8 >>= \case
        Right Nothing -> pure ()
        _ -> do
          closeTransport peer
          assertFailure "sequence finish was sent after a failed phase plan"
    let handlers =
          RiverRuntimeHandlers
            { riverProtocolHandlers = protocolHandlers (\_ -> pure ())
            , riverKeyBindings = []
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \snapshot ->
                case riverInventoryWindows (riverRuntimeInventory snapshot) of
                  window : _ -> setWindowDimensionBounds (riverWindowObject window) 1920 1080
                  [] -> pure ()
            , prepareRiverRenderPlan = \_ -> pure ()
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    runRiverSession session >>= \case
      Left (RiverRuntimeConnectionError _) -> pure ()
      result -> assertFailure ("expected phase-plan connection error, got: " <> show result)
    expectMockSuccess mockDone

testInventoryFailure :: Assertion
testInventoryFailure =
  withRuntimePair $ \target peer -> do
    deliveredWindowEvents <- newIORef (0 :: Int)
    windowId <- requireObjectId 0xff000003
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 95
        ]
      (managerId, _) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 6) (putNewId windowId)]
      _ <- receiveNewIdRequest reader peer windowId (Opcode 2)
      sendFrames peer [encodedFrame windowId (Opcode 2) (putInt 0 >> putInt 100)]
      receiveChunk peer 8 >>= \case
        Right Nothing -> pure ()
        _ -> do
          closeTransport peer
          assertFailure "runtime remained open after an inventory failure"
    let handlers =
          RiverRuntimeHandlers
            { riverProtocolHandlers =
                (protocolHandlers (\_ -> pure ()))
                  { handleRiverWindowEvent =
                      \_ _ -> modifyIORef' deliveredWindowEvents (+ 1)
                  }
            , riverKeyBindings = []
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \_ -> pure ()
            , prepareRiverRenderPlan = \_ -> pure ()
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual
      "inventory error"
      ( Left
          ( RiverRuntimeInventoryError
              (RiverInventoryInvalidWindowDimensions windowId (RiverSize 0 100))
          )
      )
      =<< runRiverSession session
    assertEqual "invalid event withheld" 0 =<< readIORef deliveredWindowEvents
    expectMockSuccess mockDone

testNodeAcquisitionFailure :: Assertion
testNodeAcquisitionFailure =
  withRuntimePair $ \target peer -> do
    deliveredCreations <- newIORef (0 :: Int)
    limit <- expectRight (makeObjectLimit 6)
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 96
        ]
      (managerId, _) <- receiveRuntimeBinds reader peer
      windowId <- requireObjectId 0xff000004
      sendFrames peer [encodedFrame managerId (Opcode 6) (putNewId windowId)]
      receiveChunk peer 8 >>= \case
        Right Nothing -> pure ()
        _ -> do
          closeTransport peer
          assertFailure "node request was sent despite the object limit"
    let handlers =
          RiverRuntimeHandlers
            { riverProtocolHandlers =
                (protocolHandlers (\_ -> pure ()))
                  { handleRiverWindowCreated =
                      \_ -> modifyIORef' deliveredCreations (+ 1)
                  }
            , riverKeyBindings = []
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \_ -> pure ()
            , prepareRiverRenderPlan = \_ -> pure ()
            }
    session <- expectRight =<< openRiverSessionAt target limit handlers
    runRiverSession session >>= \case
      Left (RiverRuntimeConnectionError _) -> pure ()
      result -> assertFailure ("expected node-acquisition error, got: " <> show result)
    assertEqual "failed creation withheld" 0 =<< readIORef deliveredCreations
    expectMockSuccess mockDone

testSeatBindingLifecycle :: Assertion
testSeatBindingLifecycle =
  withRuntimePair $ \target peer -> do
    seatCallbacks <- newIORef (0 :: Int)
    bindingPresses <- newIORef []
    let terminalBinding =
          RiverKeyBinding
            { riverKeyBindingKeysym = 0xff0d
            , riverKeyBindingModifiers = River.RiverSeatV1ModifiersFlags 64
            , riverKeyBindingLayoutOverride = Just 0
            }
        closeBinding =
          RiverKeyBinding
            { riverKeyBindingKeysym = 0x71
            , riverKeyBindingModifiers = River.RiverSeatV1ModifiersFlags 64
            , riverKeyBindingLayoutOverride = Nothing
            }
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 98
        ]
      (managerId, xkbBindingsId) <- receiveRuntimeBinds reader peer
      seatId <- requireObjectId 0xff000007
      sendFrames peer [encodedFrame managerId (Opcode 8) (putNewId seatId)]
      terminalId <- receiveKeyBindingRequest reader peer xkbBindingsId seatId terminalBinding
      closeId <- receiveKeyBindingRequest reader peer xkbBindingsId seatId closeBinding
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveExpectedRequest reader peer terminalId (Opcode 1) (putUint 0)
      receiveExpectedRequest reader peer terminalId (Opcode 2) (pure ())
      receiveExpectedRequest reader peer closeId (Opcode 2) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames
        peer
        [ encodedFrame terminalId (Opcode 0) (pure ())
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveExpectedRequest reader peer terminalId (Opcode 3) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames
        peer
        [ encodedFrame seatId (Opcode 0) (pure ())
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveExpectedRequest reader peer terminalId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer closeId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer seatId (Opcode 0) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      closeTransport peer
    let handlers =
          RiverRuntimeHandlers
            { riverProtocolHandlers =
                (protocolHandlers (\_ -> pure ()))
                  { handleRiverSeatCreated = \_ -> modifyIORef' seatCallbacks (+ 1)
                  , handleRiverSeatEvent =
                      \_ event -> case event of
                        River.RiverSeatV1Removed -> modifyIORef' seatCallbacks (+ 1)
                        _ -> pure ()
                  }
            , riverKeyBindings = [terminalBinding, closeBinding]
            , handleRiverKeyBindingEvent =
                \definition binding event -> case event of
                  Xkb.RiverXkbBindingV1Pressed -> do
                    modifyIORef' bindingPresses (<> [definition])
                    pure (RiverManageAction (\_ -> disableKeyBinding binding))
                  _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \_ -> pure ()
            , prepareRiverRenderPlan = \_ -> pure ()
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "seat binding lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    assertEqual "seat creation and removal callbacks" 2 =<< readIORef seatCallbacks
    assertEqual "routed binding press" [terminalBinding] =<< readIORef bindingPresses
    assertEqual "removed seat bindings" [] =<< readRiverSeatBindings session

testSequenceOrderError :: Assertion
testSequenceOrderError =
  withRuntimePair $ \target peer -> do
    managerEvents <- newIORef (0 :: Int)
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 92
        ]
      (managerId, _) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
    session <-
      expectRight
        =<< openRiverSessionAt
          target
          defaultObjectLimit
          (runtimeHandlers (\_ -> modifyIORef' managerEvents (+ 1)))
    expectMockSuccess mockDone
    assertEqual
      "sequence-order error"
      ( Left
          ( RiverRuntimeSequenceError
              ( InvalidRiverSequenceTransition
                  AwaitingInitialManage
                  StartRenderSequence
              )
          )
      )
      =<< runRiverSession session
    assertEqual "invalid event is not delivered" 0 =<< readIORef managerEvents

testMissingManager :: Assertion
testMissingManager =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 3 "wl_output" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 1
        ]
      expectNoBootstrapBind peer
    expectRuntimeLeft
      (RiverRuntimeBootstrapError RiverWindowManagerGlobalMissing)
      =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (\_ -> pure ()))
    expectMockSuccess mockDone

testMissingXkb :: Assertion
testMissingXkb =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , callbackDoneFrame callbackId 1
        ]
      expectNoBootstrapBind peer
    expectRuntimeLeft
      (RiverRuntimeXkbBootstrapError RiverXkbBindingsGlobalMissing)
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
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 2
        ]
      _ <- receiveRuntimeBinds reader peer
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
  RiverRuntimeHandlers
runtimeHandlers managerHandler =
  RiverRuntimeHandlers
    { riverProtocolHandlers = protocolHandlers managerHandler
    , riverKeyBindings = []
    , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
    , prepareRiverManagePlan = \_ -> pure ()
    , prepareRiverRenderPlan = \_ -> pure ()
    }

protocolHandlers ::
  (River.RiverWindowManagerV1Event OwnedFd -> IO ()) ->
  RiverWindowManagerHandlers
protocolHandlers managerHandler =
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

receiveRuntimeBinds :: FrameReader -> Transport -> IO (ObjectId, ObjectId)
receiveRuntimeBinds reader peer =
  receiveRuntimeBindsAtManagerVersion reader peer 4

receiveRuntimeBindsAtManagerVersion ::
  FrameReader ->
  Transport ->
  Word32 ->
  IO (ObjectId, ObjectId)
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
  pure (managerId, xkbBindingsId)

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

expectRuntimeLeft :: RiverRuntimeError -> Either RiverRuntimeError session -> Assertion
expectRuntimeLeft expected result = case result of
  Left actual -> assertEqual "runtime error" expected actual
  Right _ -> assertFailure "expected runtime error"
