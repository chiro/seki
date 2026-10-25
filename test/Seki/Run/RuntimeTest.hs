{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.RuntimeTest (tests) where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Internal.Core.Geometry (GeometryError (..), Point (..), mkRect, mkSize)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Inventory (
  RiverInventoryError (..),
  RiverOutputState (..),
  RiverSize (..),
  RiverWindowState (..),
  riverInventoryOutputs,
  riverInventoryWindows,
 )
import Seki.Run.Latency
import Seki.Run.LayerShell (RiverLayerShellBootstrapError (..))
import Seki.Run.MockRiver (
  callbackDoneFrame,
  encodedFrame,
  expectMockSuccess,
  expectNoBootstrapBind,
  expectRight,
  newFrameReader,
  receiveExpectedRequest,
  receiveGlobalBind,
  receiveKeyBindingRequest,
  receiveLayerShellOutputRequest,
  receiveLayerShellSeatRequest,
  receiveNewIdRequest,
  receivePointerBindingRequest,
  receiveRuntimeBinds,
  receiveRuntimeBindsAtManagerVersion,
  receiveSequenceCompletion,
  receiveStartupRequests,
  registryGlobalFrame,
  requireObjectId,
  runMock,
  sendFrames,
  withRuntimePair,
 )
import Seki.Run.Phase (disableKeyBinding, setWindowDimensionBounds)
import Seki.Run.Pointer (RiverPointerBinding (..))
import Seki.Run.River (RiverBootstrapError (..), RiverWindowManagerHandlers (..))
import Seki.Run.Runtime (
  PointerBindingEvent (..),
  RenderOutcome (..),
  RiverLayerFocus (..),
  RiverRuntimeError (..),
  RiverRuntimeHandlers (..),
  RiverRuntimeSnapshot (..),
  RiverSeatBindings (..),
  closeRiverSession,
  openRiverSessionAt,
  readRiverInventory,
  readRiverSeatBindings,
  riverLayerShell,
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
import Seki.Wire.Arguments (putInt, putNewId, putObject, putUint)
import Seki.Wire.Connection (ConnectionError (..), dispatchNextEvent)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit, makeObjectLimit, objectId)
import Seki.Wire.Connection.Registry (RegistryError (..), globalNameFromWord)
import Seki.Wire.Transport (OwnedFd, closeTransport, receiveChunk)
import Seki.Wire.Types (Opcode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river runtime"
    [ testCase "opens, binds, dispatches, and closes an owned runtime" testOwnedRuntime
    , testCase "counts and rethrows sequence handler exceptions" testHandlerException
    , testCase "retires removed inventory objects and their nodes" testInventoryRemoval
    , testCase "does not finish a sequence after a phase-plan failure" testPhasePlanFailure
    , testCase "rejects window interactions for untracked windows" testUnknownInteraction
    , testCase "promotes inventory failures before user event delivery" testInventoryFailure
    , testCase "promotes window-node acquisition failures" testNodeAcquisitionFailure
    , testCase "owns per-seat XKB binding objects" testSeatBindingLifecycle
    , testCase "owns per-seat pointer bindings and passes the hovered window with a press" testPointerBindingLifecycle
    , testCase "owns one layer-shell output per output and plans with its usable area" testOutputLayerShellLifecycle
    , testCase "promotes invalid usable areas to inventory errors" testInvalidUsableArea
    , testCase "owns one layer-shell seat per seat and tracks its keyboard owner" testSeatLayerShellLifecycle
    , testCase "rejects an out-of-order manager sequence event" testSequenceOrderError
    , testCase "fails when river reports window management unavailable" testManagerUnavailable
    , testCase "closes startup when the manager global is missing" testMissingManager
    , testCase "closes startup when the XKB bindings global is missing" testMissingXkb
    , testCase "closes startup when the layer-shell global is missing" testMissingLayerShell
    , testCase "closes startup when the layer-shell global announces version 0" testZeroVersionLayerShell
    , testCase "closes startup when the layer-shell bind fails" testLayerShellBindFailure
    , testCase "promotes live registry failures from the event loop" testLiveRegistryFailure
    ]

testOwnedRuntime :: Assertion
testOwnedRuntime =
  withRuntimePair $ \target peer -> do
    recorder <- newRecorder 10
    callbackTimes <- newIORef []
    managerEvents <- newIORef []
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 91
        ]
      (managerId, xkbBindingsId, layerShellId) <- receiveRuntimeBinds reader peer
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
      pure (managerId, xkbBindingsId, layerShellId)
    session <-
      expectRight
        =<< openRiverSessionAt
          target
          defaultObjectLimit
          ( ( runtimeHandlers
                ( \event -> do
                    now <- getMonotonicTimeNSec
                    modifyIORef' callbackTimes (<> [now])
                    modifyIORef' managerEvents (<> [event])
                )
            )
              { riverSequenceRecorder = Just recorder
              }
          )
    assertEqual "clean event-loop EOF" (Right ()) =<< runRiverSession session
    (managerId, xkbBindingsId, layerShellId) <- expectMockSuccess mockDone
    assertEqual "bound manager object" managerId (objectId (riverWindowManager session))
    assertEqual "bound XKB object" xkbBindingsId (objectId (riverXkbBindings session))
    assertEqual "bound layer-shell object" layerShellId (objectId (riverLayerShell session))
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
    recording <- readRecording recorder
    times <- readIORef callbackTimes
    assertEqual "successful measured kinds" [ManageSequence, RenderSequence, RenderSequence, ManageSequence, RenderSequence] (map sampleKind (recordingSamples recording))
    assertEqual "entry brackets handler and submission" True (and (zipWith (\sample now -> sampleStartedNS sample <= now && now <= sampleSubmittedNS sample) (recordingSamples recording) times))
    assertEqual "no measurement failures" (5, 0, 0) (recordingSuccessful recording, recordingFailed recording, recordingDropped recording)
    closeRiverSession session

testHandlerException :: Assertion
testHandlerException = withRuntimePair $ \target peer -> do
  recorder <- newRecorder 10
  mockDone <- runMock $ do
    reader <- newFrameReader
    (registryId, callbackId) <- receiveStartupRequests reader peer
    sendFrames
      peer
      [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
      , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
      , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
      , callbackDoneFrame callbackId 91
      ]
    (managerId, _, _) <- receiveRuntimeBinds reader peer
    sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
    receiveChunk peer 8 >>= \case
      Right Nothing -> pure ()
      _ -> assertFailure "handler exception sent a successful finish"
  let handlers = (runtimeHandlers (\_ -> throwIO (ErrorCall "latency exception sentinel"))){riverSequenceRecorder = Just recorder}
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
  result <- try @ErrorCall (runRiverSession session)
  case result of
    Left exception -> assertEqual "original exception" "latency exception sentinel" (show exception)
    Right value -> assertFailure ("exception swallowed: " <> show value)
  expectMockSuccess mockDone
  recording <- readRecording recorder
  assertEqual "exception has no successful sample" (0, 1, []) (recordingSuccessful recording, recordingFailed recording, recordingSamples recording)

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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 97
        ]
      (managerId, _, layerShellId) <- receiveRuntimeBinds reader peer
      windowId <- requireObjectId 0xff000005
      outputId <- requireObjectId 0xff000006
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 6) (putNewId windowId)
        , encodedFrame managerId (Opcode 7) (putNewId outputId)
        ]
      nodeId <- receiveNewIdRequest reader peer windowId (Opcode 2)
      layerShellOutputId <- receiveLayerShellOutputRequest reader peer layerShellId outputId
      sendFrames
        peer
        [ encodedFrame windowId (Opcode 0) (pure ())
        , encodedFrame outputId (Opcode 0) (pure ())
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveExpectedRequest reader peer nodeId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer layerShellOutputId (Opcode 0) (pure ())
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
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
            , riverSequenceRecorder = Nothing
            , prepareRiverManagePlan = \_ -> pure (pure ())
            , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
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
    recorder <- newRecorder 10
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 3
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 94
        ]
      (managerId, _, _) <- receiveRuntimeBindsAtManagerVersion reader peer 3
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
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
            , riverSequenceRecorder = Just recorder
            , prepareRiverManagePlan = \snapshot ->
                pure $ case riverInventoryWindows (riverRuntimeInventory snapshot) of
                  window : _ -> setWindowDimensionBounds (riverWindowObject window) 1920 1080
                  [] -> pure ()
            , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    runRiverSession session >>= \case
      Left (RiverRuntimeConnectionError _) -> pure ()
      result -> assertFailure ("expected phase-plan connection error, got: " <> show result)
    expectMockSuccess mockDone
    recording <- readRecording recorder
    assertEqual "failed phase has no successful sample" (0, 1, []) (recordingSuccessful recording, recordingFailed recording, recordingSamples recording)

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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 95
        ]
      (managerId, _, _) <- receiveRuntimeBinds reader peer
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
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
            , riverSequenceRecorder = Nothing
            , prepareRiverManagePlan = \_ -> pure (pure ())
            , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
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

testUnknownInteraction :: Assertion
testUnknownInteraction =
  withRuntimePair $ \target peer -> do
    seatId <- requireObjectId 0xff000070
    unknownId <- requireObjectId 0xff000071
    delivered <- newIORef (0 :: Int)
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 99
        ]
      (managerId, _, layerShellId) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 8) (putNewId seatId)]
      _ <- receiveLayerShellSeatRequest reader peer layerShellId seatId
      sendFrames peer [encodedFrame seatId (Opcode 4) (putObject unknownId)]
      receiveChunk peer 8 >>= \case
        Right Nothing -> pure ()
        _ -> assertFailure "runtime remained open after an unknown interaction"
    let handlers =
          (runtimeHandlers (\_ -> pure ()))
            { riverProtocolHandlers =
                (protocolHandlers (\_ -> pure ()))
                  { handleRiverSeatEvent = \_ _ -> modifyIORef' delivered (+ 1)
                  }
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "unknown interaction" (Left (RiverRuntimeInventoryError (RiverInventoryWindowNotTracked unknownId))) =<< runRiverSession session
    assertEqual "invalid interaction withheld" 0 =<< readIORef delivered
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 96
        ]
      (managerId, _, _) <- receiveRuntimeBinds reader peer
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
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
            , riverSequenceRecorder = Nothing
            , prepareRiverManagePlan = \_ -> pure (pure ())
            , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
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
    disabledBindings <- newIORef []
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 98
        ]
      (managerId, xkbBindingsId, layerShellId) <- receiveRuntimeBinds reader peer
      seatId <- requireObjectId 0xff000007
      sendFrames peer [encodedFrame managerId (Opcode 8) (putNewId seatId)]
      layerShellSeatId <- receiveLayerShellSeatRequest reader peer layerShellId seatId
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
      receiveExpectedRequest reader peer layerShellSeatId (Opcode 0) (pure ())
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
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent =
                \definition binding event -> case event of
                  Xkb.RiverXkbBindingV1Pressed -> do
                    modifyIORef' bindingPresses (<> [definition])
                    modifyIORef' disabledBindings (<> [binding])
                  _ -> pure ()
            , riverSequenceRecorder = Nothing
            , prepareRiverManagePlan = \_ -> do
                bindings <- readIORef disabledBindings
                modifyIORef' disabledBindings (const [])
                pure (mapM_ disableKeyBinding bindings)
            , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "seat binding lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    assertEqual "seat creation and removal callbacks" 2 =<< readIORef seatCallbacks
    assertEqual "routed binding press" [terminalBinding] =<< readIORef bindingPresses
    assertEqual "removed seat bindings" [] =<< readRiverSeatBindings session

testPointerBindingLifecycle :: Assertion
testPointerBindingLifecycle =
  withRuntimePair $ \target peer -> do
    events <- newIORef []
    hovers <- newIORef []
    let moveBinding = RiverPointerBinding 0x110 (River.RiverSeatV1ModifiersFlags 64)
    windowId <- requireObjectId 0xff000005
    seatId <- requireObjectId 0xff000007
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 98
        ]
      (managerId, _, layerShellId) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 8) (putNewId seatId)]
      layerShellSeatId <- receiveLayerShellSeatRequest reader peer layerShellId seatId
      bindingId <- receivePointerBindingRequest reader peer seatId moveBinding
      sendFrames peer [encodedFrame managerId (Opcode 6) (putNewId windowId)]
      nodeId <- receiveNewIdRequest reader peer windowId (Opcode 2)
      let render = do
            sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
            receiveSequenceCompletion reader peer managerId (Opcode 4)
          manage frames = do
            sendFrames peer (frames ++ [encodedFrame managerId (Opcode 2) (pure ())])
            receiveSequenceCompletion reader peer managerId (Opcode 2)
            render
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveExpectedRequest reader peer bindingId (Opcode 1) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      render
      manage [encodedFrame seatId (Opcode 2) (putObject windowId), encodedFrame bindingId (Opcode 0) (pure ())]
      manage [encodedFrame seatId (Opcode 3) (pure ()), encodedFrame bindingId (Opcode 0) (pure ())]
      manage [encodedFrame bindingId (Opcode 1) (pure ())]
      sendFrames peer [encodedFrame seatId (Opcode 2) (putObject windowId), encodedFrame windowId (Opcode 0) (pure ())]
      receiveExpectedRequest reader peer nodeId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer windowId (Opcode 0) (pure ())
      manage [encodedFrame bindingId (Opcode 0) (pure ())]
      sendFrames peer [encodedFrame seatId (Opcode 0) (pure ()), encodedFrame managerId (Opcode 2) (pure ())]
      receiveExpectedRequest reader peer bindingId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer layerShellSeatId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer seatId (Opcode 0) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      closeTransport peer
    let handlers =
          (runtimeHandlers (\_ -> pure ()))
            { riverPointerBindings = [moveBinding]
            , handleRiverPointerBindingEvent = \definition seat event -> do
                assertEqual "pressed binding definition" moveBinding definition
                assertEqual "pressed binding seat" seatId (objectId seat)
                modifyIORef' events (<> [pressedOn event])
            , prepareRiverManagePlan = \snapshot -> do
                modifyIORef' hovers (<> [map (fmap objectId . riverSeatPointerWindow) (riverRuntimeSeatBindings snapshot)])
                pure (pure ())
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "pointer binding lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    assertEqual
      "a press carries the hovered window, a leave or close clears it, and a release carries none"
      [Just (Just windowId), Just Nothing, Nothing, Just Nothing]
      =<< readIORef events
    assertEqual "the seat's hovered window at each manage" [[Nothing], [Just windowId], [Nothing], [Nothing], [Nothing], []] =<< readIORef hovers
 where
  pressedOn = \case
    PointerPressed window -> Just (objectId <$> window)
    PointerReleased -> Nothing

testOutputLayerShellLifecycle :: Assertion
testOutputLayerShellLifecycle =
  withRuntimePair $ \target peer -> do
    plannedAreas <- newIORef []
    outputId <- requireObjectId 0xff000080
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 1
        ]
      (managerId, _, layerShellId) <- receiveRuntimeBinds reader peer
      sendFrames
        peer
        [ encodedFrame managerId (Opcode 7) (putNewId outputId)
        , encodedFrame outputId (Opcode 2) (putInt 0 >> putInt 0)
        , encodedFrame outputId (Opcode 3) (putInt 1920 >> putInt 1080)
        ]
      layerShellOutputId <- receiveLayerShellOutputRequest reader peer layerShellId outputId
      sendFrames
        peer
        [ encodedFrame layerShellOutputId (Opcode 0) (putInt 0 >> putInt 30 >> putInt 1920 >> putInt 1050)
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames
        peer
        [ encodedFrame layerShellOutputId (Opcode 0) (putInt 0 >> putInt 0 >> putInt 1920 >> putInt 1080)
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames
        peer
        [ encodedFrame outputId (Opcode 0) (pure ())
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveExpectedRequest reader peer layerShellOutputId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer outputId (Opcode 0) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      closeTransport peer
    let handlers =
          (runtimeHandlers (\_ -> pure ()))
            { prepareRiverManagePlan = \snapshot -> do
                modifyIORef'
                  plannedAreas
                  (<> [map riverOutputUsableArea (riverInventoryOutputs (riverRuntimeInventory snapshot))])
                pure (pure ())
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "layer-shell output lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    firstArea <- expectRight (mkSize 1920 1050 >>= mkRect (Point 0 30))
    secondArea <- expectRight (mkSize 1920 1080 >>= mkRect (Point 0 0))
    assertEqual
      "usable area seen by each manage plan"
      [[Just firstArea], [Just secondArea], []]
      =<< readIORef plannedAreas

testInvalidUsableArea :: Assertion
testInvalidUsableArea =
  withRuntimePair $ \target peer -> do
    outputId <- requireObjectId 0xff000081
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 1
        ]
      (managerId, _, layerShellId) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 7) (putNewId outputId)]
      layerShellOutputId <- receiveLayerShellOutputRequest reader peer layerShellId outputId
      sendFrames peer [encodedFrame layerShellOutputId (Opcode 0) (putInt 0 >> putInt 0 >> putInt (-1) >> putInt 5)]
      expectNoBootstrapBind peer
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (\_ -> pure ()))
    assertEqual
      "usable-area inventory error"
      (Left (RiverRuntimeInventoryError (RiverInventoryInvalidUsableArea outputId (NegativeSize (-1) 5))))
      =<< runRiverSession session
    expectMockSuccess mockDone

testSeatLayerShellLifecycle :: Assertion
testSeatLayerShellLifecycle =
  withRuntimePair $ \target peer -> do
    plannedFocus <- newIORef []
    seatId <- requireObjectId 0xff000090
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 1
        ]
      (managerId, _, layerShellId) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 8) (putNewId seatId)]
      layerShellSeatId <- receiveLayerShellSeatRequest reader peer layerShellId seatId
      let manageWith events = do
            sendFrames peer (events <> [encodedFrame managerId (Opcode 2) (pure ())])
            receiveSequenceCompletion reader peer managerId (Opcode 2)
            sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
            receiveSequenceCompletion reader peer managerId (Opcode 4)
      manageWith []
      manageWith [encodedFrame layerShellSeatId (Opcode 0) (pure ())]
      manageWith [encodedFrame layerShellSeatId (Opcode 1) (pure ())]
      manageWith [encodedFrame layerShellSeatId (Opcode 2) (pure ())]
      sendFrames
        peer
        [ encodedFrame seatId (Opcode 0) (pure ())
        , encodedFrame managerId (Opcode 2) (pure ())
        ]
      receiveExpectedRequest reader peer layerShellSeatId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer seatId (Opcode 0) (pure ())
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      closeTransport peer
    let handlers =
          (runtimeHandlers (\_ -> pure ()))
            { prepareRiverManagePlan = \snapshot -> do
                modifyIORef' plannedFocus (<> [map riverSeatLayerFocus (riverRuntimeSeatBindings snapshot)])
                pure (pure ())
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "layer-shell seat lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    assertEqual
      "keyboard owner seen by each manage plan"
      [ [RiverLayerFocusNone]
      , [RiverLayerFocusExclusive]
      , [RiverLayerFocusNonExclusive]
      , [RiverLayerFocusNone]
      , []
      ]
      =<< readIORef plannedFocus

testSequenceOrderError :: Assertion
testSequenceOrderError =
  withRuntimePair $ \target peer -> do
    recorder <- newRecorder 10
    managerEvents <- newIORef (0 :: Int)
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 92
        ]
      (managerId, _, _) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
    session <-
      expectRight
        =<< openRiverSessionAt
          target
          defaultObjectLimit
          ((runtimeHandlers (\_ -> modifyIORef' managerEvents (+ 1))){riverSequenceRecorder = Just recorder})
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
    recording <- readRecording recorder
    assertEqual "refused sequence counted as failure" (0, 1) (recordingSuccessful recording, recordingFailed recording)

testManagerUnavailable :: Assertion
testManagerUnavailable =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 92
        ]
      (managerId, _, _) <- receiveRuntimeBinds reader peer
      sendFrames peer [encodedFrame managerId (Opcode 0) (pure ())]
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (const (pure ())))
    expectMockSuccess mockDone
    assertEqual "unavailable ends the runtime" (Left RiverRuntimeWindowManagerUnavailable) =<< runRiverSession session

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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 1
        ]
      expectNoBootstrapBind peer
    expectRuntimeLeft
      (RiverRuntimeXkbBootstrapError RiverXkbBindingsGlobalMissing)
      =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (\_ -> pure ()))
    expectMockSuccess mockDone

testMissingLayerShell :: Assertion
testMissingLayerShell =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , callbackDoneFrame callbackId 1
        ]
      expectNoBootstrapBind peer
    expectRuntimeLeft
      (RiverRuntimeLayerShellBootstrapError RiverLayerShellGlobalMissing)
      =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (\_ -> pure ()))
    expectMockSuccess mockDone

-- | Version 1 is the lowest a registry can announce, so a lower one fails startup.
testZeroVersionLayerShell :: Assertion
testZeroVersionLayerShell =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 0
        , callbackDoneFrame callbackId 1
        ]
      expectNoBootstrapBind peer
    openRiverSessionAt target defaultObjectLimit (runtimeHandlers (\_ -> pure ())) >>= \case
      Left (RiverRuntimeStartupError _) -> pure ()
      Left other -> assertFailure ("expected a startup error, got: " <> show other)
      Right _ -> assertFailure "expected the version 0 global to fail startup"
    expectMockSuccess mockDone

testLayerShellBindFailure :: Assertion
testLayerShellBindFailure =
  withRuntimePair $ \target peer -> do
    limit <- expectRight (makeObjectLimit 5)
    mockDone <- runMock $ do
      reader <- newFrameReader
      (registryId, callbackId) <- receiveStartupRequests reader peer
      sendFrames
        peer
        [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
        , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 1
        ]
      _ <- receiveGlobalBind reader peer "manager" 7 "river_window_manager_v1" 4
      _ <- receiveGlobalBind reader peer "XKB" 8 "river_xkb_bindings_v1" 3
      expectNoBootstrapBind peer
    openRiverSessionAt target limit (runtimeHandlers (\_ -> pure ())) >>= \case
      Left (RiverRuntimeLayerShellBootstrapError (RiverLayerShellBindError _)) -> pure ()
      Left other -> assertFailure ("expected a layer-shell bind error, got: " <> show other)
      Right _ -> assertFailure "expected the layer-shell bind to fail"
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
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
    , riverPointerBindings = []
    , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
    , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
    , riverSequenceRecorder = Nothing
    , prepareRiverManagePlan = \_ -> pure (pure ())
    , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
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

expectRuntimeLeft :: RiverRuntimeError -> Either RiverRuntimeError session -> Assertion
expectRuntimeLeft expected result = case result of
  Left actual -> assertEqual "runtime error" expected actual
  Right _ -> assertFailure "expected runtime error"
