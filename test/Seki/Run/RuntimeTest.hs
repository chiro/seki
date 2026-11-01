{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.RuntimeTest (tests) where

import Control.Exception (ErrorCall (..), onException, throwIO, try)
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Internal.Core.Geometry (GeometryError (..), Point (..), mkRect, mkSize)
import Seki.Internal.Input
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.External qualified as External
import Seki.Run.Input qualified as InputState
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
import Seki.Run.Libinput
import Seki.Run.MockRiver (
  FrameReader,
  callbackDoneFrame,
  decodeNewId,
  encodedFrame,
  encodedPayload,
  expectMockSuccess,
  expectNoBootstrapBind,
  expectRight,
  handshake,
  newFrameReader,
  receiveExpectedRequest,
  receiveFrame,
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
  ManageOutcome (..),
  PointerBindingEvent (..),
  RenderOutcome (..),
  RiverExit (..),
  RiverInputSnapshot (..),
  RiverLayerFocus (..),
  RiverRuntimeError (..),
  RiverRuntimeHandlers (..),
  RiverRuntimeSnapshot (..),
  RiverSeatBindings (..),
  closeRiverSession,
  openRiverSessionAt,
  readRiverInputSnapshot,
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
import Seki.Run.XkbConfig
import Seki.Wire.Arguments (fixedFromRaw, putArray, putFixed, putInt, putNewId, putObject, putString, putUint)
import Seki.Wire.Arguments qualified as SekiArguments
import Seki.Wire.Connection (ConnectionError (..), dispatchNextEvent)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit, makeObjectLimit, objectId)
import Seki.Wire.Connection.Registry (RegistryError (..), globalNameFromWord)
import Seki.Wire.Transport (OwnedFd, ReceivedChunk (..), Transport, closeTransport, receiveChunk)
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river runtime"
    [ testCase "libinput custom curves success rejection removal v1/v2" (mapM_ (\v -> mapM_ (testCustomLibinput v) [0, 1, 2, 3, 4]) [1, 2])
    , testCase "XKB retains two distinct keyboards with empty rules" testTwoXkbKeyboards
    , testCase "XKB synthetic v1/v2/v3 batching, feedback and scrolllock gate" (mapM_ testXkbConfig [1, 2, 3])
    , testCase "libinput full synthetic peer surface and acknowledgements v1/v2" (mapM_ (\v -> mapM_ (testLibinput v) [0, 1, 2]) [1, 2])
    , testCase "requested generic rules report unavailable without optional global" testInputUnavailable
    , testCase "v1 transport callbacks only ready their captured cohort" testInputCohort
    , testCase "generic input v1 uses a creation sync and v2 uses done" (testInput 1 >> testInput 2)
    , testCase "bounded external batches finish before re-wake and close on EOF" testExternalBatches
    , testCase "opens, binds, dispatches, and closes an owned runtime" testOwnedRuntime
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
    , testCase "M2-D08: answers a render sequence that comes before the first manage sequence" testRenderBeforeFirstManage
    , testCase "a manage plan that asks to stop sends stop once after manage_finish, then destroy on finished" testStopLifecycle
    , testCase "a finished seki did not ask for is destroyed and ends the session" testUnrequestedFinished
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
    assertEqual "clean event-loop EOF" (Right RiverEnded) =<< runRiverSession session
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
    assertEqual "optional input absent" (InputUnavailable []) =<< readRiverInputSnapshot session
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
            { riverInputRules = []
            , riverExternalIngress = Nothing
            , riverProtocolHandlers =
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
            , prepareRiverManagePlan = \_ -> pure (pure KeepManaging)
            , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "removal lifecycle" (Right RiverEnded) =<< runRiverSession session
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
            { riverInputRules = []
            , riverExternalIngress = Nothing
            , riverProtocolHandlers = protocolHandlers (\_ -> pure ())
            , riverKeyBindings = []
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
            , riverSequenceRecorder = Just recorder
            , prepareRiverManagePlan = \snapshot ->
                pure $ case riverInventoryWindows (riverRuntimeInventory snapshot) of
                  window : _ -> setWindowDimensionBounds (riverWindowObject window) 1920 1080 >> pure KeepManaging
                  [] -> pure KeepManaging
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
            { riverInputRules = []
            , riverExternalIngress = Nothing
            , riverProtocolHandlers =
                (protocolHandlers (\_ -> pure ()))
                  { handleRiverWindowEvent =
                      \_ _ -> modifyIORef' deliveredWindowEvents (+ 1)
                  }
            , riverKeyBindings = []
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
            , riverSequenceRecorder = Nothing
            , prepareRiverManagePlan = \_ -> pure (pure KeepManaging)
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
            { riverInputRules = []
            , riverExternalIngress = Nothing
            , riverProtocolHandlers =
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
            { riverInputRules = []
            , riverExternalIngress = Nothing
            , riverProtocolHandlers =
                (protocolHandlers (\_ -> pure ()))
                  { handleRiverWindowCreated =
                      \_ -> modifyIORef' deliveredCreations (+ 1)
                  }
            , riverKeyBindings = []
            , riverPointerBindings = []
            , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
            , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
            , riverSequenceRecorder = Nothing
            , prepareRiverManagePlan = \_ -> pure (pure KeepManaging)
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
            { riverInputRules = []
            , riverExternalIngress = Nothing
            , riverProtocolHandlers =
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
                pure (mapM_ disableKeyBinding bindings >> pure KeepManaging)
            , prepareRiverRenderPlan = \_ -> pure (pure (RenderOutcome False))
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "seat binding lifecycle" (Right RiverEnded) =<< runRiverSession session
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
                pure (pure KeepManaging)
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "pointer binding lifecycle" (Right RiverEnded) =<< runRiverSession session
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
                pure (pure KeepManaging)
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "layer-shell output lifecycle" (Right RiverEnded) =<< runRiverSession session
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
                pure (pure KeepManaging)
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "layer-shell seat lifecycle" (Right RiverEnded) =<< runRiverSession session
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
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ()), encodedFrame managerId (Opcode 2) (pure ())]
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
                  AwaitingRequiredRender
                  StartManageSequence
              )
          )
      )
      =<< runRiverSession session
    assertEqual "only the valid event is delivered" 1 =<< readIORef managerEvents
    recording <- readRecording recorder
    assertEqual "refused sequence counted as failure" (1, 1) (recordingSuccessful recording, recordingFailed recording)

{- | River keeps one sequence state across window managers, so a manager that
binds while its predecessor's render is pending gets render_start first.
-}
testRenderBeforeFirstManage :: Assertion
testRenderBeforeFirstManage =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ flip onException (closeTransport peer) $ do
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
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      closeTransport peer
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (const (pure ())))
    result <- runRiverSession session
    expectMockSuccess mockDone
    assertEqual "the early render and the first manage are answered" (Right RiverEnded) result

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
    { riverInputRules = []
    , riverExternalIngress = Nothing
    , riverProtocolHandlers = protocolHandlers managerHandler
    , riverKeyBindings = []
    , riverPointerBindings = []
    , handleRiverPointerBindingEvent = \_ _ _ -> pure ()
    , handleRiverKeyBindingEvent = \_ _ _ -> pure ()
    , riverSequenceRecorder = Nothing
    , prepareRiverManagePlan = \_ -> pure (pure KeepManaging)
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

-- | Bind the runtime's globals and return the manager's object ID.
bootManager :: FrameReader -> Transport -> IO ObjectId
bootManager reader peer = do
  (registryId, callbackId) <- receiveStartupRequests reader peer
  sendFrames
    peer
    [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
    , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
    , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
    , callbackDoneFrame callbackId 93
    ]
  (managerId, _, _) <- receiveRuntimeBinds reader peer
  pure managerId

expectClosed :: Transport -> Assertion
expectClosed peer =
  receiveChunk peer 8 >>= \case
    Right Nothing -> pure ()
    other -> assertFailure ("expected the runtime to close the connection, got " <> show (fmap (fmap receivedBytes) other))

{- | River may send more sequences before it processes stop. They are answered
as usual, and a second stop request from the plan is not sent.
-}
testStopLifecycle :: Assertion
testStopLifecycle =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ flip onException (closeTransport peer) $ do
      reader <- newFrameReader
      managerId <- bootManager reader peer
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      receiveExpectedRequest reader peer managerId (Opcode 0) (pure ())
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 4)
      sendFrames peer [encodedFrame managerId (Opcode 1) (pure ())]
      receiveExpectedRequest reader peer managerId (Opcode 1) (pure ())
      expectClosed peer
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (const (pure ()))){prepareRiverManagePlan = \_ -> pure (pure StopManaging)}
    result <- runRiverSession session
    expectMockSuccess mockDone
    assertEqual "a requested finish ends the session as stopped" (Right RiverStopped) result

testUnrequestedFinished :: Assertion
testUnrequestedFinished =
  withRuntimePair $ \target peer -> do
    mockDone <- runMock $ flip onException (closeTransport peer) $ do
      reader <- newFrameReader
      managerId <- bootManager reader peer
      sendFrames peer [encodedFrame managerId (Opcode 2) (pure ())]
      receiveSequenceCompletion reader peer managerId (Opcode 2)
      sendFrames peer [encodedFrame managerId (Opcode 1) (pure ())]
      receiveExpectedRequest reader peer managerId (Opcode 1) (pure ())
      expectClosed peer
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (const (pure ())))
    result <- runRiverSession session
    expectMockSuccess mockDone
    assertEqual "an unrequested finish ends the session" (Right RiverEnded) result

testExternalBatches :: Assertion
testExternalBatches = withRuntimePair $ \target peer -> do
  queue <- External.newExternalIngress
  applied <- newIORef []
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
    (manager, _, _) <- receiveRuntimeBinds reader peer
    receiveSequenceCompletion reader peer manager (Opcode 3)
    mapM_
      ( \index -> do
          sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
          receiveSequenceCompletion reader peer manager (Opcode 2)
          if index < 3 then receiveSequenceCompletion reader peer manager (Opcode 3) else pure ()
          sendFrames peer [encodedFrame manager (Opcode 3) (pure ())]
          receiveSequenceCompletion reader peer manager (Opcode 4)
      )
      [1 .. 3 :: Int]
    closeTransport peer
  let handlers =
        (runtimeHandlers (\_ -> pure ()))
          { riverInputRules = []
          , riverExternalIngress = Just (External.externalLifecycle queue)
          , prepareRiverManagePlan = \_ -> do
              batch <- External.drainExternal queue
              modifyIORef' applied (<> External.drainedInputs batch)
              pure (pure KeepManaging)
          }
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
  mapM (External.submitExternal (External.externalProducer queue)) [1 .. 65 :: Int] >>= assertEqual "admitted burst" (replicate 65 External.Accepted)
  runRiverSession session >>= assertEqual "EOF" (Right RiverEnded)
  expectMockSuccess mockDone
  readIORef applied >>= assertEqual "all batches applied once in FIFO order" [1 .. 65]
  External.submitExternal (External.externalProducer queue) 66 >>= assertEqual "EOF closed producer" External.IngressClosed

testInput :: Word32 -> Assertion
testInput version = withRuntimePair $ \target peer -> do
  patternValue <- expectRight (compileDevicePattern "*")
  mockDone <- runMock $ do
    reader <- newFrameReader
    (registry, callback) <- receiveStartupRequests reader peer
    sendFrames
      peer
      [ registryGlobalFrame registry 7 "river_window_manager_v1" 4
      , registryGlobalFrame registry 8 "river_xkb_bindings_v1" 3
      , registryGlobalFrame registry 9 "river_layer_shell_v1" 1
      , registryGlobalFrame registry 10 "river_input_manager_v1" version
      , callbackDoneFrame callback 1
      ]
    input <- receiveGlobalBind reader peer "input first" 10 "river_input_manager_v1" version
    (manager, _, _) <- receiveRuntimeBinds reader peer
    device <- requireObjectId 0xff000001
    sendFrames peer [encodedFrame input (Opcode 1) (putNewId device)]
    barrier <-
      if version == 1
        then do
          frame <- receiveFrame reader peer
          assertEqual "discovery sync" (Opcode 0) (rawFrameOpcode frame)
          Just <$> decodeNewId frame
        else pure Nothing
    sendFrames
      peer
      [ encodedFrame device (Opcode 1) (putUint 0)
      , encodedFrame device (Opcode 2) (putString "fixture")
      , encodedFrame manager (Opcode 2) (pure ())
      ]
    -- A manage round trip proves metadata alone has not emitted a setter.
    receiveSequenceCompletion reader peer manager (Opcode 2)
    case barrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 2]
      Nothing -> sendFrames peer [encodedFrame device (Opcode 3) (pure ())]
    receiveExpectedRequest reader peer device (Opcode 2) (putInt 37 >> putInt 410)
    sendFrames peer [encodedFrame device (Opcode 0) (pure ())]
    receiveExpectedRequest reader peer device (Opcode 0) (pure ())
    pointer <- requireObjectId 0xff000002
    sendFrames peer [encodedFrame input (Opcode 1) (putNewId pointer)]
    pointerBarrier <- if version == 1 then Just <$> (receiveFrame reader peer >>= decodeNewId) else pure Nothing
    sendFrames peer [encodedFrame pointer (Opcode 1) (putUint 1), encodedFrame pointer (Opcode 2) (putString "pointer")]
    case pointerBarrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 3]
      Nothing -> sendFrames peer [encodedFrame pointer (Opcode 3) (pure ())]
    receiveExpectedRequest reader peer pointer (Opcode 3) (putFixed (fixedFromRaw 384))
    receiveExpectedRequest reader peer pointer (Opcode 4) (putUint 0)
    receiveExpectedRequest reader peer pointer (Opcode 5) (putInt (-10) >> putInt 20 >> putInt 100 >> putInt 200)
    sendFrames peer [encodedFrame pointer (Opcode 0) (pure ())]
    receiveExpectedRequest reader peer pointer (Opcode 0) (pure ())
    closeTransport peer
  let handlers = (runtimeHandlers (\_ -> pure ())){riverInputRules = [InputRule patternValue Nothing [RepeatInfo 37 410, ScrollFactor 1.5, MapOutput Nothing, MapRectangle (Just (InputRectangle (-10) 20 100 200))]]}
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
  assertEqual "runtime clean EOF" (Right RiverEnded) =<< runRiverSession session
  expectMockSuccess mockDone
  readRiverInputSnapshot session >>= \case
    InputUnavailable _ -> assertFailure "input manager unexpectedly unavailable"
    InputAvailable inventory -> assertEqual "removed generations released" [] (map InputState.inputLifecycle (Map.elems (InputState.inputDevices inventory)))

testInputCohort :: Assertion
testInputCohort = withRuntimePair $ \target peer -> do
  patternValue <- expectRight (compileDevicePattern "*")
  mockDone <- runMock $ do
    reader <- newFrameReader
    (registry, callback) <- receiveStartupRequests reader peer
    sendFrames peer [registryGlobalFrame registry 7 "river_window_manager_v1" 4, registryGlobalFrame registry 8 "river_xkb_bindings_v1" 3, registryGlobalFrame registry 9 "river_layer_shell_v1" 1, registryGlobalFrame registry 10 "river_input_manager_v1" 1, callbackDoneFrame callback 1]
    input <- receiveGlobalBind reader peer "input" 10 "river_input_manager_v1" 1
    (manager, _, _) <- receiveRuntimeBinds reader peer
    a <- requireObjectId 0xff000001
    b <- requireObjectId 0xff000002
    let announce device = sendFrames peer [encodedFrame input (Opcode 1) (putNewId device), encodedFrame device (Opcode 1) (putUint 0), encodedFrame device (Opcode 2) (putString "fixture")]
    announce a
    callbackA <- receiveFrame reader peer >>= decodeNewId
    announce b
    callbackB <- receiveFrame reader peer >>= decodeNewId
    sendFrames peer [encodedFrame a (Opcode 0) (pure ())]
    receiveExpectedRequest reader peer a (Opcode 0) (pure ())
    sendFrames peer [callbackDoneFrame callbackA 2, encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    sendFrames peer [callbackDoneFrame callbackB 3]
    receiveExpectedRequest reader peer b (Opcode 2) (putInt 37 >> putInt 410)
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit ((runtimeHandlers (\_ -> pure ())){riverInputRules = [InputRule patternValue Nothing [RepeatInfo 37 410]]})
  assertEqual "cohort EOF" (Right RiverEnded) =<< runRiverSession session
  expectMockSuccess mockDone

testInputUnavailable :: Assertion
testInputUnavailable = withRuntimePair $ \target peer -> do
  patternValue <- expectRight (compileDevicePattern "*")
  let rules = [InputRule patternValue Nothing [RepeatInfo 37 410]]
  mockDone <- runMock $ do
    reader <- newFrameReader
    (manager, _, _) <- handshake reader peer
    sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit ((runtimeHandlers (\_ -> pure ())){riverInputRules = rules})
  assertEqual "WM usable without input manager" (Right RiverEnded) =<< runRiverSession session
  assertEqual "requested policy unavailable" (InputUnavailable rules) =<< readRiverInputSnapshot session
  expectMockSuccess mockDone

testLibinput :: Word32 -> Int -> Assertion
testLibinput version offset = withRuntimePair $ \target peer -> do
  patternValue <- expectRight (compileDevicePattern "*")
  let settings = [SendEvents EventsDisabled, Tap True, TapButtonMap LeftMiddleRight, Drag True, DragLock DragLockSticky, ThreeFingerDrag FourFingerDragEnabled, CalibrationMatrix [1, 0, 0, 0, 1, 0], Acceleration (AccelerationProfile AccelAdaptive), AccelSpeed 0.25, NaturalScroll True, LeftHanded True, ClickMethod ClickFinger, ClickfingerButtonMap LeftMiddleRight, MiddleEmulation True, ScrollMethod ScrollOnButton, ScrollButton 274, ScrollButtonLock True, DisableWhileTyping True, DisableWhileTrackpoint True, Rotation 90]
  mockDone <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (registry, callback) <- receiveStartupRequests reader peer
    sendFrames peer [registryGlobalFrame registry 7 "river_window_manager_v1" 4, registryGlobalFrame registry 8 "river_xkb_bindings_v1" 3, registryGlobalFrame registry 9 "river_layer_shell_v1" 1, registryGlobalFrame registry 10 "river_input_manager_v1" version, registryGlobalFrame registry 11 "river_libinput_config_v1" version, callbackDoneFrame callback 1]
    input <- receiveGlobalBind reader peer "generic first" 10 "river_input_manager_v1" version
    libinput <- receiveGlobalBind reader peer "libinput second" 11 "river_libinput_config_v1" version
    (manager, _, _) <- receiveRuntimeBinds reader peer
    generic <- requireObjectId 0xff000001
    device <- requireObjectId 0xff000002
    sendFrames peer [encodedFrame input (Opcode 1) (putNewId generic), encodedFrame generic (Opcode 1) (putUint 1), encodedFrame generic (Opcode 2) (putString "synthetic libinput peer"), encodedFrame libinput (Opcode 1) (putNewId device), encodedFrame device (Opcode 1) (putObject generic)]
    sendFrames peer [encodedFrame device (Opcode 2) (putUint 3)]
    sendFrames peer [encodedFrame device (Opcode 5) (putInt 3)]
    sendFrames peer [encodedFrame device (Opcode 14) (putInt 4)]
    sendFrames peer [encodedFrame device (Opcode 17) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 20) (putUint 7)]
    sendFrames peer [encodedFrame device (Opcode 25) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 28) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 31) (putUint 3)]
    sendFrames peer [encodedFrame device (Opcode 36) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 39) (putUint 7)]
    sendFrames peer [encodedFrame device (Opcode 46) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 49) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 52) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 6) (putUint 1)]
    absence <- receiveFrame reader peer >>= decodeNewId
    genericBarrier <- if version == 1 then Just <$> (receiveFrame reader peer >>= decodeNewId) else pure Nothing
    libBarrier <- if version == 1 then Just <$> (receiveFrame reader peer >>= decodeNewId) else pure Nothing
    sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    sendFrames peer [encodedFrame manager (Opcode 3) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 4)
    sendFrames peer [callbackDoneFrame absence 2]
    case genericBarrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 3]
      Nothing -> sendFrames peer [encodedFrame generic (Opcode 3) (pure ())]
    case libBarrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 4]
      Nothing -> sendFrames peer [encodedFrame device (Opcode 55) (pure ())]
    result0 <- receiveLibinputRequest reader peer device (Opcode 1) (putUint 1)
    sendFrames peer [encodedFrame result0 (Opcode (fromIntegral ((0 + offset) `mod` 3))) (pure ())]
    result1 <- receiveLibinputRequest reader peer device (Opcode 2) (putUint 1)
    sendFrames peer [encodedFrame result1 (Opcode (fromIntegral ((1 + offset) `mod` 3))) (pure ())]
    result2 <- receiveLibinputRequest reader peer device (Opcode 3) (putUint 1)
    sendFrames peer [encodedFrame result2 (Opcode (fromIntegral ((2 + offset) `mod` 3))) (pure ())]
    result3 <- receiveLibinputRequest reader peer device (Opcode 4) (putUint 1)
    sendFrames peer [encodedFrame result3 (Opcode (fromIntegral ((3 + offset) `mod` 3))) (pure ())]
    result4 <- receiveLibinputRequest reader peer device (Opcode 5) (putUint 2)
    sendFrames peer [encodedFrame result4 (Opcode (fromIntegral ((4 + offset) `mod` 3))) (pure ())]
    result5 <- receiveLibinputRequest reader peer device (Opcode 6) (putUint 2)
    sendFrames peer [encodedFrame result5 (Opcode (fromIntegral ((5 + offset) `mod` 3))) (pure ())]
    result6 <- receiveLibinputRequest reader peer device (Opcode 7) (putArray (encodeFloats [1, 0, 0, 0, 1, 0]))
    sendFrames peer [encodedFrame result6 (Opcode (fromIntegral ((6 + offset) `mod` 3))) (pure ())]
    result7 <- receiveLibinputRequest reader peer device (Opcode 8) (putUint 2)
    sendFrames peer [encodedFrame result7 (Opcode (fromIntegral ((7 + offset) `mod` 3))) (pure ())]
    result8 <- receiveLibinputRequest reader peer device (Opcode 9) (putArray (encodeDoubles [0.25]))
    sendFrames peer [encodedFrame result8 (Opcode (fromIntegral ((8 + offset) `mod` 3))) (pure ())]
    result9 <- receiveLibinputRequest reader peer device (Opcode 11) (putUint 1)
    sendFrames peer [encodedFrame result9 (Opcode (fromIntegral ((9 + offset) `mod` 3))) (pure ())]
    result10 <- receiveLibinputRequest reader peer device (Opcode 12) (putUint 1)
    sendFrames peer [encodedFrame result10 (Opcode (fromIntegral ((10 + offset) `mod` 3))) (pure ())]
    result11 <- receiveLibinputRequest reader peer device (Opcode 13) (putUint 2)
    sendFrames peer [encodedFrame result11 (Opcode (fromIntegral ((11 + offset) `mod` 3))) (pure ())]
    result12 <- receiveLibinputRequest reader peer device (Opcode 14) (putUint 1)
    sendFrames peer [encodedFrame result12 (Opcode (fromIntegral ((12 + offset) `mod` 3))) (pure ())]
    result13 <- receiveLibinputRequest reader peer device (Opcode 15) (putUint 1)
    sendFrames peer [encodedFrame result13 (Opcode (fromIntegral ((13 + offset) `mod` 3))) (pure ())]
    result14 <- receiveLibinputRequest reader peer device (Opcode 16) (putUint 4)
    sendFrames peer [encodedFrame result14 (Opcode (fromIntegral ((14 + offset) `mod` 3))) (pure ())]
    result15 <- receiveLibinputRequest reader peer device (Opcode 17) (putUint 274)
    sendFrames peer [encodedFrame result15 (Opcode (fromIntegral ((15 + offset) `mod` 3))) (pure ())]
    result16 <- receiveLibinputRequest reader peer device (Opcode 18) (putUint 1)
    sendFrames peer [encodedFrame result16 (Opcode (fromIntegral ((16 + offset) `mod` 3))) (pure ())]
    result17 <- receiveLibinputRequest reader peer device (Opcode 19) (putUint 1)
    sendFrames peer [encodedFrame result17 (Opcode (fromIntegral ((17 + offset) `mod` 3))) (pure ())]
    result18 <- receiveLibinputRequest reader peer device (Opcode 20) (putUint 1)
    sendFrames peer [encodedFrame result18 (Opcode (fromIntegral ((18 + offset) `mod` 3))) (pure ())]
    result19 <- receiveLibinputRequest reader peer device (Opcode 21) (putUint 90)
    sendFrames peer [encodedFrame result19 (Opcode (fromIntegral ((19 + offset) `mod` 3))) (pure ())]

    sendFrames peer [encodedFrame device (Opcode 7) (putUint 0)]
    if version == 2 then sendFrames peer [encodedFrame device (Opcode 55) (pure ())] else pure ()
    sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit ((runtimeHandlers (\_ -> pure ())){riverInputRules = [InputRule patternValue Nothing settings]})
  assertEqual "clean synthetic peer EOF" (Right RiverEnded) =<< runRiverSession session
  expectMockSuccess mockDone
  readRiverInputSnapshot session >>= \case
    InputUnavailable _ -> assertFailure "missing inventory"
    InputAvailable inventory -> case Map.elems (InputState.inputDevices inventory) of
      [device] -> do
        assertEqual "all result acknowledgements retained" [case (index + offset) `mod` 3 of { 0 -> InputState.SettingApplied; 1 -> InputState.SettingUnsupported; _ -> InputState.SettingRejectedInvalid } | index <- [0 .. 19]] (map InputState.settingOutcome (Map.elems (InputState.inputResults device)))
        assertEqual "feedback separate from desired" (Just (Tap False)) (Map.lookup TapKey (InputState.inputCurrent device))
        assertEqual "defaults separate from current" (Just (Tap True)) (Map.lookup TapKey (InputState.inputDefaults device))
      _ -> assertFailure "unexpected canonical devices"

receiveLibinputRequest :: FrameReader -> Transport -> ObjectId -> Opcode -> SekiArguments.ArgumentEncoder () () -> IO ObjectId
receiveLibinputRequest reader peer sender opcode arguments = do
  frame <- receiveFrame reader peer
  assertEqual "libinput setter sender" sender (rawFrameSender frame)
  assertEqual "libinput setter opcode" opcode (rawFrameOpcode frame)
  assertEqual "result id present" True (BS.length (rawFramePayload frame) >= 4)
  identifier <- requireObjectId (Get.runGet Get.getWord32host (Lazy.fromStrict (BS.take 4 (rawFramePayload frame))))
  payload <- encodedPayload (putNewId identifier >> arguments)
  assertEqual "exact libinput setter bytes" payload (rawFramePayload frame)
  pure identifier

testCustomLibinput :: Word32 -> Word32 -> Assertion
testCustomLibinput version outcome = withRuntimePair $ \target peer -> do
  patternValue <- expectRight (compileDevicePattern "*")
  let settings = [Acceleration (AccelerationCurves [AccelCurve CurveMotion 0.5 [0, 1, 2]])]
  mockDone <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (registry, callback) <- receiveStartupRequests reader peer
    sendFrames peer [registryGlobalFrame registry 7 "river_window_manager_v1" 4, registryGlobalFrame registry 8 "river_xkb_bindings_v1" 3, registryGlobalFrame registry 9 "river_layer_shell_v1" 1, registryGlobalFrame registry 10 "river_input_manager_v1" version, registryGlobalFrame registry 11 "river_libinput_config_v1" version, callbackDoneFrame callback 1]
    input <- receiveGlobalBind reader peer "generic first" 10 "river_input_manager_v1" version
    libinput <- receiveGlobalBind reader peer "libinput second" 11 "river_libinput_config_v1" version
    (manager, _, _) <- receiveRuntimeBinds reader peer
    generic <- requireObjectId 0xff000001
    device <- requireObjectId 0xff000002
    sendFrames peer [encodedFrame input (Opcode 1) (putNewId generic), encodedFrame generic (Opcode 1) (putUint 1), encodedFrame generic (Opcode 2) (putString "synthetic libinput peer"), encodedFrame libinput (Opcode 1) (putNewId device), encodedFrame device (Opcode 1) (putObject generic)]
    sendFrames peer [encodedFrame device (Opcode 2) (putUint 3)]
    sendFrames peer [encodedFrame device (Opcode 5) (putInt 3)]
    sendFrames peer [encodedFrame device (Opcode 14) (putInt 4)]
    sendFrames peer [encodedFrame device (Opcode 17) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 20) (putUint 7)]
    sendFrames peer [encodedFrame device (Opcode 25) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 28) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 31) (putUint 3)]
    sendFrames peer [encodedFrame device (Opcode 36) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 39) (putUint 7)]
    sendFrames peer [encodedFrame device (Opcode 46) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 49) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 52) (putInt 1)]
    sendFrames peer [encodedFrame device (Opcode 6) (putUint 1)]
    absence <- receiveFrame reader peer >>= decodeNewId
    genericBarrier <- if version == 1 then Just <$> (receiveFrame reader peer >>= decodeNewId) else pure Nothing
    libBarrier <- if version == 1 then Just <$> (receiveFrame reader peer >>= decodeNewId) else pure Nothing
    sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    sendFrames peer [encodedFrame manager (Opcode 3) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 4)
    sendFrames peer [callbackDoneFrame absence 2]
    case genericBarrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 3]
      Nothing -> sendFrames peer [encodedFrame generic (Opcode 3) (pure ())]
    case libBarrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 4]
      Nothing -> sendFrames peer [encodedFrame device (Opcode 55) (pure ())]
    config <- receiveLibinputRequest reader peer libinput (Opcode 2) (putUint 4)
    pointResult <- receiveLibinputRequest reader peer config (Opcode 1) (putUint 1 >> putArray (encodeDoubles [0.5]) >> putArray (encodeDoubles [0, 1, 2]))
    if outcome >= 3
      then do
        if outcome == 3 then sendFrames peer [encodedFrame generic (Opcode 0) (pure ())] else sendFrames peer [encodedFrame libinput (Opcode 0) (pure ())]
        receiveExpectedRequest reader peer device (Opcode 0) (pure ())
        if outcome == 3 then receiveExpectedRequest reader peer generic (Opcode 0) (pure ()) else receiveExpectedRequest reader peer libinput (Opcode 1) (pure ())
        sendFrames peer [encodedFrame pointResult (Opcode 0) (pure ())]
        receiveExpectedRequest reader peer config (Opcode 0) (pure ())
      else do
        sendFrames peer [encodedFrame pointResult (Opcode (fromIntegral outcome)) (pure ())]
        if outcome == 0
          then do
            applied <- receiveLibinputRequest reader peer device (Opcode 10) (putObject config)
            receiveExpectedRequest reader peer config (Opcode 0) (pure ())
            sendFrames peer [encodedFrame applied (Opcode 0) (pure ())]
          else receiveExpectedRequest reader peer config (Opcode 0) (pure ())
    sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit ((runtimeHandlers (\_ -> pure ())){riverInputRules = [InputRule patternValue Nothing settings]})
  assertEqual "custom peer clean EOF" (Right RiverEnded) =<< runRiverSession session
  expectMockSuccess mockDone
  readRiverInputSnapshot session >>= \case
    InputUnavailable _ -> assertFailure "missing custom inventory"
    InputAvailable inventory ->
      if outcome == 3
        then assertEqual "removed generation released before late result" [] (Map.elems (InputState.inputDevices inventory))
        else assertEqual "only final acknowledgement applies custom configuration" [case outcome of { 0 -> InputState.SettingApplied; 1 -> InputState.SettingUnsupported; 4 -> InputState.SettingUnavailable; _ -> InputState.SettingRejectedInvalid }] [InputState.settingOutcome result | device <- Map.elems (InputState.inputDevices inventory), result <- Map.elems (InputState.inputResults device)]

testXkbConfig :: Word32 -> Assertion
testXkbConfig version = withRuntimePair $ \target peer -> do
  patternValue <- expectRight (compileDevicePattern "*")
  mockDone <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (registry, callback) <- receiveStartupRequests reader peer
    let genericVersion = min version 2
    sendFrames peer [registryGlobalFrame registry 7 "river_window_manager_v1" 4, registryGlobalFrame registry 8 "river_xkb_bindings_v1" 3, registryGlobalFrame registry 9 "river_layer_shell_v1" 1, registryGlobalFrame registry 10 "river_input_manager_v1" genericVersion, registryGlobalFrame registry 11 "river_xkb_config_v1" version, callbackDoneFrame callback 1]
    input <- receiveGlobalBind reader peer "generic first" 10 "river_input_manager_v1" genericVersion
    xkb <- receiveGlobalBind reader peer "XKB config follows generic" 11 "river_xkb_config_v1" version
    (manager, _, _) <- receiveRuntimeBinds reader peer
    generic <- requireObjectId 0xff000001
    keyboard <- requireObjectId 0xff000002
    sendFrames peer [encodedFrame input (Opcode 1) (putNewId generic), encodedFrame generic (Opcode 1) (putUint 1), encodedFrame generic (Opcode 2) (putString "synthetic XKB peer"), encodedFrame xkb (Opcode 1) (putNewId keyboard), encodedFrame keyboard (Opcode 1) (putObject generic), encodedFrame keyboard (Opcode 2) (putUint 0 >> putString "English"), encodedFrame keyboard (Opcode 4) (pure ()), encodedFrame keyboard (Opcode 6) (pure ())]
    absence <- receiveFrame reader peer >>= decodeNewId
    genericBarrier <- if version == 1 then Just <$> (receiveFrame reader peer >>= decodeNewId) else pure Nothing
    xkbBarrier <- if version == 1 then Just <$> (receiveFrame reader peer >>= decodeNewId) else pure Nothing
    sendFrames peer [callbackDoneFrame absence 2]
    case genericBarrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 3]
      Nothing -> sendFrames peer [encodedFrame generic (Opcode 3) (pure ())]
    sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    sendFrames peer [encodedFrame manager (Opcode 3) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 4)
    case xkbBarrier of
      Just cb -> sendFrames peer [callbackDoneFrame cb 4]
      Nothing -> sendFrames peer [encodedFrame keyboard (Opcode 7) (pure ())]
    receiveExpectedRequest reader peer keyboard (Opcode 3) (putString "Japanese")
    receiveExpectedRequest reader peer keyboard (Opcode 4) (pure ())
    receiveExpectedRequest reader peer keyboard (Opcode 7) (pure ())
    if version == 3 then receiveExpectedRequest reader peer keyboard (Opcode 8) (pure ()) else pure ()
    sendFrames peer [encodedFrame keyboard (Opcode 2) (putUint 1 >> putString "Japanese"), encodedFrame keyboard (Opcode 3) (pure ()), encodedFrame keyboard (Opcode 6) (pure ())]
    if version >= 2 then sendFrames peer [encodedFrame keyboard (Opcode 7) (pure ())] else pure ()
    sendFrames peer [encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    closeTransport peer
  let settings = [ActiveLayout (LayoutByName "Japanese"), CapsLock True, NumLock False, ScrollLock True]
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit ((runtimeHandlers (\_ -> pure ())){riverInputRules = [InputRule patternValue Nothing settings]})
  assertEqual "mock EOF" (Right RiverEnded) =<< runRiverSession session
  expectMockSuccess mockDone
  readRiverInputSnapshot session >>= \case
    InputUnavailable _ -> assertFailure "missing mock inventory"
    InputAvailable inventory -> case Map.elems (InputState.inputDevices inventory) of
      [device] -> do
        assertEqual "layout is per device" (Just (1, Just "Japanese")) (xkbLayout (xkbCurrent (InputState.inputXkb device)))
        assertEqual "observed, never inferred applied" (Just InputState.SettingObserved) (InputState.settingOutcome <$> Map.lookup ActiveLayoutKey (InputState.inputResults device))
        assertEqual "version gate" (Just (if version == 3 then InputState.SettingSent else InputState.SettingUnsupported)) (InputState.settingOutcome <$> Map.lookup ScrollLockKey (InputState.inputResults device))
      _ -> assertFailure "wrong mock inventory"

testTwoXkbKeyboards :: Assertion
testTwoXkbKeyboards = withRuntimePair $ \target peer -> do
  mockDone <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (registry, callback) <- receiveStartupRequests reader peer
    sendFrames peer [registryGlobalFrame registry 7 "river_window_manager_v1" 4, registryGlobalFrame registry 8 "river_xkb_bindings_v1" 3, registryGlobalFrame registry 9 "river_layer_shell_v1" 1, registryGlobalFrame registry 10 "river_input_manager_v1" 2, registryGlobalFrame registry 11 "river_xkb_config_v1" 2, callbackDoneFrame callback 1]
    input <- receiveGlobalBind reader peer "generic" 10 "river_input_manager_v1" 2
    xkb <- receiveGlobalBind reader peer "XKB config" 11 "river_xkb_config_v1" 2
    (manager, _, _) <- receiveRuntimeBinds reader peer
    let announce number name layout = do
          generic <- requireObjectId number
          keyboard <- requireObjectId (number + 1)
          sendFrames peer [encodedFrame input (Opcode 1) (putNewId generic), encodedFrame generic (Opcode 1) (putUint 1), encodedFrame generic (Opcode 2) (putString name), encodedFrame generic (Opcode 3) (pure ()), encodedFrame xkb (Opcode 1) (putNewId keyboard), encodedFrame keyboard (Opcode 1) (putObject generic), encodedFrame keyboard (Opcode 2) (putUint layout >> putString name), encodedFrame keyboard (Opcode 7) (pure ())]
    announce 0xff000001 "English" 0
    announce 0xff000003 "Japanese" 1
    first <- receiveFrame reader peer >>= decodeNewId
    second <- receiveFrame reader peer >>= decodeNewId
    sendFrames peer [callbackDoneFrame first 2, callbackDoneFrame second 3, encodedFrame manager (Opcode 2) (pure ())]
    receiveSequenceCompletion reader peer manager (Opcode 2)
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (runtimeHandlers (\_ -> pure ()))
  assertEqual "WM unchanged with empty rules" (Right RiverEnded) =<< runRiverSession session
  expectMockSuccess mockDone
  readRiverInputSnapshot session >>= \case
    InputUnavailable _ -> assertFailure "missing inventory"
    InputAvailable inventory -> do
      let devices = Map.elems (InputState.inputDevices inventory)
      assertEqual "each canonical generation retains its own state" [Just (0, Just "English"), Just (1, Just "Japanese")] (map (xkbLayout . xkbCurrent . InputState.inputXkb) devices)
      assertBool "no results invented for empty rules" (all (Map.null . InputState.inputResults) devices)
