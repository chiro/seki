{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.RuntimeTest (tests) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Seki.Internal.Core.Geometry (GeometryError (..), Point (..), mkRect, mkSize)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Default (DefaultConfig (..), defaultConfig, defaultRuntimeHandlers)
import Seki.Run.Full (prepareFullManagePlan, prepareFullRenderPlan)
import Seki.Run.Inventory (
  RiverInventoryError (..),
  RiverOutputState (..),
  RiverRectangle (..),
  RiverSize (..),
  RiverWindowState (..),
  riverInventoryActiveWindow,
  riverInventoryOutputs,
  riverInventoryWindows,
  riverOutputRectangle,
 )
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
  receiveNewIdRequest,
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
    , testCase "executes phase plans before sequence completion" testPhasePlans
    , testCase "Full placement hides windows without an output" testFullWithoutOutput
    , testCase "runs the default focus and close policy" testDefaultPolicy
    , testCase "tracks the window selected by seat interaction" testSeatInteractionFocus
    , testCase "retires removed inventory objects and their nodes" testInventoryRemoval
    , testCase "does not finish a sequence after a phase-plan failure" testPhasePlanFailure
    , testCase "promotes inventory failures before user event delivery" testInventoryFailure
    , testCase "promotes window-node acquisition failures" testNodeAcquisitionFailure
    , testCase "owns per-seat XKB binding objects" testSeatBindingLifecycle
    , testCase "owns one layer-shell output per output and plans with its usable area" testOutputLayerShellLifecycle
    , testCase "promotes invalid usable areas to inventory errors" testInvalidUsableArea
    , testCase "rejects an out-of-order manager sequence event" testSequenceOrderError
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
          (runtimeHandlers (\event -> modifyIORef' managerEvents (<> [event])))
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 93
        ]
      (managerId, _, layerShellId) <- receiveRuntimeBinds reader peer
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
      _ <- receiveLayerShellOutputRequest reader peer layerShellId outputId
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
            , prepareRiverManagePlan = pure . prepareFullManagePlan . riverRuntimeInventory
            , prepareRiverRenderPlan = pure . prepareFullRenderPlan . riverRuntimeInventory
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 95
        ]
      (managerId, _, _) <- receiveRuntimeBinds reader peer
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
            , prepareRiverManagePlan = pure . prepareFullManagePlan . riverRuntimeInventory
            , prepareRiverRenderPlan = pure . prepareFullRenderPlan . riverRuntimeInventory
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 100
        ]
      (managerId, xkbBindingsId, layerShellId) <- receiveRuntimeBinds reader peer
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
      _ <- receiveLayerShellOutputRequest reader peer layerShellId outputId
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 99
        ]
      (managerId, _, _) <- receiveRuntimeBinds reader peer
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
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \_ -> pure (pure ())
            , prepareRiverRenderPlan = \_ -> pure (pure ())
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
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \snapshot ->
                pure $ case riverInventoryWindows (riverRuntimeInventory snapshot) of
                  window : _ -> setWindowDimensionBounds (riverWindowObject window) 1920 1080
                  [] -> pure ()
            , prepareRiverRenderPlan = \_ -> pure (pure ())
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
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \_ -> pure (pure ())
            , prepareRiverRenderPlan = \_ -> pure (pure ())
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
            , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
            , prepareRiverManagePlan = \_ -> pure (pure ())
            , prepareRiverRenderPlan = \_ -> pure (pure ())
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
        , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
        , callbackDoneFrame callbackId 98
        ]
      (managerId, xkbBindingsId, _) <- receiveRuntimeBinds reader peer
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
            , prepareRiverManagePlan = \_ -> pure (pure ())
            , prepareRiverRenderPlan = \_ -> pure (pure ())
            }
    session <- expectRight =<< openRiverSessionAt target defaultObjectLimit handlers
    assertEqual "seat binding lifecycle" (Right ()) =<< runRiverSession session
    expectMockSuccess mockDone
    assertEqual "seat creation and removal callbacks" 2 =<< readIORef seatCallbacks
    assertEqual "routed binding press" [terminalBinding] =<< readIORef bindingPresses
    assertEqual "removed seat bindings" [] =<< readRiverSeatBindings session

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
    , handleRiverKeyBindingEvent = \_ _ _ -> pure noRiverManageAction
    , prepareRiverManagePlan = \_ -> pure (pure ())
    , prepareRiverRenderPlan = \_ -> pure (pure ())
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
