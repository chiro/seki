{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.ReconcileTest (tests) where

import Control.Exception (onException)
import Data.Set qualified as Set
import Seki.Internal.Core.Action (sendMessage, windows)
import Seki.Internal.Core.Layout (ChangeLayout (..), (|||))
import Seki.Internal.Core.ManageHook (idHook)
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Core.StackSet (WindowId (..), view)
import Seki.Internal.Core.State (CoreState (..))
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.MockRiver (FrameReader, callbackDoneFrame, encodedFrame, expectMockSuccess, expectRight, newFrameReader, receiveExpectedRequest, receiveKeyBindingRequest, receiveLayerShellOutputRequest, receiveLayerShellSeatRequest, receiveNewIdRequest, receiveRuntimeBinds, receiveStartupRequests, registryGlobalFrame, requireObjectId, runMock, sendFrames, withRuntimePair)
import Seki.Run.Policy (PolicyConfig (..), newPolicy, policyRuntimeHandlers, readPolicyState)
import Seki.Run.Runtime (openRiverSessionAt, runRiverSession)
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Arguments (putInt, putNewId, putNullableObject, putObject, putUint)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (Transport, closeTransport)
import Seki.Wire.Types (ObjectId, Opcode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "scene protocol reconciliation"
    [ testCase "serializes proposals, restores focus, accepts client size mismatch, and honors fullscreen but not maximize or minimize requests" testSettlement
    , testCase "retains omitted dimensions across layouts and prunes a pending close" testLayouts
    , testCase "respects layer focus and recovers on every focus_none report" testLayerFocus
    , testCase "flags a single Tall window tiled on every edge" testSingleTall
    ]

testSettlement :: Assertion
testSettlement = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000030
  output <- requireObjectId 0xff000031
  seat <- requireObjectId 0xff000032
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    let request sender opcode args = receiveExpectedRequest reader peer sender (Opcode opcode) args
        event sender opcode args = encodedFrame sender (Opcode opcode) args
        start = sendFrames peer [event manager 2 (pure ())]
        render = sendFrames peer [event manager 3 (pure ())]
        area ls width = event ls 0 (putInt (-100) >> putInt 20 >> putInt width >> putInt 80)
        dimensions width = event window 2 (putInt width >> putInt 76)
        manageEnd ls = request ls 1 (pure ()) >> request manager 2 (pure ())
        dirty = request manager 3 (pure ())
        border = request window 8 (putUint 15 >> putInt 2 >> putUint 0 >> putUint 0x80000000 >> putUint 0xffffffff >> putUint 0xffffffff)
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    node <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 1000 >> putInt 1000)]
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    let stack = request node 2 (pure ())
        position = request node 1 (putInt (-98) >> putInt 22)
    sendFrames peer [dimensions 96, area ls 100]
    start
    request window 7 (pure ())
    request window 14 (putUint 4)
    request window 9 (putUint 15)
    request window 3 (putInt 96 >> putInt 76)
    request ls 1 (pure ())
    request seat 3 (pure ())
    request manager 2 (pure ())
    render
    stack
    request window 4 (pure ())
    request manager 4 (pure ())
    sendFrames peer [area ls 120]
    start
    manageEnd ls
    render

    request manager 4 (pure ())
    sendFrames peer [dimensions 96]
    render
    position

    border
    request window 5 (pure ())
    request manager 4 (pure ())
    dirty
    start
    request window 3 (putInt 116 >> putInt 76)
    manageEnd ls
    render

    request manager 4 (pure ())
    sendFrames peer [dimensions 116]
    render
    position

    request manager 4 (pure ())
    dirty
    start
    request ls 1 (pure ())
    request seat 1 (putObject window)
    request manager 2 (pure ())
    render

    request manager 4 (pure ())
    sendFrames peer [area ls 140]
    start
    request window 3 (putInt 136 >> putInt 76)
    request ls 1 (pure ())
    request seat 3 (pure ())
    request manager 2 (pure ())
    render

    request manager 4 (pure ())
    sendFrames peer [dimensions 130]
    render
    position

    request manager 4 (pure ())
    dirty
    start
    request ls 1 (pure ())
    request seat 1 (putObject window)
    request manager 2 (pure ())
    render

    request manager 4 (pure ())
    start
    manageEnd ls
    render

    request manager 4 (pure ())
    sendFrames peer [area ls 200]
    start
    request window 3 (putInt 196 >> putInt 76)
    request ls 1 (pure ())
    request seat 3 (pure ())
    request manager 2 (pure ())
    render
    request manager 4 (pure ())
    sendFrames peer [area ls 220]
    start
    manageEnd ls
    render
    request manager 4 (pure ())
    sendFrames peer [area ls 200]
    start
    manageEnd ls
    render
    request manager 4 (pure ())
    sendFrames peer [area ls 0]
    start
    manageEnd ls
    render
    request window 4 (pure ())
    request manager 4 (pure ())
    sendFrames peer [dimensions 196]
    render
    request manager 4 (pure ())
    sendFrames peer [area ls 200]
    start
    manageEnd ls
    render
    position
    request window 5 (pure ())
    request manager 4 (pure ())
    dirty
    start
    request ls 1 (pure ())
    request seat 1 (putObject window)
    request manager 2 (pure ())
    render
    request manager 4 (pure ())
    unsupportedBefore <- readPolicyState policy
    sendFrames
      peer
      [ event window 7 (putObject seat)
      , event window 8 (putObject seat >> putUint 15)
      , event window 9 (putInt 4 >> putInt 5)
      , event window 10 (pure ())
      , event window 11 (pure ())
      , event window 14 (pure ())
      ]
    start
    manageEnd ls
    unsupportedAfter <- readPolicyState policy
    unsupportedAfter @?= unsupportedBefore
    render
    request manager 4 (pure ())
    sendFrames peer [event window 12 (putNullableObject Nothing)]
    start
    request window 19 (putObject output)
    request window 17 (pure ())
    manageEnd ls
    fullscreen <- readPolicyState policy
    coreFullscreen fullscreen @?= Set.singleton (WindowId 0xff000030)
    render
    request manager 4 (pure ())
    sendFrames peer [event window 13 (pure ())]
    start
    request window 20 (pure ())
    request window 18 (pure ())
    request window 3 (putInt 196 >> putInt 76)
    request ls 1 (pure ())
    request seat 3 (pure ())
    request manager 2 (pure ())
    sendFrames peer [dimensions 196]
    render
    position
    request manager 4 (pure ())
    dirty
    start
    request ls 1 (pure ())
    request seat 1 (putObject window)
    request manager 2 (pure ())
    render
    request manager 4 (pure ())
    sendFrames peer [event window 0 (pure ()), event manager 6 (putNewId window)]
    request node 0 (pure ())
    request window 0 (pure ())
    replacementNode <- receiveNewIdRequest reader peer window (Opcode 2)
    start
    request window 7 (pure ())
    request window 14 (putUint 4)
    request window 9 (putUint 15)
    request window 3 (putInt 196 >> putInt 76)
    request ls 1 (pure ())
    request seat 3 (pure ())
    request manager 2 (pure ())
    render
    request replacementNode 2 (pure ())
    request window 4 (pure ())
    request manager 4 (pure ())
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()

testLayerFocus :: Assertion
testLayerFocus = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000040
  output <- requireObjectId 0xff000041
  seat <- requireObjectId 0xff000042
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    let request sender opcode args = receiveExpectedRequest reader peer sender (Opcode opcode) args
        event sender opcode args = encodedFrame sender (Opcode opcode) args
        start = sendFrames peer [event manager 2 (pure ())]
        render = sendFrames peer [event manager 3 (pure ())]
        border focused = request window 8 (putUint 15 >> putInt 2 >> mapM_ putUint (if focused then [0, 0x80000000, 0xffffffff, 0xffffffff] else [0x40000000, 0x40000000, 0x40000000, 0xffffffff]))
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    node <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 1000 >> putInt 1000)]
    layerSeat <- receiveLayerShellSeatRequest reader peer layerShell seat
    layoutBinding <- receiveKeyBindingRequest reader peer xkb seat layoutKey
    focusBinding <- receiveKeyBindingRequest reader peer xkb seat focusKey
    let stack = request node 2 (pure ())
        manageEnd = request ls 1 (pure ()) >> request manager 2 (pure ())
        renderEnd = request manager 4 (pure ())
        layer opcode = sendFrames peer [event layerSeat opcode (pure ())]
        focusedManage = request ls 1 (pure ()) >> request seat 1 (putObject window) >> request manager 2 (pure ())
    sendFrames peer [event ls 0 (putInt 0 >> putInt 0 >> putInt 100 >> putInt 80)]
    start
    request layoutBinding 2 (pure ())
    request focusBinding 2 (pure ())
    request window 7 (pure ())
    request window 14 (putUint 4)
    request window 9 (putUint 15)
    request window 3 (putInt 96 >> putInt 76)
    request ls 1 (pure ())
    request seat 3 (pure ())
    request manager 2 (pure ())
    sendFrames peer [event window 2 (putInt 96 >> putInt 76)]
    render
    request node 1 (putInt 2 >> putInt 2)
    stack
    border True
    request window 5 (pure ())
    request manager 4 (pure ())
    request manager 3 (pure ())
    start
    focusedManage
    render
    renderEnd
    layer 0
    start
    manageEnd
    render

    border False
    request manager 4 (pure ())
    sendFrames peer [event focusBinding 0 (pure ())]
    start
    manageEnd
    render
    renderEnd
    sendFrames peer [event seat 4 (putObject window)]
    start
    manageEnd
    render
    renderEnd
    layer 2
    start
    focusedManage
    render

    border True
    request manager 4 (pure ())
    layer 2
    start
    focusedManage
    render
    renderEnd
    layer 1
    start
    manageEnd
    render

    border False
    request manager 4 (pure ())
    sendFrames peer [event layoutBinding 0 (pure ())]
    start
    manageEnd
    render
    renderEnd
    sendFrames peer [event focusBinding 0 (pure ())]
    start
    focusedManage
    render

    border True
    request manager 4 (pure ())
    layer 2
    start
    focusedManage
    render
    renderEnd
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1 :: Int] [(layoutKey, sendMessage NextLayout), (focusKey, windows id)] defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()
 where
  layoutKey = RiverKeyBinding 0x61 (River.RiverSeatV1ModifiersFlags 0) Nothing
  focusKey = RiverKeyBinding 0x62 (River.RiverSeatV1ModifiersFlags 0) Nothing

testLayouts :: Assertion
testLayouts = withRuntimePair $ \target peer -> do
  w1 <- requireObjectId 0xff000050
  w2 <- requireObjectId 0xff000051
  output <- requireObjectId 0xff000052
  seat <- requireObjectId 0xff000053
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    let request sender opcode args = receiveExpectedRequest reader peer sender (Opcode opcode) args
        event sender opcode args = encodedFrame sender (Opcode opcode) args
        start = sendFrames peer [event manager 2 (pure ())]
        render = sendFrames peer [event manager 3 (pure ())]
        end = request manager 4 (pure ())
        dirty = request manager 3 (pure ())
        dimensions w width = event w 2 (putInt width >> putInt 96)
        proposal w width = request w 3 (putInt width >> putInt 96)
        border w focused = request w 8 (putUint 15 >> putInt 2 >> mapM_ putUint (if focused then [0, 0x80000000, 0xffffffff, 0xffffffff] else [0x40000000, 0x40000000, 0x40000000, 0xffffffff]))
    sendFrames peer [event manager 6 (putNewId w1), event manager 6 (putNewId w2), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    n1 <- receiveNewIdRequest reader peer w1 (Opcode 2)
    n2 <- receiveNewIdRequest reader peer w2 (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 1000 >> putInt 1000)]
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    [cycleBinding, hideBinding, showBinding] <- mapM (receiveKeyBindingRequest reader peer xkb seat) keys
    let manageEnd = request ls 1 (pure ()) >> request manager 2 (pure ())
        focusedManage w = request ls 1 (pure ()) >> request seat 1 (putObject w) >> request manager 2 (pure ())
        clearedManage = request ls 1 (pure ()) >> request seat 3 (pure ()) >> request manager 2 (pure ())
        position n x = request n 1 (putInt x >> putInt 2)
        top n = request n 2 (pure ())
        press binding = sendFrames peer [event binding 0 (pure ())]
    sendFrames peer [event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    start
    mapM_ (\binding -> request binding 2 (pure ())) [cycleBinding, hideBinding, showBinding]
    mapM_ (\w -> request w 7 (pure ()) >> request w 14 (putUint 4)) [w1, w2]
    mapM_ (\w -> request w 9 (putUint 15)) [w1, w2]
    mapM_ (\w -> proposal w 96) [w1, w2]
    clearedManage
    render
    top n2
    top n1
    mapM_ (\w -> request w 4 (pure ())) [w1, w2]
    end
    sendFrames peer [dimensions w1 96, dimensions w2 96]
    render
    position n1 102
    position n2 2
    top n1
    top n2
    border w1 False
    border w2 True
    mapM_ (\w -> request w 5 (pure ())) [w1, w2]
    end
    dirty
    start
    focusedManage w2
    render
    end
    press cycleBinding
    start
    proposal w2 196
    clearedManage
    render
    request w1 4 (pure ())
    end
    sendFrames peer [dimensions w2 196]
    render
    position n2 2
    end
    dirty
    start
    focusedManage w2
    render
    end
    press cycleBinding
    start
    proposal w2 96
    clearedManage
    render
    position n1 102
    request w1 5 (pure ())
    end
    sendFrames peer [event w2 0 (pure ())]
    request n2 0 (pure ())
    request w2 0 (pure ())
    start
    proposal w1 196
    manageEnd
    render
    position n1 2
    top n1
    border w1 True
    end
    sendFrames peer [dimensions w1 196]
    render
    position n1 2
    end
    dirty
    start
    focusedManage w1
    render
    end
    press hideBinding
    start
    clearedManage
    render
    request w1 4 (pure ())
    end
    press showBinding
    start
    manageEnd
    render
    position n1 2
    request w1 5 (pure ())
    end
    dirty
    start
    focusedManage w1
    render
    end
    closeTransport peer
  let actions = zip keys [sendMessage NextLayout, windows (view 2), windows (view 1)]
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 0.03 0.5 ||| Full) [1 :: Int, 2] actions defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()
 where
  keys = [RiverKeyBinding key (River.RiverSeatV1ModifiersFlags 0) Nothing | key <- [0x61, 0x62, 0x63]]

testSingleTall :: Assertion
testSingleTall = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000060
  output <- requireObjectId 0xff000061
  seat <- requireObjectId 0xff000062
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    let request sender opcode args = receiveExpectedRequest reader peer sender (Opcode opcode) args
        event sender opcode args = encodedFrame sender (Opcode opcode) args
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    node <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 1000 >> putInt 1000)]
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    sendFrames peer [event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100), event manager 2 (pure ())]
    request window 7 (pure ())
    request window 14 (putUint 4)
    request window 9 (putUint 15)
    request window 3 (putInt 196 >> putInt 96)
    request ls 1 (pure ())
    request seat 3 (pure ())
    request manager 2 (pure ())
    sendFrames peer [event manager 3 (pure ())]
    request node 2 (pure ())
    request window 4 (pure ())
    request manager 4 (pure ())
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 0.03 0.5) [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()

handshake :: FrameReader -> Transport -> IO (ObjectId, ObjectId, ObjectId)
handshake reader peer = do
  (registry, callback) <- receiveStartupRequests reader peer
  sendFrames
    peer
    [ registryGlobalFrame registry 7 "river_window_manager_v1" 4
    , registryGlobalFrame registry 8 "river_xkb_bindings_v1" 3
    , registryGlobalFrame registry 9 "river_layer_shell_v1" 1
    , callbackDoneFrame callback 1
    ]
  receiveRuntimeBinds reader peer
