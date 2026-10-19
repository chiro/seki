{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.ReconcileTraceTest (tests) where

import Control.Exception (onException)
import Control.Monad (foldM, forM, forM_, when)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Seki.Internal.Core.Action (float, windows, withFocused)
import Seki.Internal.Core.Layout (LayoutClass, Mirror (..))
import Seki.Internal.Core.ManageHook (doFloat, idHook, isDialog, (-->))
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Core.StackSet (FloatGeom (..), StackSet (..), WindowId (..), focusDown, index, peek)
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.MockRiver (FrameReader, callbackDoneFrame, encodedFrame, encodedPayload, expectMockSuccess, expectRight, newFrameReader, receiveFrame, receiveKeyBindingRequest, receiveLayerShellOutputRequest, receiveLayerShellSeatRequest, receiveNewIdRequest, receiveRuntimeBinds, receiveStartupRequests, registryGlobalFrame, requireObjectId, runMock, sendFrames, withRuntimePair)
import Seki.Run.Policy (PolicyConfig (..), newPolicy, policyRuntimeHandlers, readPolicyState)
import Seki.Run.Runtime (openRiverSessionAt, runRiverSession)
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Arguments (ArgumentEncoder, putInt, putNewId, putObject, putUint)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (Transport, closeTransport)
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (arbitrary, elements, forAll, ioProperty, testProperty, vectorOf, withMaxSuccess)

tests :: TestTree
tests =
  testGroup
    "reconciliation traces"
    [ traceProperty "generated area and dimensions traces preserve proposal, visibility, and focus invariants" Full $ \width ->
        [content width 100]
    , traceProperty "generated Tall traces never propose or show a degenerate tile" (Tall 1 (1 / 10) (1 / 2)) $ \width ->
        [content (width - width `div` 2) 100, content (width `div` 2) 100]
    , traceProperty "generated Mirror Tall traces never propose or show a degenerate tile" (Mirror (Tall 1 (1 / 10) (1 / 2))) $ \width ->
        [content width 50, content width 50]
    , testCase "focus raises a shown family and new windows retain nonexclusive layer focus" testFamilies
    , testCase "M1-D06 superseded: Full keeps a floating dialog centered above its refocused parent" testChildUnderFull
    , testCase "Full shows only a smaller focused commit and focus cycling hides it" testFullSelection
    , testCase "floating a tile clears its tiled edges, lets the client pick its size, and keeps it centered" testTileFloats
    ]
 where
  content width height = if width > 4 && height > 4 then Just (width - 4, height - 4) else Nothing

traceProperty :: (LayoutClass l) => TestName -> l -> (Int32 -> [Maybe (Int32, Int32)]) -> TestTree
traceProperty name layout desiredSizes =
  testProperty name $
    withMaxSuccess 30 $
      forAll (vectorOf 16 ((,) <$> elements [0, 4, 9, 50, 100, 150] <*> arbitrary)) $ \steps ->
        ioProperty (testTrace layout desiredSizes steps >> pure True)

data Model = Model
  { pendingSize :: Maybe (Int32, Int32)
  , settledSize :: Maybe (Int32, Int32)
  , shown :: Maybe Bool
  }

testTrace :: (LayoutClass l) => l -> (Int32 -> [Maybe (Int32, Int32)]) -> [(Int32, Bool)] -> Assertion
testTrace layout desiredSizes steps = withRuntimePair $ \target peer -> do
  windows <- mapM requireObjectId (take (length (desiredSizes 0)) [0xff000090, 0xff000093])
  output <- requireObjectId 0xff000091
  seat <- requireObjectId 0xff000092
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer (map (\w -> event manager 6 (putNewId w)) windows ++ [event manager 7 (putNewId output), event manager 8 (putNewId seat)])
    mapM_ (\w -> receiveNewIdRequest reader peer w (Opcode 2)) windows
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100)]
    let step models (width, acknowledge) = do
          sendFrames peer [event ls 0 (putInt 0 >> putInt 0 >> putInt width >> putInt 100), event manager 2 (pure ())]
          manage <- readUntil reader peer manager 2
          let desired = desiredSizes width
          pending <- forM (zip3 windows desired models) $ \(window, size, model) -> do
            let propose = pendingSize model == Nothing && isJust size && settledSize model /= size
            expected <- case (propose, size) of
              (True, Just (w, h)) -> (: []) <$> encodedPayload (putInt w >> putInt h)
              _ -> pure []
            assertEqual "one proposal only when no earlier proposal is pending" expected (requests window 3 manage)
            pure (if propose then size else pendingSize model)
          when (not (null (requests seat 1 manage))) $ do
            assertBool "focus requires an already shown settled selection" (shown (last models) == Just True && last pending == Nothing && isJust (last desired))
            focusPayload <- encodedPayload (putObject (last windows))
            requests seat 1 manage @?= [focusPayload]
          let settles = map (\p -> acknowledge && isJust p) pending
              settled = [if s then p else settledSize model | (s, p, model) <- zip3 settles pending models]
              pending' = [if s then Nothing else p | (s, p) <- zip settles pending]
              visible = [isJust size && isJust s | (size, s) <- zip desired settled]
          forM_ (zip3 windows settles pending) $ \(window, s, p) ->
            when s $ forM_ p $ \(w, h) ->
              sendFrames peer [event window 2 (putInt (w + 1) >> putInt h)]
          sendFrames peer [event manager 3 (pure ())]
          render <- readUntil reader peer manager 4
          forM_ (zip3 windows models visible) $ \(window, model, v) -> do
            let visibility = [rawFrameOpcode frame | frame <- render, rawFrameSender frame == window, rawFrameOpcode frame `elem` [Opcode 4, Opcode 5]]
                expectedVisibility = if shown model == Just v then [] else [Opcode (if v then 5 else 4)]
            assertEqual "visibility follows selection and settlement" expectedVisibility visibility
          requests seat 1 render @?= []
          pure (zipWith3 (\p s v -> Model p s (Just v)) pending' settled visible)
    _ <- foldM step (map (const (Model Nothing Nothing Nothing)) windows) (steps ++ replicate 3 (100, True))
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig layout [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()

testFamilies :: Assertion
testFamilies = withRuntimePair $ \target peer -> do
  w1 <- requireObjectId 0xff0000a0
  w2 <- requireObjectId 0xff0000a1
  w3 <- requireObjectId 0xff0000a2
  w4 <- requireObjectId 0xff0000a3
  output <- requireObjectId 0xff0000a4
  seat <- requireObjectId 0xff0000a5
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer (map (\w -> event manager 6 (putNewId w)) [w1, w2, w3] ++ [event manager 7 (putNewId output), event manager 8 (putNewId seat)])
    nodes <- mapM (\w -> receiveNewIdRequest reader peer w (Opcode 2)) [w1, w2, w3]
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    layerSeat <- receiveLayerShellSeatRequest reader peer layerShell seat
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
        stacked frames = [rawFrameSender f | f <- frames, rawFrameSender f `elem` nodes, rawFrameOpcode f == Opcode 2]
        assertLayerOwns frames = do
          requests seat 1 frames @?= []
          requests seat 3 frames @?= []
        windowOrder st = case coreWorkspaces st of
          AttachedState ss _ -> (index ss, peek ss)
          HeadlessState{} -> ([], Nothing)
        order = map WindowId [0xff0000a2, 0xff0000a1, 0xff0000a0]
    sendFrames
      peer
      [ event output 2 (putInt 0 >> putInt 0)
      , event output 3 (putInt 300 >> putInt 100)
      , event ls 0 (putInt 0 >> putInt 0 >> putInt 300 >> putInt 100)
      , event w2 5 (putObject w1)
      , event w3 5 (putObject w1)
      , event layerSeat 1 (pure ())
      ]
    first <- manage
    assertLayerOwns first
    forM_ [w1, w2, w3] $ \w -> do
      emptyCapabilities <- encodedPayload (putUint 0)
      requests w 14 first @?= [emptyCapabilities]
    sendFrames peer [event w3 2 (putInt 146 >> putInt 96), event w2 2 (putInt 146 >> putInt 46), event w1 2 (putInt 146 >> putInt 46)]
    initialRender <- render
    stacked initialRender @?= nodes
    unfocused <- encodedPayload (putUint 15 >> putInt 2 >> mapM_ putUint [0x40000000, 0x40000000, 0x40000000, 0xffffffff])
    forM_ [w1, w2, w3] $ \w -> requests w 8 initialRender @?= [unfocused]
    manage >>= assertLayerOwns
    _ <- render
    beforeClick <- readPolicyState policy
    windowOrder beforeClick @?= (order, Just (WindowId 0xff0000a2))
    sendFrames peer [event seat 4 (putObject w2)]
    interacted <- manage
    focusPayload <- encodedPayload (putObject w2)
    requests seat 1 interacted @?= [focusPayload]
    forM_ [w1, w2, w3] $ \w -> requests w 3 interacted @?= []
    raised <- render
    case nodes of
      [n1, n2, n3] -> stacked raised @?= [n1, n3, n2]
      _ -> fail "expected three nodes"
    afterRaise <- readPolicyState policy
    windowOrder afterRaise @?= (order, Just (WindowId 0xff0000a1))
    focusedBorder <- encodedPayload (putUint 15 >> putInt 2 >> mapM_ putUint [0, 0x80000000, 0xffffffff, 0xffffffff])
    requests w2 8 raised @?= [focusedBorder]
    sendFrames peer [event layerSeat 2 (pure ())]
    refocused <- manage
    requests seat 1 refocused @?= [focusPayload]
    _ <- render
    sendFrames peer [event layerSeat 1 (pure ()), event manager 6 (putNewId w4)]
    _ <- receiveNewIdRequest reader peer w4 (Opcode 2)
    created <- manage
    assertLayerOwns created
    proposed <- encodedPayload (putInt 146 >> putInt 29)
    requests w4 3 created @?= [proposed]
    sendFrames peer [event w4 2 (putInt 146 >> putInt 29)]
    newRender <- render
    requests w4 8 newRender @?= [unfocused]
    requests w4 5 newRender @?= [mempty]
    manage >>= assertLayerOwns
    _ <- render
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()

testChildUnderFull :: Assertion
testChildUnderFull = withRuntimePair $ \target peer -> do
  parent <- requireObjectId 0xff0000b0
  child <- requireObjectId 0xff0000b1
  output <- requireObjectId 0xff0000b2
  seat <- requireObjectId 0xff0000b3
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId parent), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    parentNode <- receiveNewIdRequest reader peer parent (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
        stacked nodes frames = [s | f <- frames, let s = rawFrameSender f, s `elem` nodes, rawFrameOpcode f == Opcode 2]
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    proposed <- encodedPayload (putInt 196 >> putInt 96)
    first <- manage
    requests parent 3 first @?= [proposed]
    sendFrames peer [event parent 2 (putInt 196 >> putInt 96)]
    _ <- render
    _ <- manage
    _ <- render
    sendFrames peer [event manager 6 (putNewId child)]
    childNode <- receiveNewIdRequest reader peer child (Opcode 2)
    sendFrames peer [event child 5 (putObject parent)]
    inserted <- manage
    clientChooses <- encodedPayload (putInt 0 >> putInt 0)
    requests child 3 inserted @?= [clientChooses]
    requests child 9 inserted @?= []
    requests parent 3 inserted @?= []
    sendFrames peer [event child 2 (putInt 80 >> putInt 40)]
    childShown <- render
    requests parent 4 childShown @?= []
    requests child 5 childShown @?= [mempty]
    centered <- encodedPayload (putInt 60 >> putInt 30)
    requests childNode 1 childShown @?= [centered]
    stacked [parentNode, childNode] childShown @?= [parentNode, childNode]
    childFocused <- manage
    childPayload <- encodedPayload (putObject child)
    requests seat 1 childFocused @?= [childPayload]
    _ <- render
    sendFrames peer [event seat 4 (putObject parent)]
    parentFocused <- manage
    parentPayload <- encodedPayload (putObject parent)
    requests seat 1 parentFocused @?= [parentPayload]
    parentFocusedRender <- render
    requests child 4 parentFocusedRender @?= []
    requests parent 4 parentFocusedRender @?= []
    stacked [parentNode, childNode] parentFocusedRender @?= []
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) (isDialog --> doFloat) (pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()

testFullSelection :: Assertion
testFullSelection = withRuntimePair $ \target peer -> do
  w1 <- requireObjectId 0xff0000c0
  w2 <- requireObjectId 0xff0000c1
  w3 <- requireObjectId 0xff0000c2
  output <- requireObjectId 0xff0000c3
  seat <- requireObjectId 0xff0000c4
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    sendFrames peer (map (\w -> event manager 6 (putNewId w)) [w1, w2, w3] ++ [event manager 7 (putNewId output), event manager 8 (putNewId seat)])
    mapM_ (\w -> receiveNewIdRequest reader peer w (Opcode 2)) [w1, w2, w3]
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    focusBinding <- receiveKeyBindingRequest reader peer xkb seat focusKey
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    proposed <- encodedPayload (putInt 196 >> putInt 96)
    first <- manage
    requests w3 3 first @?= [proposed]
    forM_ [w1, w2] $ \w -> requests w 3 first @?= []
    sendFrames peer [event w3 2 (putInt 150 >> putInt 80)]
    smaller <- render
    requests w3 5 smaller @?= [mempty]
    forM_ [w1, w2] $ \w -> do
      requests w 5 smaller @?= []
      requests w 4 smaller @?= [mempty]
    _ <- manage
    _ <- render
    sendFrames peer [event focusBinding 0 (pure ())]
    cycled <- manage
    requests w2 3 cycled @?= [proposed]
    forM_ [w1, w3] $ \w -> requests w 3 cycled @?= []
    sendFrames peer [event w2 2 (putInt 196 >> putInt 96)]
    selected <- render
    requests w3 4 selected @?= [mempty]
    requests w2 5 selected @?= [mempty]
    forM_ [w1, w3] $ \w -> requests w 5 selected @?= []
    requests w1 4 selected @?= []
    refocused <- manage
    focusPayload <- encodedPayload (putObject w2)
    requests seat 1 refocused @?= [focusPayload]
    _ <- render
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1 :: Int] [(focusKey, windows focusDown)] defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()
 where
  focusKey = RiverKeyBinding 0x62 (River.RiverSeatV1ModifiersFlags 0) Nothing

testTileFloats :: Assertion
testTileFloats = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff0000d0
  output <- requireObjectId 0xff0000d1
  seat <- requireObjectId 0xff0000d2
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    node <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    floatBinding <- receiveKeyBindingRequest reader peer xkb seat floatKey
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    first <- manage
    allEdges <- encodedPayload (putUint 15)
    requests window 9 first @?= [allEdges]
    sendFrames peer [event window 2 (putInt 196 >> putInt 96)]
    _ <- render
    _ <- manage
    _ <- render
    sendFrames peer [event floatBinding 0 (pure ())]
    floated <- manage
    noEdges <- encodedPayload (putUint 0)
    clientChooses <- encodedPayload (putInt 0 >> putInt 0)
    requests window 9 floated @?= [noEdges]
    requests window 3 floated @?= [clientChooses]
    sendFrames peer [event window 2 (putInt 120 >> putInt 50)]
    centered <- render
    position <- encodedPayload (putInt 40 >> putInt 25)
    requests node 1 centered @?= [position]
    again <- manage
    requests window 3 again @?= []
    requests window 9 again @?= []
    sendFrames peer [event window 2 (putInt 150 >> putInt 60)]
    resized <- render
    recentered <- encodedPayload (putInt 25 >> putInt 20)
    requests node 1 resized @?= [recentered]
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int] [(floatKey, withFocused float)] defaultSceneStyle (\_ -> pure ()) idHook (pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()
  final <- readPolicyState policy
  case coreWorkspaces final of
    AttachedState ss _ -> assertEqual "committed sizes never become stored geometry" [ClientSized] (Map.elems (floating ss))
    HeadlessState{} -> assertFailure "expected an attached state"
 where
  floatKey = RiverKeyBinding 0x74 (River.RiverSeatV1ModifiersFlags 0) Nothing

requests :: ObjectId -> Int -> [RawFrame] -> [ByteString]
requests sender opcode frames = [rawFramePayload f | f <- frames, rawFrameSender f == sender, rawFrameOpcode f == Opcode (fromIntegral opcode)]

event :: ObjectId -> Int -> ArgumentEncoder () () -> IO ByteString
event sender opcode = encodedFrame sender (Opcode (fromIntegral opcode))

readUntil :: FrameReader -> Transport -> ObjectId -> Int -> IO [RawFrame]
readUntil reader peer manager opcode = do
  frame <- receiveFrame reader peer
  if rawFrameSender frame == manager && rawFrameOpcode frame == Opcode (fromIntegral opcode)
    then pure []
    else (frame :) <$> readUntil reader peer manager opcode

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
