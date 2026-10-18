{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.ReconcileTraceTest (tests) where

import Control.Exception (onException)
import Control.Monad (foldM, forM_, when)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Run.MockRiver (FrameReader, callbackDoneFrame, encodedFrame, encodedPayload, expectMockSuccess, expectRight, newFrameReader, receiveFrame, receiveLayerShellOutputRequest, receiveLayerShellSeatRequest, receiveNewIdRequest, receiveRuntimeBinds, receiveStartupRequests, registryGlobalFrame, requireObjectId, runMock, sendFrames, withRuntimePair)
import Seki.Run.Policy (PolicyConfig (..), newPolicy, policyRuntimeHandlers)
import Seki.Run.Runtime (openRiverSessionAt, runRiverSession)
import Seki.Wire.Arguments (ArgumentEncoder, putInt, putNewId, putObject, putUint)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (Transport, closeTransport)
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.QuickCheck (arbitrary, elements, forAll, ioProperty, testProperty, vectorOf, withMaxSuccess)

tests :: TestTree
tests =
  testGroup
    "reconciliation traces"
    [ testProperty "generated area and dimensions traces preserve proposal, visibility, and focus invariants" $
        withMaxSuccess 30 $
          forAll (vectorOf 16 ((,) <$> elements [0, 4, 50, 100, 150] <*> arbitrary)) $ \steps ->
            ioProperty (testTrace steps >> pure True)
    , testCase "focus raises a shown family and new windows retain nonexclusive layer focus" testFamilies
    ]

data Model = Model
  { pendingSize :: Maybe Int32
  , settledSize :: Maybe Int32
  , shown :: Maybe Bool
  }

testTrace :: [(Int32, Bool)] -> Assertion
testTrace steps = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000090
  output <- requireObjectId 0xff000091
  seat <- requireObjectId 0xff000092
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    _ <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100)]
    let step model (width, acknowledge) = do
          sendFrames peer [event ls 0 (putInt 0 >> putInt 0 >> putInt width >> putInt 100), event manager 2 (pure ())]
          manage <- readUntil reader peer manager 2
          let desired = if width > 4 then Just (width - 4) else Nothing
              propose = pendingSize model == Nothing && isJust desired && settledSize model /= desired
              pending = if propose then desired else pendingSize model
          expected <- case (propose, desired) of
            (True, Just size) -> (: []) <$> encodedPayload (putInt size >> putInt 96)
            _ -> pure []
          assertEqual "one proposal only when no earlier proposal is pending" expected (requests window 3 manage)
          when (not (null (requests seat 1 manage))) $ do
            assertBool "focus requires an already shown settled selection" (shown model == Just True && pending == Nothing && isJust desired)
            focusPayload <- encodedPayload (putObject window)
            requests seat 1 manage @?= [focusPayload]
          let settles = acknowledge && isJust pending
              settled = if settles then pending else settledSize model
              pending' = if settles then Nothing else pending
              visible = isJust desired && isJust settled
          when settles $ forM_ pending $ \size ->
            sendFrames peer [event window 2 (putInt (size + 1) >> putInt 96)]
          sendFrames peer [event manager 3 (pure ())]
          render <- readUntil reader peer manager 4
          let visibility = [rawFrameOpcode frame | frame <- render, rawFrameSender frame == window, rawFrameOpcode frame `elem` [Opcode 4, Opcode 5]]
              expectedVisibility = if shown model == Just visible then [] else [Opcode (if visible then 5 else 4)]
          assertEqual "visibility follows selection and settlement" expectedVisibility visibility
          requests seat 1 render @?= []
          pure (Model pending' settled (Just visible))
    _ <- foldM step (Model Nothing Nothing Nothing) (steps ++ replicate 3 (100, True))
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1 :: Int] [] defaultSceneStyle (\_ -> pure ()))
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
    sendFrames peer [event seat 4 (putObject w2)]
    interacted <- manage
    focusPayload <- encodedPayload (putObject w2)
    requests seat 1 interacted @?= [focusPayload]
    forM_ [w1, w2, w3] $ \w -> requests w 3 interacted @?= []
    raised <- render
    case nodes of
      [n1, n2, n3] -> stacked raised @?= [n1, n3, n2]
      _ -> fail "expected three nodes"
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
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int] [] defaultSceneStyle (\_ -> pure ()))
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right ()

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
