{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.ReconcileTraceTest (tests) where

import Control.Applicative ((<|>))
import Control.Exception (onException)
import Control.Monad (foldM_, forM, forM_, replicateM, unless, when)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Word (Word32)
import Seki.Internal.Core.Action (float, sink, windows, withFocused)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize, rectX, rectY)
import Seki.Internal.Core.Layout (LayoutClass (..), Mirror (..), SizePolicy (..), tolerant)
import Seki.Internal.Core.ManageHook (ManageHook, appId, doFloat, doShift, idHook, isDialog, (-->), (=?))
import Seki.Internal.Core.PointerOp (OpKind (..))
import Seki.Internal.Core.Scene (defaultSceneStyle)
import Seki.Internal.Core.StackSet (FloatGeom (..), StackSet (..), WindowId (..), focusDown, index, integrate, peek, view)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..))
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.MockRiver (FrameReader, callbackDoneFrame, encodedFrame, encodedPayload, expectMockSuccess, expectRight, newFrameReader, receiveFrame, receiveKeyBindingRequest, receiveLayerShellOutputRequest, receiveLayerShellSeatRequest, receiveNewIdRequest, receivePointerBindingRequest, receiveRuntimeBinds, receiveStartupRequests, registryGlobalFrame, requireObjectId, runMock, sendFrames, withRuntimePair)
import Seki.Run.Pointer (RiverPointerBinding (..))
import Seki.Run.Policy (PolicyConfig (..), newPolicy, policyRuntimeHandlers, readPolicyState)
import Seki.Run.Runtime (RiverExit (..), openRiverSessionAt, runRiverSession)
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Arguments (ArgumentEncoder, DecodeResult (..), getInt, putInt, putNewId, putNullableObject, putNullableString, putObject, putUint, runCompleteArgumentDecoder)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (Transport, closeTransport)
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (arbitrary, elements, forAll, frequency, ioProperty, testProperty, vectorOf, withMaxSuccess)

tests :: TestTree
tests =
  testGroup
    "reconciliation traces"
    [ traceProperty "generated area and dimensions traces preserve proposal, visibility, and focus invariants" Full $ \width ->
        [content 0 0 width 100]
    , traceProperty "generated Tall traces never propose or show a degenerate tile" tall $ \width ->
        [content (width `div` 2) 0 (width - width `div` 2) 100, content 0 0 (width `div` 2) 100]
    , traceProperty "generated Mirror Tall traces never propose or show a degenerate tile" (Mirror tall) $ \width ->
        [content 0 50 width 50, content 0 0 width 50]
    , traceProperty "M2-D04: generated Tolerate traces never clip, keep the committed size, and propose within the hints" (tolerant tall) $ \width ->
        [content (width `div` 2) 0 (width - width `div` 2) 100, content 0 0 (width `div` 2) 100]
    , traceProperty "M2-D04: generated Tolerate Full traces never clip, keep the committed size, and propose within the hints" (tolerant Full) $ \width ->
        [content 0 0 width 100]
    , testCase "focus raises a shown family and new windows retain nonexclusive layer focus" testFamilies
    , testCase "M1-D06 superseded: Full keeps a floating dialog centered above its refocused parent" testChildUnderFull
    , testCase "Full shows only a smaller focused commit and focus cycling hides it" testFullSelection
    , testCase "floating a tile clears its tiled edges, lets the client pick its size, and keeps it centered" testTileFloats
    , testCase "M2-D04: a Crop tile's clip box is untouched while fullscreen and fits its new tile after exit" testCropAcrossFullscreen
    , testCase "M2-D03: exiting fullscreen with a proposal pending proposes the tile again and settles to it" testFullscreenExit
    , testCase "removing a fullscreen window's output makes it fullscreen where its workspace went" testFullscreenOutputMoved
    , testCase "removing a fullscreen window's output hides it with its workspace and sends no fullscreen request" testFullscreenOutputHidden
    , testCase "switching away from a fullscreen window and back hides and shows it with no fullscreen request" testFullscreenHidden
    , testCase "a fullscreen window's workspace shown on another output sends fullscreen for that output alone" testFullscreenShownElsewhere
    , testCase "a hidden window that leaves the fullscreen set exits fullscreen at once" testFullscreenLostWhileHidden
    , testCase "a binding drag floats a tile at its shown box, moves it with set_position alone, and ends with op_end" testPointerMove
    , testCase "a left-edge resize informs the client before proposing and keeps the right edge under a smaller commit" testPointerResize
    , testCase "an op that replaces another starts in the sequence after the one that ends it" testPointerSwap
    , pointerProperty
    ]
 where
  tall = Tall 1 (1 / 10) (1 / 2)
  content x y width height = if width > 4 && height > 4 then Just (Tile (x + 2, y + 2) (width - 4, height - 4)) else Nothing

-- | A tile's content box: its top-left and its size.
data Tile = Tile (Int32, Int32) (Int32, Int32)

-- | Minimum width and height, then maximum width and height, zero meaning no preference.
type Hint = (Int32, Int32, Int32, Int32)

data Step = Step
  { stepWidth :: Int32
  , stepAcknowledge :: Bool
  , stepCommitDelta :: (Int32, Int32)
  , stepHints :: [Maybe Hint]
  }
  deriving (Show)

traceProperty :: (LayoutClass l) => TestName -> l -> (Int32 -> [Maybe Tile]) -> TestTree
traceProperty name layout tiles =
  testProperty name $
    withMaxSuccess 30 $
      forAll (vectorOf 16 step) $ \steps ->
        ioProperty (testTrace layout tiles steps >> pure True)
 where
  step =
    Step
      <$> elements [0, 4, 9, 50, 100, 150]
      <*> arbitrary
      <*> elements [(1, 0), (0, 0), (-3, 0), (40, 30), (0, -5)]
      <*> vectorOf 2 (elements [Nothing, Just (0, 0, 0, 0), Just (120, 0, 0, 0), Just (0, 0, 60, 40), Just (30, 60, 80, 70)])

data Model = Model
  { pendingSize :: Maybe (Int32, Int32)
  , settledSize :: Maybe (Int32, Int32)
  , shown :: Maybe Bool
  , hint :: Maybe Hint
  , committed :: Maybe (Int32, Int32)
  , position :: Maybe (Int32, Int32)
  , clip :: Maybe (Int32, Int32)
  -- ^ The content clip box in effect, 'Nothing' while disabled as river starts.
  }

testTrace :: (LayoutClass l) => l -> (Int32 -> [Maybe Tile]) -> [Step] -> Assertion
testTrace layout tiles steps = withRuntimePair $ \target peer -> do
  traced <- mapM requireObjectId (take (length (tiles 0)) [0xff000090, 0xff000093])
  output <- requireObjectId 0xff000091
  seat <- requireObjectId 0xff000092
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer (map (\w -> event manager 6 (putNewId w)) traced ++ [event manager 7 (putNewId output), event manager 8 (putNewId seat)])
    nodes <- mapM (\w -> receiveNewIdRequest reader peer w (Opcode 2)) traced
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100)]
    let crop = sizePolicy layout == Crop
        run models (Step width acknowledge (dw, dh) hints) = do
          let hinted = [h <|> hint model | (model, h) <- zip models (hints ++ repeat Nothing)]
          sendFrames peer ([event window 1 (mapM_ putInt [a, b, c, d]) | (window, Just (a, b, c, d)) <- zip traced (hints ++ repeat Nothing)] ++ [event ls 0 (putInt 0 >> putInt 0 >> putInt width >> putInt 100), event manager 2 (pure ())])
          manage <- readUntil reader peer manager 2
          let desired = tiles width
              wanted = [(\(Tile _ size) -> if crop then size else clamp h size) <$> tile | (tile, h) <- zip desired hinted]
          pending <- forM (zip3 traced wanted models) $ \(window, size, model) -> do
            let propose = isNothing (pendingSize model) && isJust size && settledSize model /= size
            expected <- case (propose, size) of
              (True, Just (w, h)) -> (: []) <$> encodedPayload (putInt w >> putInt h)
              _ -> pure []
            assertEqual "one proposal only when no earlier proposal is pending" expected (requests window 3 manage)
            pure (if propose then size else pendingSize model)
          unless (null (requests seat 1 manage)) $ do
            assertBool "focus requires an already shown settled selection" (shown (last models) == Just True && isNothing (last pending) && isJust (last desired))
            focusPayload <- encodedPayload (putObject (last traced))
            requests seat 1 manage @?= [focusPayload]
          let settles = map (\p -> acknowledge && isJust p) pending
              commits = [if s then (\(w, h) -> (max 1 (w + dw), max 1 (h + dh))) <$> p else Nothing | (s, p) <- zip settles pending]
              settled = [if s then p else settledSize model | (s, p, model) <- zip3 settles pending models]
              pending' = [if s then Nothing else p | (s, p) <- zip settles pending]
              visible = [isJust tile && isJust s | (tile, s) <- zip desired settled]
          forM_ (zip traced commits) $ \(window, c) ->
            forM_ c $ \(w, h) -> sendFrames peer [event window 2 (putInt w >> putInt h)]
          sendFrames peer [event manager 3 (pure ())]
          render <- readUntil reader peer manager 4
          forM (zip3 (zip traced nodes) (zip3 models visible desired) (zip3 pending' settled (zip hinted commits))) $ \((window, node), (model, v, tile), (p, s, (h, c))) -> do
            let visibility = [rawFrameOpcode frame | frame <- render, rawFrameSender frame == window, rawFrameOpcode frame `elem` [Opcode 4, Opcode 5]]
                expectedVisibility = if shown model == Just v then [] else [Opcode (if v then 5 else 4)]
            assertEqual "visibility follows selection and settlement" expectedVisibility visibility
            moves <- mapM (decodeInts 2) (requests node 1 render)
            clips <- mapM (decodeInts 4) (requests window 22 render)
            assertBool "a content clip box starts at the window's top-left" (all (\b -> take 2 b == [0, 0]) clips)
            unless crop $ assertEqual "M2-D04: Tolerate sends no content clip box" [] clips
            let model' =
                  model
                    { pendingSize = p
                    , settledSize = s
                    , shown = Just v
                    , hint = h
                    , committed = c <|> committed model
                    , position = if null moves then position model else movedTo (last moves)
                    , clip = if null clips then clip model else clipped (last clips)
                    }
            forM_ tile $ \(Tile origin (tw, th)) -> when v $ do
              assertEqual "a shown tile sits at its tile's top-left, even while a proposal is pending" (Just origin) (position model')
              if crop
                then do
                  assertEqual "M2-D04: under Crop the content clip box equals the tile's content box" (Just (tw, th)) (clip model')
                  forM_ (committed model') $ \(cw, ch) -> do
                    let (bw, bh) = fromMaybe (cw, ch) (clip model')
                    assertBool "M2-D04: under Crop no committed content pixel leaves its tile" (min cw bw <= tw && min ch bh <= th)
                else assertEqual "M2-D04: under Tolerate no content clip box is in effect" Nothing (clip model')
            pure model'
    foldM_ run (map (const (Model Nothing Nothing Nothing Nothing Nothing Nothing Nothing)) traced) (steps ++ replicate 3 (Step 100 True (0, 0) []))
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig layout [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded
 where
  clamp current (w, h) = case current of
    Nothing -> (w, h)
    Just (minW, minH, maxW, maxH) -> (bound minW maxW w, bound minH maxH h)
  bound lo hi v = let raised = max lo v in if hi > 0 then min hi raised else raised
  movedTo = \case
    [x, y] -> Just (x, y)
    _ -> Nothing
  clipped = \case
    [_, _, w, h] | w > 0 && h > 0 -> Just (w, h)
    _ -> Nothing

decodeInts :: Int -> ByteString -> IO [Int32]
decodeInts n payload = case runCompleteArgumentDecoder (replicateM n getInt) payload ([] :: [()]) of
  Done values _ _ -> pure values
  other -> assertFailure ("undecodable request payload: " <> show other)

testFamilies :: Assertion
testFamilies = withRuntimePair $ \target peer -> do
  w1 <- requireObjectId 0xff0000a0
  w2 <- requireObjectId 0xff0000a1
  w3 <- requireObjectId 0xff0000a2
  w4 <- requireObjectId 0xff0000a3
  output <- requireObjectId 0xff0000a4
  seat <- requireObjectId 0xff0000a5
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
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
      fullscreenOnly <- encodedPayload (putUint 4)
      requests w 14 first @?= [fullscreenOnly]
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
  result @?= Right RiverEnded

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
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) (isDialog --> doFloat) (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded

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
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1 :: Int] [(focusKey, windows focusDown)] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded
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
    tiledRender <- render
    cropped <- encodedPayload (mapM_ putInt [0, 0, 196, 96])
    requests window 22 tiledRender @?= [cropped]
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
    unclipped <- encodedPayload (mapM_ putInt [0, 0, 0, 0])
    assertEqual "floating a tile disables its content clip box" [unclipped] (requests window 22 centered)
    again <- manage
    requests window 3 again @?= []
    requests window 9 again @?= []
    sendFrames peer [event window 2 (putInt 150 >> putInt 60)]
    resized <- render
    recentered <- encodedPayload (putInt 25 >> putInt 20)
    requests node 1 resized @?= [recentered]
    assertEqual "a float stays unclipped" [] (requests window 22 resized)
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int] [(floatKey, withFocused float)] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded
  final <- readPolicyState policy
  case coreWorkspaces final of
    AttachedState ss _ -> assertEqual "committed sizes never become stored geometry" [ClientSized] (Map.elems (floating ss))
    HeadlessState{} -> assertFailure "expected an attached state"
 where
  floatKey = RiverKeyBinding 0x74 (River.RiverSeatV1ModifiersFlags 0) Nothing

testFullscreenExit :: Assertion
testFullscreenExit = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff0000e0
  output <- requireObjectId 0xff0000e1
  seat <- requireObjectId 0xff0000e2
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    node <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    first <- manage
    tile <- encodedPayload (putInt 196 >> putInt 96)
    requests window 3 first @?= [tile]
    _ <- render
    sendFrames peer [event window 12 (putNullableObject Nothing)]
    entered <- manage
    onOutput <- encodedPayload (putObject output)
    requests window 19 entered @?= [onOutput]
    requests window 17 entered @?= [mempty]
    requests window 3 entered @?= []
    sendFrames peer [event window 2 (putInt 200 >> putInt 100)]
    covering <- render
    requests window 5 covering @?= [mempty]
    requests node 1 covering @?= []
    stillCovering <- manage
    requests window 3 stillCovering @?= []
    _ <- render
    sendFrames peer [event window 13 (pure ())]
    exited <- manage
    [rawFrameOpcode f | f <- exited, rawFrameSender f == window] @?= map Opcode [20, 18, 3]
    requests window 3 exited @?= [tile]
    sendFrames peer [event window 2 (putInt 196 >> putInt 96)]
    settled <- render
    position <- encodedPayload (putInt 2 >> putInt 2)
    requests node 1 settled @?= [position]
    requests window 4 settled @?= []
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded

testCropAcrossFullscreen :: Assertion
testCropAcrossFullscreen = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000130
  output <- requireObjectId 0xff000131
  seat <- requireObjectId 0xff000132
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    _ <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
        clipBox w h = encodedPayload (mapM_ putInt [0, 0, w, h])
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    _ <- manage
    sendFrames peer [event window 2 (putInt 196 >> putInt 96)]
    tiled <- render
    wholeTile <- clipBox 196 96
    assertEqual "the tile is clipped to its content box" [wholeTile] (requests window 22 tiled)
    sendFrames peer [event window 12 (putNullableObject Nothing)]
    _ <- manage
    sendFrames peer [event window 2 (putInt 200 >> putInt 100)]
    covering <- render
    assertEqual "a covering window's clip box is left alone" [] (requests window 22 covering)
    sendFrames peer [event ls 0 (putInt 0 >> putInt 0 >> putInt 150 >> putInt 100)]
    _ <- manage
    narrowed <- render
    assertEqual "a narrower tile while fullscreen sends no clip box" [] (requests window 22 narrowed)
    sendFrames peer [event window 13 (pure ())]
    exited <- manage
    narrowTile <- encodedPayload (putInt 146 >> putInt 96)
    requests window 3 exited @?= [narrowTile]
    sendFrames peer [event window 2 (putInt 200 >> putInt 100)]
    settled <- render
    narrowClip <- clipBox 146 96
    assertEqual "after exit an oversized commit is clipped to the narrower tile" [narrowClip] (requests window 22 settled)
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded

-- | One window fullscreen on the first of two outputs, then that output removed.
fullscreenOnRemovedOutput :: [Int] -> ([RawFrame] -> ObjectId -> ObjectId -> IO ()) -> Assertion
fullscreenOnRemovedOutput tags afterRemoval = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff0000f0
  first <- requireObjectId 0xff0000f1
  second <- requireObjectId 0xff0000f2
  seat <- requireObjectId 0xff0000f3
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, _, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId first), event manager 7 (putNewId second), event manager 8 (putNewId seat)]
    _ <- receiveNewIdRequest reader peer window (Opcode 2)
    ls1 <- receiveLayerShellOutputRequest reader peer layerShell first
    ls2 <- receiveLayerShellOutputRequest reader peer layerShell second
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
    sendFrames
      peer
      [ event first 2 (putInt 0 >> putInt 0)
      , event first 3 (putInt 200 >> putInt 100)
      , event ls1 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)
      , event second 2 (putInt 200 >> putInt 0)
      , event second 3 (putInt 200 >> putInt 100)
      , event ls2 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)
      ]
    _ <- manage
    sendFrames peer [event window 2 (putInt 196 >> putInt 96)]
    _ <- render
    sendFrames peer [event window 12 (putNullableObject Nothing)]
    entered <- manage
    onFirst <- encodedPayload (putObject first)
    requests window 19 entered @?= [onFirst]
    sendFrames peer [event window 2 (putInt 200 >> putInt 100)]
    _ <- render
    sendFrames peer [event first 0 (pure ())]
    removed <- manage
    forM_ [20, 3] $ \opcode -> requests window opcode removed @?= []
    afterRemoval removed window second
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) tags [] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded

testFullscreenOutputMoved :: Assertion
testFullscreenOutputMoved = fullscreenOnRemovedOutput [1] $ \removed window second -> do
  onSecond <- encodedPayload (putObject second)
  requests window 19 removed @?= [onSecond]
  forM_ [17, 18] $ \opcode -> requests window opcode removed @?= []

testFullscreenOutputHidden :: Assertion
testFullscreenOutputHidden = fullscreenOnRemovedOutput [1, 2] $ \removed window _ ->
  fullscreenRequests window removed @?= []

testFullscreenHidden :: Assertion
testFullscreenHidden = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000100
  output <- requireObjectId 0xff000101
  seat <- requireObjectId 0xff000102
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int, 2] [(viewKey 1, windows (view 1)), (viewKey 2, windows (view 2))] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    _ <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    one <- receiveKeyBindingRequest reader peer xkb seat (viewKey 1)
    two <- receiveKeyBindingRequest reader peer xkb seat (viewKey 2)
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    _ <- manage
    sendFrames peer [event window 2 (putInt 196 >> putInt 96)]
    _ <- render
    sendFrames peer [event window 12 (putNullableObject Nothing)]
    _ <- manage
    sendFrames peer [event window 2 (putInt 200 >> putInt 100)]
    _ <- render
    sendFrames peer [event two 0 (pure ())]
    away <- manage
    fullscreenRequests window away @?= []
    hiddenRender <- render
    requests window 4 hiddenRender @?= [mempty]
    stillFullscreen <- coreFullscreen <$> readPolicyState policy
    stillFullscreen @?= Set.singleton (WindowId 0xff000100)
    sendFrames peer [event one 0 (pure ())]
    back <- manage
    fullscreenRequests window back @?= []
    requests window 3 back @?= []
    shownRender <- render
    requests window 5 shownRender @?= [mempty]
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded

testFullscreenShownElsewhere :: Assertion
testFullscreenShownElsewhere = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000110
  first <- requireObjectId 0xff000111
  second <- requireObjectId 0xff000112
  seat <- requireObjectId 0xff000113
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId first), event manager 7 (putNewId second), event manager 8 (putNewId seat)]
    _ <- receiveNewIdRequest reader peer window (Opcode 2)
    ls1 <- receiveLayerShellOutputRequest reader peer layerShell first
    ls2 <- receiveLayerShellOutputRequest reader peer layerShell second
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    keys <- mapM (receiveKeyBindingRequest reader peer xkb seat . viewKey) [1, 2, 3]
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
        press tag = sendFrames peer [event (keys !! (tag - 1)) 0 (pure ())]
    sendFrames
      peer
      [ event first 2 (putInt 0 >> putInt 0)
      , event first 3 (putInt 200 >> putInt 100)
      , event ls1 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)
      , event second 2 (putInt 200 >> putInt 0)
      , event second 3 (putInt 200 >> putInt 100)
      , event ls2 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)
      ]
    _ <- manage
    sendFrames peer [event window 2 (putInt 196 >> putInt 96)]
    _ <- render
    sendFrames peer [event window 12 (putNullableObject Nothing)]
    entered <- manage
    onFirst <- encodedPayload (putObject first)
    requests window 19 entered @?= [onFirst]
    sendFrames peer [event window 2 (putInt 200 >> putInt 100)]
    _ <- render
    press 3
    hidden <- manage
    fullscreenRequests window hidden @?= []
    hiddenRender <- render
    requests window 4 hiddenRender @?= [mempty]
    press 2
    onOther <- manage
    fullscreenRequests window onOther @?= []
    _ <- render
    press 1
    shownElsewhere <- manage
    onSecond <- encodedPayload (putObject second)
    fullscreenRequests window shownElsewhere @?= [Opcode 19]
    requests window 19 shownElsewhere @?= [onSecond]
    requests window 3 shownElsewhere @?= []
    shownRender <- render
    requests window 5 shownRender @?= [mempty]
    closeTransport peer
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int, 2, 3] [(viewKey t, windows (view t)) | t <- [1, 2, 3]] defaultSceneStyle (\_ -> pure ()) idHook (pure ()) [])
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded

testFullscreenLostWhileHidden :: Assertion
testFullscreenLostWhileHidden = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000120
  output <- requireObjectId 0xff000121
  seat <- requireObjectId 0xff000122
  shifted <- requireObjectId 0xff000123
  policy <- expectRight =<< newPolicy (PolicyConfig (Tall 1 (1 / 10) (1 / 2)) [1 :: Int, 2] [(viewKey 2, windows (view 2))] defaultSceneStyle (\_ -> pure ()) (appId =? "mpv" --> doShift 1) (pure ()) [])
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    _ <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    two <- receiveKeyBindingRequest reader peer xkb seat (viewKey 2)
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    _ <- manage
    sendFrames peer [event window 2 (putInt 196 >> putInt 96)]
    _ <- render
    sendFrames peer [event window 12 (putNullableObject Nothing)]
    _ <- manage
    sendFrames peer [event window 2 (putInt 200 >> putInt 100)]
    _ <- render
    sendFrames peer [event two 0 (pure ())]
    away <- manage
    fullscreenRequests window away @?= []
    _ <- render
    sendFrames peer [event manager 6 (putNewId shifted), event shifted 3 (putNullableString (Just "mpv"))]
    _ <- receiveNewIdRequest reader peer shifted (Opcode 2)
    lost <- manage
    fullscreenRequests window lost @?= map Opcode [20, 18]
    requests window 3 lost @?= []
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded
  remaining <- coreFullscreen <$> readPolicyState policy
  remaining @?= Set.empty

moveBinding :: RiverPointerBinding
moveBinding = RiverPointerBinding 0x110 (River.RiverSeatV1ModifiersFlags 64)

pointerConfig :: l -> ManageHook Int l -> PolicyConfig Int l
pointerConfig layout hook = PolicyConfig layout [1 :: Int] [] defaultSceneStyle (\_ -> pure ()) hook (pure ()) [(moveBinding, Move)]

-- | One window on a 200 by 100 output and a seat with the move binding, after the window is shown and focused.
data PointerTrace = PointerTrace
  { ptWindow :: ObjectId
  , ptNode :: ObjectId
  , ptSeat :: ObjectId
  , ptBinding :: ObjectId
  , ptManager :: ObjectId
  , ptKeys :: [ObjectId]
  , ptManage :: IO [RawFrame]
  , ptRender :: IO [RawFrame]
  , ptSend :: [IO ByteString] -> IO ()
  }

pointerTrace :: (LayoutClass l) => PolicyConfig Int l -> (Int32, Int32) -> (PointerTrace -> IO ()) -> (CoreState Int l -> Assertion) -> Assertion
pointerTrace config (cw, ch) script final = withRuntimePair $ \target peer -> do
  window <- requireObjectId 0xff000150
  output <- requireObjectId 0xff000151
  seat <- requireObjectId 0xff000152
  policy <- expectRight =<< newPolicy config
  done <- runMock $ flip onException (closeTransport peer) $ do
    reader <- newFrameReader
    (manager, xkb, layerShell) <- handshake reader peer
    sendFrames peer [event manager 6 (putNewId window), event manager 7 (putNewId output), event manager 8 (putNewId seat)]
    node <- receiveNewIdRequest reader peer window (Opcode 2)
    ls <- receiveLayerShellOutputRequest reader peer layerShell output
    _ <- receiveLayerShellSeatRequest reader peer layerShell seat
    keys <- mapM (receiveKeyBindingRequest reader peer xkb seat . fst) (policyBindings config)
    binding <- receivePointerBindingRequest reader peer seat moveBinding
    let manage = sendFrames peer [event manager 2 (pure ())] >> readUntil reader peer manager 2
        render = sendFrames peer [event manager 3 (pure ())] >> readUntil reader peer manager 4
    sendFrames peer [event output 2 (putInt 0 >> putInt 0), event output 3 (putInt 200 >> putInt 100), event ls 0 (putInt 0 >> putInt 0 >> putInt 200 >> putInt 100)]
    first <- manage
    requests binding 1 first @?= [mempty]
    sendFrames peer [event window 2 (putInt cw >> putInt ch)]
    _ <- render
    _ <- manage
    _ <- render
    script (PointerTrace window node seat binding manager keys manage render (sendFrames peer))
    closeTransport peer
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit (policyRuntimeHandlers policy)
  result <- runRiverSession session
  expectMockSuccess done
  result @?= Right RiverEnded
  final =<< readPolicyState policy

floatsIn :: CoreState i l -> [FloatGeom]
floatsIn st = case coreWorkspaces st of
  AttachedState ss _ -> Map.elems (floating ss)
  HeadlessState hw -> Map.elems (headlessFloating hw)

outputBox :: Int32 -> Int32 -> Int32 -> Int32 -> FloatGeom
outputBox x y w h = FloatAt (OutputRect (either (error . show) id (mkSize w h >>= mkRect (Point x y))))

testPointerMove :: Assertion
testPointerMove = pointerTrace (pointerConfig SmallTile idHook) (96, 46) script $ \st ->
  floatsIn st @?= [outputBox 32 12 96 46]
 where
  script t = do
    let seat = ptSeat t
        window = ptWindow t
    ptSend t [event seat 2 (putObject window), event (ptBinding t) 0 (pure ())]
    clicked <- ptManage t
    requests seat 4 clicked @?= [mempty]
    _ <- ptRender t
    ptSend t [event seat 6 (putInt 0 >> putInt 0), event seat 7 (pure ()), event (ptBinding t) 1 (pure ())]
    clickReleased <- ptManage t
    requests seat 5 clickReleased @?= [mempty]
    assertEqual "a mod+click with no motion leaves the tile tiled" [] (requests window 9 (clicked ++ clickReleased))
    _ <- ptRender t
    ptSend t [event (ptBinding t) 0 (pure ())]
    pressed <- ptManage t
    requests seat 4 pressed @?= [mempty]
    _ <- ptRender t
    ptSend t [event seat 6 (putInt 30 >> putInt 10)]
    moved <- ptManage t
    untiled <- encodedPayload (putUint 0)
    requests window 9 moved @?= [untiled]
    assertEqual "a tile floated at its shown box needs no proposal" [] (requests window 3 moved)
    assertEqual "a running op is not started again" [] (requests seat 4 moved)
    movedRender <- ptRender t
    shifted <- encodedPayload (putInt 32 >> putInt 12)
    requests (ptNode t) 1 movedRender @?= [shifted]
    ptSend t [event seat 7 (pure ()), event (ptBinding t) 1 (pure ())]
    released <- ptManage t
    requests seat 5 released @?= [mempty]
    _ <- ptRender t
    ptSend t [event seat 6 (putInt 90 >> putInt 90)]
    late <- ptManage t
    assertEqual "a delta after the op ended changes nothing" [] (requests window 3 late ++ requests seat 4 late ++ requests seat 5 late)
    lateRender <- ptRender t
    requests (ptNode t) 1 lateRender @?= []

-- | Tiles its first window in a 100 by 50 box at the area's top-left, leaving room to drag it.
data SmallTile = SmallTile deriving (Show, Read)

instance LayoutClass SmallTile where
  pureLayout _ r s = [(w, tile) | Right tile <- [mkSize 100 50 >>= mkRect (Point (rectX r) (rectY r))], w <- take 1 (integrate s)]

testPointerResize :: Assertion
testPointerResize = pointerTrace (pointerConfig (Tall 1 (1 / 10) (1 / 2)) doFloat) (80, 40) script $ \st ->
  floatsIn st @?= [outputBox 40 30 100 40]
 where
  script t = do
    let seat = ptSeat t
        window = ptWindow t
    ptSend t [event window 8 (putObject seat >> putUint 4)]
    started <- ptManage t
    requests seat 4 started @?= [mempty]
    assertEqual "nothing resizes before the first delta" [] (requests window 12 started)
    _ <- ptRender t
    ptSend t [event seat 6 (putInt (-20) >> putInt 0)]
    dragged <- ptManage t
    assertEqual "inform_resize_start precedes the proposal" (map Opcode [12, 3]) [rawFrameOpcode f | f <- dragged, rawFrameSender f == window, rawFrameOpcode f `elem` map Opcode [12, 3, 13]]
    proposed <- encodedPayload (putInt 100 >> putInt 40)
    requests window 3 dragged @?= [proposed]
    pendingRender <- ptRender t
    assertEqual "while the proposal is pending the right edge stays" [] (requests (ptNode t) 1 pendingRender)
    ptSend t [event window 2 (putInt 90 >> putInt 40)]
    committed <- ptRender t
    anchored <- encodedPayload (putInt 50 >> putInt 30)
    assertEqual "a smaller commit keeps the right edge at 140" [anchored] (requests (ptNode t) 1 committed)
    ptSend t [event seat 7 (pure ())]
    released <- ptManage t
    assertEqual "the release informs the end and ends the op" [Opcode 13] [rawFrameOpcode f | f <- released, rawFrameSender f == window, rawFrameOpcode f `elem` map Opcode [12, 13]]
    requests seat 5 released @?= [mempty]
    _ <- ptRender t
    pure ()

testPointerSwap :: Assertion
testPointerSwap = pointerTrace (pointerConfig (Tall 1 (1 / 10) (1 / 2)) doFloat) (80, 40) script (const (pure ()))
 where
  script t = do
    let seat = ptSeat t
        window = ptWindow t
    ptSend t [event seat 2 (putObject window), event (ptBinding t) 0 (pure ())]
    first <- ptManage t
    requests seat 4 first @?= [mempty]
    _ <- ptRender t
    ptSend t [event seat 7 (pure ()), event (ptBinding t) 1 (pure ()), event window 7 (putObject seat)]
    swapped <- ptManage t
    requests seat 5 swapped @?= [mempty]
    assertEqual "the new op does not start in the sequence that ends the old one" [] (requests seat 4 swapped)
    _ <- ptRender t
    next <- ptManage t
    assertEqual "the render asked for another manage sequence" [mempty] (requests (ptManager t) 3 next)
    requests seat 4 next @?= [mempty]
    requests seat 5 next @?= []

data PointerStep = Press | Delta Int32 Int32 | OpRelease | BindingRelease | ClientMove | ClientResize Word32 | Commit Int32 Int32 | Sink
  deriving (Show)

-- | Never two starts without an end between, for ops per seat and for resizes per window.
alternates :: [Bool] -> Bool
alternates = go False
 where
  go _ [] = True
  go running (start : rest) = start /= running && go start rest

pointerProperty :: TestTree
pointerProperty =
  testProperty "generated pointer traces never repeat an op start or a resize start without the end between" $
    withMaxSuccess 30 $
      forAll (vectorOf 24 step) $ \steps ->
        ioProperty (pointerTrace (sinkConfig (pointerConfig (Tall 1 (1 / 10) (1 / 2)) idHook)) (196, 96) (run steps) (const (pure ())) >> pure True)
 where
  step =
    frequency
      [ (3, pure Press)
      , (4, Delta <$> elements [-150, -30, 0, 25, 120] <*> elements [-80, -10, 0, 15, 60])
      , (2, pure OpRelease)
      , (2, pure BindingRelease)
      , (1, pure ClientMove)
      , (2, ClientResize <$> elements [1, 2, 4, 8, 5, 10])
      , (3, Commit <$> elements [1, 50, 120, 196] <*> elements [1, 40, 96])
      , (1, pure Sink)
      ]
  sinkConfig config = config{policyBindings = [(RiverKeyBinding 0x74 (River.RiverSeatV1ModifiersFlags 0) Nothing, withFocused sink)]}
  run steps t = do
    frames <- concat <$> mapM (\s -> send t s >> (++) <$> ptManage t <*> ptRender t) steps
    let ops = [rawFrameOpcode f == Opcode 4 | f <- frames, rawFrameSender f == ptSeat t, rawFrameOpcode f `elem` map Opcode [4, 5]]
        resizes = [rawFrameOpcode f == Opcode 12 | f <- frames, rawFrameSender f == ptWindow t, rawFrameOpcode f `elem` map Opcode [12, 13]]
    assertBool ("op_start_pointer and op_end alternate: " <> show ops) (alternates ops)
    assertBool ("inform_resize_start and inform_resize_end alternate: " <> show resizes) (alternates resizes)
  send t = \case
    Press -> ptSend t [event (ptSeat t) 2 (putObject (ptWindow t)), event (ptBinding t) 0 (pure ())]
    Delta dx dy -> ptSend t [event (ptSeat t) 6 (putInt dx >> putInt dy)]
    OpRelease -> ptSend t [event (ptSeat t) 7 (pure ())]
    BindingRelease -> ptSend t [event (ptBinding t) 1 (pure ())]
    ClientMove -> ptSend t [event (ptWindow t) 7 (putObject (ptSeat t))]
    ClientResize edges -> ptSend t [event (ptWindow t) 8 (putObject (ptSeat t) >> putUint edges)]
    Commit w h -> ptSend t [event (ptWindow t) 2 (putInt w >> putInt h)]
    Sink -> ptSend t [event k 0 (pure ()) | k <- take 1 (ptKeys t)]

viewKey :: Int -> RiverKeyBinding
viewKey tag = RiverKeyBinding (0x30 + fromIntegral tag) (River.RiverSeatV1ModifiersFlags 0) Nothing

fullscreenRequests :: ObjectId -> [RawFrame] -> [Opcode]
fullscreenRequests window frames = [rawFrameOpcode f | f <- frames, rawFrameSender f == window, rawFrameOpcode f `elem` map Opcode [17 .. 20]]

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
