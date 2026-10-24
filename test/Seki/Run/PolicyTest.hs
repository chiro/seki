{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.PolicyTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException)
import Data.ByteString.Char8 qualified as Char8
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Word (Word32)
import Seki.Internal.Core.Action (Action, kill, spawn, windows)
import Seki.Internal.Core.ManageHook (ManageHook, appId, doShift, idHook, (-->), (=?))
import Seki.Internal.Core.Scene (BorderColor (..), SceneStyle (..), defaultSceneStyle)
import Seki.Internal.Core.StackSet (
  OutputId (..),
  Screen (..),
  Stack (..),
  StackSet (..),
  WindowId (..),
  Workspace (..),
  currentTag,
  greedyView,
  integrate',
  peek,
  view,
 )
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), WorkspaceTagError (..), Workspaces (..), fromWorkspaces, newCoreState)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Inventory (RiverWindowState (..), riverInventoryWindows)
import Seki.Run.MockRiver (
  FrameReader,
  callbackDoneFrame,
  encodedFrame,
  expectMockSuccess,
  expectRight,
  newFrameReader,
  receiveExpectedRequest,
  receiveFrame,
  receiveKeyBindingRequest,
  receiveLayerShellOutputRequest,
  receiveLayerShellSeatRequest,
  receiveNewIdRequest,
  receiveRuntimeBinds,
  receiveStartupRequests,
  registryGlobalFrame,
  requireObjectId,
  runMock,
  sendFrames,
  withRuntimePair,
 )
import Seki.Run.Policy (
  Policy,
  PolicyConfig (..),
  PolicyError (..),
  launchDetached,
  newPolicy,
  policyRuntimeHandlers,
  readPolicyState,
 )
import Seki.Run.Runtime (
  RiverRuntimeHandlers (..),
  RiverRuntimeSnapshot (..),
  openRiverSessionAt,
  runRiverSession,
 )
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Arguments (putNewId, putNullableString, putObject)
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit, objectId)
import Seki.Wire.Transport (ConnectionTarget, Transport, closeTransport)
import Seki.Wire.Types (ObjectId, Opcode (..), RawFrame (..), objectIdValue)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, fileExist)
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), closeFd, defaultFileFlags, fdWrite, openFd)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "runtime policy"
    [ testCase "rejects configuration errors before connecting" testConfigErrors
    , testCase "applies river events at the next manage_start" testEventsAtManageStart
    , testCase "runs the manage hook on the properties river sent before manage_start" testManageHookProps
    , testCase "focuses on interaction and ignores pointer enter and leave" testSeatEvents
    , testCase "applies bindings pressed before one manage_start in press order" testBindingOrder
    , testCase "queues window events and bindings in one arrival order" testOneQueue
    , testCase "sends close requests only for windows still in inventory" testCloseRequests
    , testCase "sets the current output as the layer-shell default" testSetDefaultFollowsCurrentOutput
    , testCase "sets no layer-shell default while headless" testSetDefaultHeadless
    , testCase "runs the startup hook once, after the first output attaches" testStartupHookOnce
    , testCase "launches a command without waiting for it" testLaunchDetached
    ]

type TestPolicy = Policy Int Full

-- | What one manage plan preparation saw and did.
data Drain = Drain
  { drainBefore :: CoreState Int Full
  , drainAfter :: CoreState Int Full
  , drainLaunchesBefore :: [String]
  , drainNodeWindows :: [WindowId]
  }

testConfigErrors :: Assertion
testConfigErrors = do
  let binding = keyBinding 0x61
      config tags bindings = PolicyConfig Full tags bindings defaultSceneStyle (\_ -> pure ()) idHook (pure ()) []
  let invalidWidth = defaultSceneStyle{sceneBorderWidth = -1}
      invalidColor = defaultSceneStyle{sceneFocusedColor = BorderColor 2 0 0 1}
  assertError (PolicyInvalidSceneStyle invalidWidth) =<< newPolicy (config [1] []){policySceneStyle = invalidWidth}
  assertError (PolicyInvalidSceneStyle invalidColor) =<< newPolicy (config [1] []){policySceneStyle = invalidColor}
  assertError (PolicyTagError NoWorkspaceTags) =<< newPolicy (config [] [])
  assertError (PolicyTagError (DuplicateWorkspaceTag 2)) =<< newPolicy (config [1, 2, 2] [])
  assertError (PolicyBindingConflict binding)
    =<< newPolicy (config [1] [(binding, spawn "a"), (keyBinding 0x62, kill), (binding, kill)])
 where
  assertError :: PolicyError Int -> Either (PolicyError Int) TestPolicy -> Assertion
  assertError expected = either (@?= expected) (const (assertFailure "expected a configuration error"))

testEventsAtManageStart :: Assertion
testEventsAtManageStart =
  withRuntimePair $ \target peer -> do
    w1 <- requireObjectId 0xff000030
    o1 <- requireObjectId 0xff000031
    w2 <- requireObjectId 0xff000032
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, _, layerShellId) <- handshake reader peer
      sendFrames peer [newWindow managerId w1, newOutput managerId o1, manageStart managerId]
      _ <- receiveNewIdRequest reader peer w1 (Opcode 2)
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      finishManageDefault reader peer managerId ls1
      sendFrames peer [newWindow managerId w2, manageStart managerId]
      _ <- receiveNewIdRequest reader peer w2 (Opcode 2)
      finishManageDefault reader peer managerId ls1
      closeTransport peer
    (_, drains) <- runPolicy target mockDone []
    case drains of
      [d0, d1] -> do
        assertEqual "nothing applied before the first manage_start" initialState (drainBefore d0)
        assertEqual
          "the first manage_start applies the window and output"
          (fromWorkspaces (AttachedState (StackSet (Screen (Workspace 1 Full (Just (Stack (wid w1) [] []))) (oid o1)) [] [empty 2, empty 3] Map.empty) []))
          (drainAfter d0)
        assertEqual "the second window waits for the next manage_start" (drainAfter d0) (drainBefore d1)
        assertEqual "the second manage_start manages it" (sort [wid w1, wid w2]) (managed (drainAfter d1))
      _ -> assertFailure ("expected two drains, got " <> show (length drains))
    mapM_ (\d -> assertEqual "every inventory window with a node is managed" (sort (drainNodeWindows d)) (managed (drainAfter d))) drains

testManageHookProps :: Assertion
testManageHookProps =
  withRuntimePair $ \target peer -> do
    w1 <- requireObjectId 0xff000038
    w2 <- requireObjectId 0xff000039
    o1 <- requireObjectId 0xff00003a
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, _, layerShellId) <- handshake reader peer
      sendFrames
        peer
        [ newWindow managerId w1
        , newWindow managerId w2
        , encodedFrame w1 (Opcode 3) (putNullableString (Just (Text.pack "mpv")))
        , newOutput managerId o1
        , manageStart managerId
        ]
      _ <- receiveNewIdRequest reader peer w1 (Opcode 2)
      _ <- receiveNewIdRequest reader peer w2 (Opcode 2)
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      finishManageDefault reader peer managerId ls1
      closeTransport peer
    (_, drains) <- runPolicyWith (appId =? "mpv" --> doShift 2) (pure ()) target mockDone []
    case drains of
      [d0] ->
        assertEqual
          "the app_id sent after the window event moves only that window, and w2 keeps focus"
          (fromWorkspaces (AttachedState (StackSet (Screen (Workspace 1 Full (Just (Stack (wid w2) [] []))) (oid o1)) [] [Workspace 2 Full (Just (Stack (wid w1) [] [])), empty 3] Map.empty) []))
          (drainAfter d0)
      _ -> assertFailure ("expected one drain, got " <> show (length drains))

testSeatEvents :: Assertion
testSeatEvents =
  withRuntimePair $ \target peer -> do
    w1 <- requireObjectId 0xff000040
    w2 <- requireObjectId 0xff000041
    o1 <- requireObjectId 0xff000042
    s1 <- requireObjectId 0xff000043
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, _, layerShellId) <- handshake reader peer
      sendFrames peer [newWindow managerId w1, newWindow managerId w2, newOutput managerId o1, newSeat managerId s1, manageStart managerId]
      _ <- receiveNewIdRequest reader peer w1 (Opcode 2)
      _ <- receiveNewIdRequest reader peer w2 (Opcode 2)
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      _ <- receiveLayerShellSeatRequest reader peer layerShellId s1
      finishManageDefault reader peer managerId ls1
      sendFrames
        peer
        [ encodedFrame s1 (Opcode 2) (putObject w1)
        , encodedFrame s1 (Opcode 3) (pure ())
        , manageStart managerId
        ]
      finishManageDefault reader peer managerId ls1
      sendFrames peer [encodedFrame s1 (Opcode 4) (putObject w1), manageStart managerId]
      finishManageDefault reader peer managerId ls1
      closeTransport peer
    (_, drains) <- runPolicy target mockDone []
    case map drainAfter drains of
      [afterCreate, afterPointer, afterInteraction] -> do
        assertEqual "the newest window starts focused" (Just (wid w2)) (focused afterCreate)
        assertEqual "pointer enter and leave change nothing" afterCreate afterPointer
        assertEqual "interaction focuses the window" (Just (wid w1)) (focused afterInteraction)
      _ -> assertFailure ("expected three drains, got " <> show (length drains))

testBindingOrder :: Assertion
testBindingOrder =
  withRuntimePair $ \target peer -> do
    o1 <- requireObjectId 0xff000050
    s1 <- requireObjectId 0xff000051
    let bindings =
          [ (keyBinding 0x32, windows (greedyView 2))
          , (keyBinding 0x33, windows (greedyView 3))
          , (keyBinding 0x61, spawn "one")
          , (keyBinding 0x62, spawn "two")
          ]
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, xkbBindingsId, layerShellId) <- handshake reader peer
      sendFrames peer [newOutput managerId o1, newSeat managerId s1]
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      _ <- receiveLayerShellSeatRequest reader peer layerShellId s1
      bindingIds <- mapM (receiveKeyBindingRequest reader peer xkbBindingsId s1 . fst) bindings
      sendFrames peer [manageStart managerId]
      mapM_ (\b -> receiveExpectedRequest reader peer b (Opcode 2) (pure ())) bindingIds
      finishManageDefault reader peer managerId ls1
      sendFrames peer (map pressed bindingIds ++ [manageStart managerId])
      finishManageDefault reader peer managerId ls1
      closeTransport peer
    (launches, drains) <- runPolicy target mockDone bindings
    case drains of
      [_, d1] -> do
        assertEqual "nothing launched before manage_start" [] (drainLaunchesBefore d1)
        assertEqual "the later view wins" (Just 3) (currentTagOf (drainAfter d1))
      _ -> assertFailure ("expected two drains, got " <> show (length drains))
    assertEqual "launches in press order" ["one", "two"] launches

testOneQueue :: Assertion
testOneQueue =
  withRuntimePair $ \target peer -> do
    w1 <- requireObjectId 0xff000060
    w2 <- requireObjectId 0xff000061
    o1 <- requireObjectId 0xff000062
    s1 <- requireObjectId 0xff000063
    let bindings = [(keyBinding 0x63, kill)]
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, xkbBindingsId, layerShellId) <- handshake reader peer
      sendFrames peer [newWindow managerId w1, newOutput managerId o1, newSeat managerId s1]
      _ <- receiveNewIdRequest reader peer w1 (Opcode 2)
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      _ <- receiveLayerShellSeatRequest reader peer layerShellId s1
      killId <- receiveKeyBindingRequest reader peer xkbBindingsId s1 (keyBinding 0x63)
      sendFrames peer [manageStart managerId]
      receiveExpectedRequest reader peer killId (Opcode 2) (pure ())
      finishManageDefault reader peer managerId ls1
      sendFrames peer [newWindow managerId w2, pressed killId, manageStart managerId]
      _ <- receiveNewIdRequest reader peer w2 (Opcode 2)
      receiveExpectedRequest reader peer w2 (Opcode 1) (pure ())
      finishManageDefault reader peer managerId ls1
      closeTransport peer
    _ <- runPolicy target mockDone bindings
    pure ()

testCloseRequests :: Assertion
testCloseRequests =
  withRuntimePair $ \target peer -> do
    w1 <- requireObjectId 0xff000070
    o1 <- requireObjectId 0xff000071
    s1 <- requireObjectId 0xff000072
    let bindings = [(keyBinding 0x63, kill)]
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, xkbBindingsId, layerShellId) <- handshake reader peer
      sendFrames peer [newWindow managerId w1, newOutput managerId o1, newSeat managerId s1]
      nodeId <- receiveNewIdRequest reader peer w1 (Opcode 2)
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      _ <- receiveLayerShellSeatRequest reader peer layerShellId s1
      killId <- receiveKeyBindingRequest reader peer xkbBindingsId s1 (keyBinding 0x63)
      sendFrames peer [manageStart managerId]
      receiveExpectedRequest reader peer killId (Opcode 2) (pure ())
      finishManageDefault reader peer managerId ls1
      sendFrames peer [pressed killId, manageStart managerId]
      receiveExpectedRequest reader peer w1 (Opcode 1) (pure ())
      finishManageDefault reader peer managerId ls1
      sendFrames peer [pressed killId, encodedFrame w1 (Opcode 0) (pure ()), manageStart managerId]
      receiveExpectedRequest reader peer nodeId (Opcode 0) (pure ())
      receiveExpectedRequest reader peer w1 (Opcode 0) (pure ())
      finishManageDefault reader peer managerId ls1
      closeTransport peer
    (_, drains) <- runPolicy target mockDone bindings
    assertEqual "the closed window is unmanaged" [] (maybe [] (managed . drainAfter) (lastMaybe drains))

testSetDefaultFollowsCurrentOutput :: Assertion
testSetDefaultFollowsCurrentOutput =
  withRuntimePair $ \target peer -> do
    o1 <- requireObjectId 0xff000080
    o2 <- requireObjectId 0xff000081
    s1 <- requireObjectId 0xff000082
    let bindings =
          [ (keyBinding 0x32, windows (view 2))
          , (keyBinding 0x31, windows (greedyView 1))
          ]
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, xkbBindingsId, layerShellId) <- handshake reader peer
      sendFrames peer [newOutput managerId o1, newOutput managerId o2, newSeat managerId s1]
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      ls2 <- receiveLayerShellOutputRequest reader peer layerShellId o2
      _ <- receiveLayerShellSeatRequest reader peer layerShellId s1
      [viewId, greedyViewId] <- mapM (receiveKeyBindingRequest reader peer xkbBindingsId s1 . fst) bindings
      sendFrames peer [manageStart managerId]
      mapM_ (\b -> receiveExpectedRequest reader peer b (Opcode 2) (pure ())) [viewId, greedyViewId]
      finishManageDefault reader peer managerId ls1
      sendFrames peer [pressed viewId, manageStart managerId]
      finishManageDefault reader peer managerId ls2
      sendFrames peer [pressed greedyViewId, manageStart managerId]
      finishManageDefault reader peer managerId ls2
      closeTransport peer
    (_, drains) <- runPolicy target mockDone bindings
    assertEqual
      "view moves to the second output; greedyView then pulls tag 1 onto it"
      [Just (1, oid o1), Just (2, oid o2), Just (1, oid o2)]
      (map (currentScreenOf . drainAfter) drains)

testSetDefaultHeadless :: Assertion
testSetDefaultHeadless =
  withRuntimePair $ \target peer -> do
    w1 <- requireObjectId 0xff000090
    o1 <- requireObjectId 0xff000091
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, _, layerShellId) <- handshake reader peer
      sendFrames peer [newWindow managerId w1, manageStart managerId]
      _ <- receiveNewIdRequest reader peer w1 (Opcode 2)
      finishManage reader peer managerId
      sendFrames peer [newOutput managerId o1, manageStart managerId]
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      finishManageDefault reader peer managerId ls1
      sendFrames peer [encodedFrame o1 (Opcode 0) (pure ()), manageStart managerId]
      receiveExpectedRequest reader peer ls1 (Opcode 0) (pure ())
      receiveExpectedRequest reader peer o1 (Opcode 0) (pure ())
      finishManage reader peer managerId
      closeTransport peer
    (_, drains) <- runPolicy target mockDone []
    assertEqual
      "headless, attached, then headless again"
      [Nothing, Just (1, oid o1), Nothing]
      (map (currentScreenOf . drainAfter) drains)

testStartupHookOnce :: Assertion
testStartupHookOnce =
  withRuntimePair $ \target peer -> do
    o1 <- requireObjectId 0xff0000a0
    o2 <- requireObjectId 0xff0000a1
    mockDone <- runMock $ do
      reader <- newFrameReader
      (managerId, _, layerShellId) <- handshake reader peer
      sendFrames peer [manageStart managerId]
      finishManage reader peer managerId
      sendFrames peer [newOutput managerId o1, manageStart managerId]
      ls1 <- receiveLayerShellOutputRequest reader peer layerShellId o1
      finishManageDefault reader peer managerId ls1
      sendFrames peer [encodedFrame o1 (Opcode 0) (pure ()), manageStart managerId]
      receiveExpectedRequest reader peer ls1 (Opcode 0) (pure ())
      receiveExpectedRequest reader peer o1 (Opcode 0) (pure ())
      finishManage reader peer managerId
      sendFrames peer [newOutput managerId o2, manageStart managerId]
      ls2 <- receiveLayerShellOutputRequest reader peer layerShellId o2
      finishManageDefault reader peer managerId ls2
      closeTransport peer
    (launches, drains) <- runPolicyWith idHook (windows (greedyView 2) >> spawn "startup") target mockDone []
    assertEqual
      "headless start, the hook views tag 2 once attached, headless again, then tag 2 returns"
      [Nothing, Just (2, oid o1), Nothing, Just (2, oid o2)]
      (map (currentScreenOf . drainAfter) drains)
    assertEqual "launches seen before each drain" [[], [], ["startup"], ["startup"]] (map drainLaunchesBefore drains)
    assertEqual "the hook launched once across detach and reattach" ["startup"] launches

testLaunchDetached :: Assertion
testLaunchDetached =
  withSystemTempDirectory "seki-launch" $ \dir -> do
    let fifo = dir <> "/go"
        started = dir <> "/started"
        done = dir <> "/done"
    createNamedPipe fifo 0o600
    launchDetached
      ( "exec 3<> " <> fifo <> "; touch " <> started <> "; timeout 10 head -n 1 <&3 > /dev/null && touch " <> done
      )
    assertBool "the command has not finished when launchDetached returns" . not =<< fileExist done
    waitFor "the command to start" started
    fd <- openFd fifo WriteOnly defaultFileFlags{nonBlock = True}
    _ <- fdWrite fd "go\n"
    closeFd fd
    waitFor "the command to finish" done

waitFor :: String -> FilePath -> Assertion
waitFor label path = go (500 :: Int)
 where
  go 0 = assertFailure ("timed out waiting for " <> label)
  go n = fileExist path >>= \exists -> if exists then pure () else threadDelay 10000 >> go (n - 1)

{- | Run a policy over tags 1 to 3 against the mock, recording each manage
plan preparation and every launched command.
-}
runPolicy ::
  ConnectionTarget ->
  MVar (Either SomeException ()) ->
  [(RiverKeyBinding, Action Int Full ())] ->
  IO ([String], [Drain])
runPolicy = runPolicyWith idHook (pure ())

-- | 'runPolicy' with a manage hook and a startup hook.
runPolicyWith ::
  ManageHook Int Full ->
  Action Int Full () ->
  ConnectionTarget ->
  MVar (Either SomeException ()) ->
  [(RiverKeyBinding, Action Int Full ())] ->
  IO ([String], [Drain])
runPolicyWith hook startup target mockDone bindings = do
  launches <- newIORef []
  drains <- newIORef []
  policy <- expectRight =<< newPolicy (PolicyConfig Full [1, 2, 3] bindings defaultSceneStyle (record launches) hook startup [])
  let handlers = policyRuntimeHandlers policy
      observed =
        handlers
          { prepareRiverManagePlan = \snapshot -> do
              before <- readPolicyState policy
              launchedBefore <- readIORef launches
              plan <- prepareRiverManagePlan handlers snapshot
              after <- readPolicyState policy
              record drains (Drain before after launchedBefore (nodeWindows snapshot))
              pure plan
          }
  session <- expectRight =<< openRiverSessionAt target defaultObjectLimit observed
  assertEqual "policy session lifecycle" (Right ()) =<< runRiverSession session
  expectMockSuccess mockDone
  (,) <$> readIORef launches <*> readIORef drains
 where
  record ref x = modifyIORef' ref (<> [x])

nodeWindows :: RiverRuntimeSnapshot -> [WindowId]
nodeWindows snapshot =
  [ wid (objectId (riverWindowObject w))
  | w <- riverInventoryWindows (riverRuntimeInventory snapshot)
  , isJust (riverWindowNode w)
  ]

handshake :: FrameReader -> Transport -> IO (ObjectId, ObjectId, ObjectId)
handshake reader peer = do
  (registryId, callbackId) <- receiveStartupRequests reader peer
  sendFrames
    peer
    [ registryGlobalFrame registryId 7 "river_window_manager_v1" 4
    , registryGlobalFrame registryId 8 "river_xkb_bindings_v1" 3
    , registryGlobalFrame registryId 9 "river_layer_shell_v1" 1
    , callbackDoneFrame callbackId 1
    ]
  receiveRuntimeBinds reader peer

-- | Complete a manage sequence and the render sequence river requires after it.
finishManage :: FrameReader -> Transport -> ObjectId -> IO ()
finishManage reader peer managerId = do
  consumeUntil reader peer managerId (Opcode 2)
  sendFrames peer [encodedFrame managerId (Opcode 3) (pure ())]
  consumeUntil reader peer managerId (Opcode 4)

finishManageDefault :: FrameReader -> Transport -> ObjectId -> ObjectId -> IO ()
finishManageDefault reader peer managerId layerShellOutput = do
  consumeUntil reader peer layerShellOutput (Opcode 1)
  finishManage reader peer managerId

consumeUntil :: FrameReader -> Transport -> ObjectId -> Opcode -> IO ()
consumeUntil reader peer sender opcode = do
  frame <- receiveFrame reader peer
  if rawFrameSender frame == sender && rawFrameOpcode frame == opcode
    then pure ()
    else do
      assertBool "scene requests belong to windows, nodes, or seats" (rawFrameSender frame /= sender)
      consumeUntil reader peer sender opcode

newWindow, newOutput, newSeat :: ObjectId -> ObjectId -> IO Char8.ByteString
newWindow managerId w = encodedFrame managerId (Opcode 6) (putNewId w)
newOutput managerId o = encodedFrame managerId (Opcode 7) (putNewId o)
newSeat managerId s = encodedFrame managerId (Opcode 8) (putNewId s)

manageStart :: ObjectId -> IO Char8.ByteString
manageStart managerId = encodedFrame managerId (Opcode 2) (pure ())

pressed :: ObjectId -> IO Char8.ByteString
pressed binding = encodedFrame binding (Opcode 0) (pure ())

keyBinding :: Word32 -> RiverKeyBinding
keyBinding keysym =
  RiverKeyBinding
    { riverKeyBindingKeysym = keysym
    , riverKeyBindingModifiers = River.RiverSeatV1ModifiersFlags 64
    , riverKeyBindingLayoutOverride = Nothing
    }

initialState :: CoreState Int Full
initialState = either (error . show) id (newCoreState Full [1, 2, 3])

empty :: Int -> Workspace Int Full
empty t = Workspace t Full Nothing

wid :: ObjectId -> WindowId
wid = WindowId . objectIdValue

oid :: ObjectId -> OutputId
oid = OutputId . objectIdValue

managed :: CoreState i l -> [WindowId]
managed st = sort (concatMap (integrate' . stack) allWs)
 where
  allWs = case coreWorkspaces st of
    HeadlessState hw -> headlessCurrent hw : headlessHidden hw
    AttachedState ss _ -> workspace (current ss) : map workspace (visible ss) ++ hidden ss

focused :: CoreState i l -> Maybe WindowId
focused st = case coreWorkspaces st of
  AttachedState ss _ -> peek ss
  HeadlessState{} -> Nothing

currentScreenOf :: CoreState i l -> Maybe (i, OutputId)
currentScreenOf st = case coreWorkspaces st of
  AttachedState ss _ -> Just (currentTag ss, output (current ss))
  HeadlessState{} -> Nothing

currentTagOf :: CoreState i l -> Maybe i
currentTagOf st = case coreWorkspaces st of
  AttachedState ss _ -> Just (currentTag ss)
  HeadlessState{} -> Nothing

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)
