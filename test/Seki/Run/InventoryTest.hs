{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Seki.Run.InventoryTest (tests) where

import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Inventory (
  RiverDimensionsHint (..),
  RiverInventory,
  RiverInventoryError (..),
  RiverOutputState (..),
  RiverPoint (..),
  RiverRectangle (..),
  RiverSize (..),
  RiverWindowState (..),
  addRiverOutput,
  addRiverWindow,
  applyRiverOutputEvent,
  applyRiverWindowEvent,
  attachRiverWindowNode,
  emptyRiverInventory,
  lookupRiverOutput,
  lookupRiverWindow,
  riverInventoryOutputs,
  riverInventoryWindows,
  riverOutputRectangle,
 )
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Connection.ObjectTable (
  Object,
  allocateClientObject,
  defaultObjectLimit,
  newObjectTable,
  objectId,
  registerServerObject,
 )
import Seki.Wire.Types (ObjectId, mkObjectId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river inventory"
    [ testCase "accumulates persistent window state and removes closed windows" testWindowLifecycle
    , testCase "accumulates output geometry and removes outputs" testOutputLifecycle
    , testCase "rejects duplicate IDs and stale object handles" testObjectIdentity
    , testCase "rejects invalid and repeated protocol state" testInvalidState
    ]

testWindowLifecycle :: Assertion
testWindowLifecycle = do
  window <- makeServerObject River.riverWindowV1Interface 0xff000001 False
  parent <- requireObjectId 0xff000099
  inventory0 <- expectRight (addRiverWindow window emptyRiverInventory)
  inventory1 <-
    applyWindowEvents
      window
      [ River.RiverWindowV1DimensionsHint 10 20 1000 800
      , River.RiverWindowV1Dimensions 640 480
      , River.RiverWindowV1AppId (Just "terminal")
      , River.RiverWindowV1Title (Just "shell")
      , River.RiverWindowV1Parent (Just parent)
      , River.RiverWindowV1DecorationHint River.RiverWindowV1DecorationHintEnumPrefersSsd
      , River.RiverWindowV1UnreliablePid 1234
      , River.RiverWindowV1PresentationHint River.RiverOutputV1PresentationModeEnumVsync
      , River.RiverWindowV1Identifier "window-1"
      ]
      inventory0
  state <- maybe (assertFailure "window missing") pure (lookupRiverWindow (objectId window) inventory1)
  assertEqual "one window" [state] (riverInventoryWindows inventory1)
  assertEqual "dimensions hint" (Just (RiverDimensionsHint 10 20 1000 800)) (riverWindowDimensionsHint state)
  assertEqual "dimensions" (Just (RiverSize 640 480)) (riverWindowDimensions state)
  assertEqual "app ID" (Just "terminal") (riverWindowAppId state)
  assertEqual "title" (Just "shell") (riverWindowTitle state)
  assertEqual "parent" (Just parent) (riverWindowParent state)
  assertEqual "PID" (Just 1234) (riverWindowUnreliablePid state)
  assertEqual "identifier" (Just "window-1") (riverWindowIdentifier state)

  unchanged <- expectRight (applyRiverWindowEvent window River.RiverWindowV1MaximizeRequested inventory1)
  assertEqual "transient request does not alter persistent state" inventory1 unchanged
  closed <- expectRight (applyRiverWindowEvent window River.RiverWindowV1Closed inventory1)
  assertEqual "closed window removed" [] (riverInventoryWindows closed)
  expectLeft
    (RiverInventoryWindowNotTracked (objectId window))
    (applyRiverWindowEvent window (River.RiverWindowV1Title Nothing) closed)

testOutputLifecycle :: Assertion
testOutputLifecycle = do
  output <- makeServerObject River.riverOutputV1Interface 0xff000010 False
  inventory0 <- expectRight (addRiverOutput output emptyRiverInventory)
  inventory1 <-
    applyOutputEvents
      output
      [ River.RiverOutputV1WlOutput 7
      , River.RiverOutputV1Position (-1920) 30
      , River.RiverOutputV1Dimensions 1920 1080
      ]
      inventory0
  state <- maybe (assertFailure "output missing") pure (lookupRiverOutput (objectId output) inventory1)
  assertEqual "one output" [state] (riverInventoryOutputs inventory1)
  assertEqual "wl_output name" (Just 7) (riverOutputWlOutputName state)
  assertEqual "position" (Just (RiverPoint (-1920) 30)) (riverOutputPosition state)
  assertEqual "dimensions" (Just (RiverSize 1920 1080)) (riverOutputDimensions state)
  assertEqual
    "complete rectangle"
    (Just (RiverRectangle (-1920) 30 1920 1080))
    (riverOutputRectangle state)

  removed <- expectRight (applyRiverOutputEvent output River.RiverOutputV1Removed inventory1)
  assertEqual "removed output" [] (riverInventoryOutputs removed)

testObjectIdentity :: Assertion
testObjectIdentity = do
  window <- makeServerObject River.riverWindowV1Interface 0xff000020 False
  staleWindow <- makeServerObject River.riverWindowV1Interface 0xff000020 True
  outputWithSameId <- makeServerObject River.riverOutputV1Interface 0xff000020 False
  node <- makeClientObject River.riverNodeV1Interface
  inventory <- expectRight (addRiverWindow window emptyRiverInventory)
  expectLeft
    (RiverInventoryObjectAlreadyTracked (objectId window))
    (addRiverOutput outputWithSameId inventory)
  expectLeft
    (RiverInventoryWindowHandleMismatch (objectId window))
    (applyRiverWindowEvent staleWindow (River.RiverWindowV1Title (Just "stale")) inventory)
  attached <- expectRight (attachRiverWindowNode window node inventory)
  assertEqual "attached node" (Just node) (riverWindowNode =<< lookupRiverWindow (objectId window) attached)
  expectLeft
    (RiverInventoryWindowNodeAlreadyAttached (objectId window))
    (attachRiverWindowNode window node attached)

testInvalidState :: Assertion
testInvalidState = do
  window <- makeServerObject River.riverWindowV1Interface 0xff000030 False
  output <- makeServerObject River.riverOutputV1Interface 0xff000031 False
  inventory0 <- expectRight (addRiverWindow window emptyRiverInventory >>= addRiverOutput output)
  expectLeft
    (RiverInventoryInvalidWindowDimensions (objectId window) (RiverSize 0 20))
    (applyRiverWindowEvent window (River.RiverWindowV1Dimensions 0 20) inventory0)
  let invalidHint = RiverDimensionsHint 100 10 50 20
  expectLeft
    (RiverInventoryInvalidDimensionsHint (objectId window) invalidHint)
    (applyRiverWindowEvent window (River.RiverWindowV1DimensionsHint 100 10 50 20) inventory0)
  expectLeft
    (RiverInventoryInvalidOutputDimensions (objectId output) (RiverSize 800 (-1)))
    (applyRiverOutputEvent output (River.RiverOutputV1Dimensions 800 (-1)) inventory0)

  withPid <- expectRight (applyRiverWindowEvent window (River.RiverWindowV1UnreliablePid 1) inventory0)
  expectLeft
    (RiverInventoryRepeatedWindowPid (objectId window))
    (applyRiverWindowEvent window (River.RiverWindowV1UnreliablePid 2) withPid)
  withIdentifier <- expectRight (applyRiverWindowEvent window (River.RiverWindowV1Identifier "first") inventory0)
  expectLeft
    (RiverInventoryRepeatedWindowIdentifier (objectId window))
    (applyRiverWindowEvent window (River.RiverWindowV1Identifier "second") withIdentifier)
  expectLeft
    (RiverInventoryInvalidWindowIdentifier (objectId window) "")
    (applyRiverWindowEvent window (River.RiverWindowV1Identifier "") inventory0)
  expectLeft
    (RiverInventoryInvalidWindowIdentifier (objectId window) "not-ascii-日")
    (applyRiverWindowEvent window (River.RiverWindowV1Identifier "not-ascii-日") inventory0)
  withOutputName <- expectRight (applyRiverOutputEvent output (River.RiverOutputV1WlOutput 1) inventory0)
  expectLeft
    (RiverInventoryRepeatedOutputName (objectId output))
    (applyRiverOutputEvent output (River.RiverOutputV1WlOutput 2) withOutputName)

applyWindowEvents ::
  Object River.RiverWindowV1 ->
  [River.RiverWindowV1Event ()] ->
  RiverInventory ->
  IO RiverInventory
applyWindowEvents _ [] inventory = pure inventory
applyWindowEvents window (event : rest) inventory = do
  next <- expectRight (applyRiverWindowEvent window event inventory)
  applyWindowEvents window rest next

applyOutputEvents ::
  Object River.RiverOutputV1 ->
  [River.RiverOutputV1Event ()] ->
  RiverInventory ->
  IO RiverInventory
applyOutputEvents _ [] inventory = pure inventory
applyOutputEvents output (event : rest) inventory = do
  next <- expectRight (applyRiverOutputEvent output event inventory)
  applyOutputEvents output rest next

makeServerObject :: Interface interface -> Word32 -> Bool -> IO (Object interface)
makeServerObject interface rawIdentifier bumpGeneration = do
  identifier <- requireObjectId rawIdentifier
  (initialTable, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  table <-
    if bumpGeneration
      then do
        (nextTable, _) <- expectRight (allocateClientObject Wayland.wlCallbackInterface 1 initialTable)
        pure nextTable
      else pure initialTable
  (_, object) <-
    expectRight
      (registerServerObject identifier interface (interfaceMaximumVersion interface) table)
  pure object

makeClientObject :: Interface interface -> IO (Object interface)
makeClientObject interface = do
  (initialTable, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (_, object) <-
    expectRight
      (allocateClientObject interface (interfaceMaximumVersion interface) initialTable)
  pure object

requireObjectId :: Word32 -> IO ObjectId
requireObjectId rawIdentifier =
  maybe (assertFailure ("invalid object ID: " <> show rawIdentifier)) pure (mkObjectId rawIdentifier)

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

expectLeft :: (Eq errorValue, Show errorValue) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result = case result of
  Left actual -> assertEqual "error" expected actual
  Right _ -> assertFailure "expected error"
