module Seki.Run.FullTest (tests) where

import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Full (selectFullOutputRectangle)
import Seki.Run.Inventory (
  RiverInventory,
  RiverRectangle (..),
  addRiverOutput,
  applyRiverOutputEvent,
  emptyRiverInventory,
 )
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Connection.ObjectTable (
  Object,
  defaultObjectLimit,
  newObjectTable,
  registerServerObject,
 )
import Seki.Wire.Types (mkObjectId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Full placement"
    [ testCase "selects the lowest-ID complete output" testOutputSelection
    , testCase "has no target before an output is complete" testIncompleteOutput
    ]

testOutputSelection :: Assertion
testOutputSelection = do
  incomplete <- makeOutput 0xff000001
  selected <- makeOutput 0xff000002
  later <- makeOutput 0xff000003
  inventory0 <-
    expectRight
      ( addRiverOutput incomplete emptyRiverInventory
          >>= addRiverOutput selected
          >>= addRiverOutput later
      )
  inventory1 <- applyOutput selected (River.RiverOutputV1Position (-1920) 20) inventory0
  inventory2 <- applyOutput selected (River.RiverOutputV1Dimensions 1280 720) inventory1
  inventory3 <- applyOutput later (River.RiverOutputV1Position 0 0) inventory2
  inventory4 <- applyOutput later (River.RiverOutputV1Dimensions 2560 1440) inventory3
  assertEqual
    "selected rectangle"
    (Just (RiverRectangle (-1920) 20 1280 720))
    (selectFullOutputRectangle inventory4)

testIncompleteOutput :: Assertion
testIncompleteOutput = do
  output <- makeOutput 0xff000010
  inventory0 <- expectRight (addRiverOutput output emptyRiverInventory)
  inventory1 <- applyOutput output (River.RiverOutputV1Position 10 30) inventory0
  assertEqual "incomplete output" Nothing (selectFullOutputRectangle inventory1)

applyOutput ::
  Object River.RiverOutputV1 ->
  River.RiverOutputV1Event () ->
  RiverInventory ->
  IO RiverInventory
applyOutput output event inventory =
  expectRight (applyRiverOutputEvent output event inventory)

makeOutput :: Word32 -> IO (Object River.RiverOutputV1)
makeOutput rawIdentifier = do
  identifier <- maybe (assertFailure "invalid test object ID") pure (mkObjectId rawIdentifier)
  (initialTable, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (_, output) <-
    expectRight
      ( registerServerObject
          identifier
          River.riverOutputV1Interface
          (interfaceMaximumVersion River.riverOutputV1Interface)
          initialTable
      )
  pure output

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure
