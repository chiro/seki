{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Wire.Connection.RequestTest (tests) where

import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Arguments (ArgumentError (..), putArray, putFd, putString, runArgumentEncoder)
import Seki.Wire.Binding (Interface (..), RequestEncoder (..))
import Seki.Wire.Codec (encodeFrame)
import Seki.Wire.Connection.ObjectTable (
  ObjectInfo (..),
  ObjectLifecycle (..),
  ObjectTableError (..),
  allocateClientObject,
  defaultObjectLimit,
  lookupObjectInfo,
  newObjectTable,
  objectCount,
  objectId,
  retiredObjectCount,
 )
import Seki.Wire.Connection.Request (
  PreparedRequest (..),
  RequestPreparationError (..),
  prepareNewObjectRequest,
  prepareRequest,
 )
import Seki.Wire.Types (FrameError (..), Opcode (..), RawFrame (..), objectIdValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

data Parent

tests :: TestTree
tests =
  testGroup
    "Wayland request preparation"
    [ testCase "allocates and encodes wl_display.sync atomically" testDisplaySync
    , testCase "rejects requests newer than the sender" testUnsupportedVersion
    , testCase "rejects invalid generated since metadata" testInvalidSince
    , testCase "applies destructor state only after encoding" testDestructor
    , testCase "preserves the table after argument failure" testArgumentFailure
    , testCase "preserves the table after oversized frame failure" testFrameFailure
    , testCase "rejects a mismatched sender interface" testInterfaceMismatch
    , testCase "preserves descriptor order" testDescriptorOrder
    , testCase "does not expose failed child allocation" testNewObjectRollback
    ]

testDisplaySync :: Assertion
testDisplaySync = do
  (table0, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (table1, callback, prepared) <-
    expectRight
      ( prepareNewObjectRequest
          Wayland.wlDisplayInterface
          display
          Wayland.wlCallbackInterface
          1
          (Wayland.encodeWlDisplaySync . objectId)
          table0
      )
  assertEqual "callback ID" 2 (objectIdValue (objectId callback))
  assertEqual "display and callback registered" 2 (objectCount table1)
  assertEqual "no descriptors" ([] :: [()]) (preparedRequestFds prepared)
  expectedPayload <- expectPayload (Wayland.encodeWlDisplaySync (objectId callback))
  expectedFrame <- expectRight (encodeFrame (RawFrame (objectId display) (Opcode 0) expectedPayload))
  assertEqual "complete frame" expectedFrame (preparedRequestBytes prepared)

testUnsupportedVersion :: Assertion
testUnsupportedVersion = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (table1, parent) <- expectRight (allocateClientObject parentInterface 2 table0)
  expectLeft
    (UnsupportedRequestVersion "parent" "late" 3 2)
    (prepareRequest parentInterface parent (emptyRequest "late" 3 False) table1)

testInvalidSince :: Assertion
testInvalidSince = do
  (table, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  expectLeft
    (InvalidRequestSince "wl_display" "invalid" 0)
    (prepareRequest Wayland.wlDisplayInterface display (emptyRequest "invalid" 0 False) table)

testDestructor :: Assertion
testDestructor = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (table1, parent) <- expectRight (allocateClientObject parentInterface 1 table0)
  (table2, _) <- expectRight (prepareRequest parentInterface parent (emptyRequest "destroy" 1 True) table1)
  assertEqual "sender retired" 1 (retiredObjectCount table2)
  assertEqual "retired lifecycle" (Just ObjectAwaitingDeleteId) (objectInfoLifecycle <$> lookupObjectInfo (objectId parent) table2)

testArgumentFailure :: Assertion
testArgumentFailure = do
  (table, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  let invalidRequest = RequestEncoder "invalid_string" (Opcode 2) 1 True (putString "bad\0string")
  expectLeft
    (RequestArgumentError "wl_display" "invalid_string" StringContainsNul)
    (prepareRequest Wayland.wlDisplayInterface display invalidRequest table)
  assertEqual "display remains active" (Just ObjectActive) (objectInfoLifecycle <$> lookupObjectInfo (objectId display) table)

testFrameFailure :: Assertion
testFrameFailure = do
  (table, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  let oversizedRequest =
        RequestEncoder "oversized" (Opcode 2) 1 True $ do
          putArray (ByteString.replicate 32760 0)
          putArray (ByteString.replicate 32760 0)
  expectLeft
    (RequestFrameError "wl_display" "oversized" (FrameSizeTooLarge 65536))
    (prepareRequest Wayland.wlDisplayInterface display oversizedRequest table)
  assertEqual "display remains active" (Just ObjectActive) (objectInfoLifecycle <$> lookupObjectInfo (objectId display) table)

testInterfaceMismatch :: Assertion
testInterfaceMismatch = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (table1, parent) <- expectRight (allocateClientObject parentInterface 1 table0)
  expectLeft
    (RequestObjectError (ObjectInterfaceMismatch (objectId parent) "other_parent" "parent"))
    (prepareRequest otherParentInterface parent (emptyRequest "request" 1 False) table1)

testDescriptorOrder :: Assertion
testDescriptorOrder = do
  (table, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  let descriptorRequest = RequestEncoder "descriptors" (Opcode 2) 1 False (putFd (10 :: Int) >> putFd 20)
  (_, prepared) <- expectRight (prepareRequest Wayland.wlDisplayInterface display descriptorRequest table)
  assertEqual "descriptor FIFO" [10, 20] (preparedRequestFds prepared)
  assertEqual "descriptor-free payload frame length" 8 (ByteString.length (preparedRequestBytes prepared))

testNewObjectRollback :: Assertion
testNewObjectRollback = do
  (table0, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  expectLeft
    (UnsupportedRequestVersion "wl_display" "future_create" 2 1)
    ( prepareNewObjectRequest
        Wayland.wlDisplayInterface
        display
        Wayland.wlCallbackInterface
        1
        (\_ -> emptyRequest "future_create" 2 False)
        table0
    )
  (_, callback, _) <-
    expectRight
      ( prepareNewObjectRequest
          Wayland.wlDisplayInterface
          display
          Wayland.wlCallbackInterface
          1
          (Wayland.encodeWlDisplaySync . objectId)
          table0
      )
  assertEqual "failed transaction did not consume ID" 2 (objectIdValue (objectId callback))

emptyRequest :: Text -> Word32 -> Bool -> RequestEncoder fd
emptyRequest name since destructor = RequestEncoder name (Opcode 0) since destructor (pure ())

expectPayload :: RequestEncoder () -> IO ByteString.ByteString
expectPayload request = case runArgumentEncoder (requestArguments request) of
  Left errorValue -> assertFailure ("unexpected argument error: " <> show errorValue)
  Right ((), payload, []) -> pure payload
  Right (_, _, descriptors) -> assertFailure ("unexpected descriptors: " <> show (length descriptors))

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

expectLeft :: (Eq errorValue, Show errorValue) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result = case result of
  Left actual -> assertEqual "error" expected actual
  Right _ -> assertFailure "expected error"

parentInterface :: Interface Parent
parentInterface = Interface "parent" 3

otherParentInterface :: Interface Parent
otherParentInterface = Interface "other_parent" 3
