{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Wire.Connection.EventTest (tests) where

import Data.ByteString qualified as ByteString
import Data.Word (Word32)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Arguments (ArgumentEncoder, DecodeResult (..), putUint, runArgumentEncoder, runCompleteArgumentDecoder)
import Seki.Wire.Binding (EventDecoder (..), Interface (..))
import Seki.Wire.Connection.Event (
  EventPreparationError (..),
  finalizeDecodedEvent,
  finalizeDeleteIdEvent,
  prepareEvent,
  preparedEventArguments,
  preparedEventDestructor,
  preparedEventInterface,
  preparedEventName,
  preparedEventSince,
 )
import Seki.Wire.Connection.ObjectTable (
  Object,
  ObjectLifecycle (..),
  ObjectTable,
  allocateClientObject,
  defaultObjectLimit,
  destroyObjectByRequest,
  lookupObjectInfo,
  newObjectTable,
  objectId,
  objectInfoLifecycle,
 )
import Seki.Wire.Types (Opcode (..), RawFrame (..), objectIdValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

data Versioned

tests :: TestTree
tests =
  testGroup
    "Wayland event preparation"
    [ testCase "selects and decodes a generated callback event" testCallbackDone
    , testCase "applies destructor state only after decoding" testDestructorFinalization
    , testCase "rejects a frame from a different sender" testSenderMismatch
    , testCase "rejects an unknown event opcode" testUnknownOpcode
    , testCase "rejects events newer than the object" testUnsupportedVersion
    , testCase "rejects invalid generated since metadata" testInvalidSince
    , testCase "releases a tombstoned ID after delete_id" testDeleteId
    , testCase "guards the specialized delete_id transition" testWrongDeleteIdEvent
    ]

testCallbackDone :: Assertion
testCallbackDone = do
  (table, callback) <- callbackTable
  payload <- encodedUint 37
  let frame = RawFrame (objectId callback) (Opcode 0) payload
  prepared <- expectRight (prepareEvent Wayland.wlCallbackInterface callback decodeCallbackEvent frame table)
  assertEqual "interface" "wl_callback" (preparedEventInterface prepared)
  assertEqual "event" "done" (preparedEventName prepared)
  assertEqual "since" 1 (preparedEventSince prepared)
  assertEqual "destructor" True (preparedEventDestructor prepared)
  case runCompleteArgumentDecoder (preparedEventArguments prepared) (rawFramePayload frame) [] of
    Done (Wayland.WlCallbackDone callbackData) remaining [] -> do
      assertEqual "callback data" 37 callbackData
      assertEqual "complete payload" ByteString.empty remaining
    result -> assertFailure ("unexpected decode result: " <> show result)

testDestructorFinalization :: Assertion
testDestructorFinalization = do
  (table, callback) <- callbackTable
  payload <- encodedUint 1
  let frame = RawFrame (objectId callback) (Opcode 0) payload
  prepared <- expectRight (prepareEvent Wayland.wlCallbackInterface callback decodeCallbackEvent frame table)
  assertEqual
    "decoder needs bytes"
    (NeedBytes 4)
    (runCompleteArgumentDecoder (preparedEventArguments prepared) ByteString.empty ([] :: [()]))
  assertEqual "still active" (Just ObjectActive) (objectInfoLifecycle <$> lookupObjectInfo (objectId callback) table)
  case runCompleteArgumentDecoder (preparedEventArguments prepared) (rawFramePayload frame) [] of
    Done (Wayland.WlCallbackDone 1) remaining [] -> assertEqual "complete payload" ByteString.empty remaining
    result -> assertFailure ("unexpected decode result: " <> show result)
  finalized <- expectRight (finalizeDecodedEvent Wayland.wlCallbackInterface prepared table)
  assertEqual "retired after decode" (Just ObjectAwaitingDeleteId) (objectInfoLifecycle <$> lookupObjectInfo (objectId callback) finalized)

testSenderMismatch :: Assertion
testSenderMismatch = do
  (table0, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (table1, callback) <- expectRight (allocateClientObject Wayland.wlCallbackInterface 1 table0)
  payload <- encodedUint 1
  let frame = RawFrame (objectId display) (Opcode 0) payload
  expectLeft
    (EventSenderMismatch (objectId callback) (objectId display))
    (prepareEvent Wayland.wlCallbackInterface callback decodeCallbackEvent frame table1)

testUnknownOpcode :: Assertion
testUnknownOpcode = do
  (table, callback) <- callbackTable
  let frame = RawFrame (objectId callback) (Opcode 1) ByteString.empty
  expectLeft
    (UnexpectedEventOpcode "wl_callback" (Opcode 1))
    (prepareEvent Wayland.wlCallbackInterface callback decodeCallbackEvent frame table)

testUnsupportedVersion :: Assertion
testUnsupportedVersion = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (table1, object) <- expectRight (allocateClientObject versionedInterface 1 table0)
  let frame = RawFrame (objectId object) (Opcode 0) ByteString.empty
      decoder _ = Just (EventDecoder "newer" 2 False (pure ()))
  expectLeft
    (UnsupportedEventVersion "versioned" "newer" 2 1)
    (prepareEvent versionedInterface object decoder frame table1)

testInvalidSince :: Assertion
testInvalidSince = do
  (table, callback) <- callbackTable
  let frame = RawFrame (objectId callback) (Opcode 0) ByteString.empty
      decoder _ = Just (EventDecoder "invalid" 0 False (pure ()))
  expectLeft
    (InvalidEventSince "wl_callback" "invalid" 0)
    (prepareEvent Wayland.wlCallbackInterface callback decoder frame table)

testDeleteId :: Assertion
testDeleteId = do
  (table0, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (table1, callback) <- expectRight (allocateClientObject Wayland.wlCallbackInterface 1 table0)
  table2 <- expectRight (destroyObjectByRequest callback table1)
  let deletedId = objectIdValue (objectId callback)
  payload <- encodedUint deletedId
  let frame = RawFrame (objectId display) (Opcode 1) payload
  prepared <- expectRight (prepareEvent Wayland.wlDisplayInterface display decodeDisplayEvent frame table2)
  decodedId <- case runCompleteArgumentDecoder (preparedEventArguments prepared) (rawFramePayload frame) [] of
    Done (Wayland.WlDisplayDeleteId value) _ [] -> pure value
    result -> assertFailure ("unexpected decode result: " <> show result)
  table3 <- expectRight (finalizeDeleteIdEvent Wayland.wlDisplayInterface prepared decodedId table2)
  assertEqual "deleted object removed" Nothing (lookupObjectInfo (objectId callback) table3)
  (_, reused) <- expectRight (allocateClientObject Wayland.wlCallbackInterface 1 table3)
  assertEqual "ID reusable" (objectId callback) (objectId reused)

testWrongDeleteIdEvent :: Assertion
testWrongDeleteIdEvent = do
  (table, display) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  let frame = RawFrame (objectId display) (Opcode 0) ByteString.empty
  prepared <- expectRight (prepareEvent Wayland.wlDisplayInterface display decodeDisplayEvent frame table)
  expectLeft
    (ExpectedDeleteIdEvent "wl_display" "error")
    (finalizeDeleteIdEvent Wayland.wlDisplayInterface prepared 2 table)

callbackTable :: IO (ObjectTable, Object Wayland.WlCallback)
callbackTable = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  expectRight (allocateClientObject Wayland.wlCallbackInterface 1 table0)

versionedInterface :: Interface Versioned
versionedInterface = Interface "versioned" 3

decodeCallbackEvent :: Opcode -> Maybe (EventDecoder () (Wayland.WlCallbackEvent ()))
decodeCallbackEvent = Wayland.decodeWlCallbackEvent

decodeDisplayEvent :: Opcode -> Maybe (EventDecoder () (Wayland.WlDisplayEvent ()))
decodeDisplayEvent = Wayland.decodeWlDisplayEvent

encodedUint :: Word32 -> IO ByteString.ByteString
encodedUint value = case runArgumentEncoder (putUint value :: ArgumentEncoder () ()) of
  Left errorValue -> assertFailure ("unexpected encoding error: " <> show errorValue)
  Right ((), payload, []) -> pure payload
  Right (_, _, descriptors) -> assertFailure ("unexpected descriptors: " <> show (length descriptors))

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

expectLeft :: (Eq errorValue, Show errorValue) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result = case result of
  Left actual -> assertEqual "error" expected actual
  Right _ -> assertFailure "expected error"
