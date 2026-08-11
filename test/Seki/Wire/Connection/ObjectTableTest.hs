{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Wire.Connection.ObjectTableTest (tests) where

import Data.Either (isRight)
import Data.List (nub)
import Data.Word (Word32)
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Connection.ObjectTable (
  Object,
  ObjectInfo (..),
  ObjectLifecycle (..),
  ObjectOrigin (..),
  ObjectTable,
  ObjectTableError (..),
  acknowledgeDeleteId,
  allocateClientObject,
  closeObjectTable,
  defaultObjectLimit,
  destroyObjectByEvent,
  destroyObjectByRequest,
  lookupObjectInfo,
  makeObjectLimit,
  newObjectTable,
  objectCount,
  objectId,
  objectVersion,
  registerServerObject,
  requireActiveObject,
  retiredObjectCount,
  serverObjectIdMinimum,
  validateObject,
 )
import Seki.Wire.Types (ObjectId, mkObjectId, objectIdValue)
import Test.QuickCheck (Property, counterexample)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QuickCheck

data Display

data Alpha

data Beta

tests :: TestTree
tests =
  testGroup
    "Wayland object table"
    [ testCase "installs the implicit display object" testInitialDisplay
    , testCase "allocates dense typed client IDs" testDenseAllocation
    , testCase "enforces versions and the object limit" testVersionsAndLimit
    , testCase "tombstones destructor requests until delete_id" testDestructorRequest
    , testCase "tombstones destructor events until delete_id" testDestructorEvent
    , testCase "accepts delete_id for an active client object" testActiveDeleteId
    , testCase "rejects invalid delete_id events" testInvalidDeleteId
    , testCase "tracks the separate server ID namespace" testServerObjects
    , testCase "rejects mismatched descriptors" testInterfaceMismatch
    , testCase "closes the table idempotently" testClose
    , QuickCheck.testProperty "matches an allocate/destroy/reuse model" propObjectLifecycleModel
    ]

testInitialDisplay :: Assertion
testInitialDisplay = do
  (table, display) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  assertEqual "display ID" 1 (objectIdValue (objectId display))
  assertEqual "display version" 1 (objectVersion display)
  assertEqual "one table entry" 1 (objectCount table)
  assertEqual
    "display metadata"
    (Just (ObjectInfo (objectId display) "wl_display" 1 1 DisplayObject ObjectActive))
    (lookupObjectInfo (objectId display) table)
  assertBool "typed display validates" (isRight (validateObject displayInterface display table))

testDenseAllocation :: Assertion
testDenseAllocation = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  (table1, alpha) <- expectRight (allocateClientObject alphaInterface 3 table0)
  (table2, beta) <- expectRight (allocateClientObject betaInterface 1 table1)
  assertEqual "first client ID" 2 (objectIdValue (objectId alpha))
  assertEqual "second client ID" 3 (objectIdValue (objectId beta))
  assertEqual "negotiated version" 3 (objectVersion alpha)
  alphaInfo <- expectRight (requireActiveObject (objectId alpha) table2)
  assertEqual "client origin" ClientCreatedObject (objectInfoOrigin alphaInfo)

testVersionsAndLimit :: Assertion
testVersionsAndLimit = do
  limit <- expectRight (makeObjectLimit 2)
  (table0, _) <- expectRight (newObjectTable limit displayInterface)
  expectLeft (InvalidObjectVersion "alpha" 0 3) (allocateClientObject alphaInterface 0 table0)
  expectLeft (InvalidObjectVersion "alpha" 4 3) (allocateClientObject alphaInterface 4 table0)
  (table1, _) <- expectRight (allocateClientObject alphaInterface 3 table0)
  expectLeft (ObjectLimitExceeded 2) (allocateClientObject betaInterface 1 table1)
  expectLeft (InvalidObjectLimit 0) (makeObjectLimit 0)

testDestructorRequest :: Assertion
testDestructorRequest = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  (table1, original) <- expectRight (allocateClientObject alphaInterface 2 table0)
  table2 <- expectRight (destroyObjectByRequest original table1)
  assertEqual "one tombstone" 1 (retiredObjectCount table2)
  expectLeft (ObjectNotActive (objectId original)) (validateObject alphaInterface original table2)
  (table3, nextObject) <- expectRight (allocateClientObject alphaInterface 2 table2)
  assertEqual "tombstoned ID is skipped" 3 (objectIdValue (objectId nextObject))
  table4 <- expectRight (acknowledgeDeleteId 2 table3)
  (table5, replacement) <- expectRight (allocateClientObject alphaInterface 2 table4)
  assertEqual "acknowledged ID is reused" 2 (objectIdValue (objectId replacement))
  expectLeft (StaleObjectHandle (objectId original)) (validateObject alphaInterface original table5)

testDestructorEvent :: Assertion
testDestructorEvent = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  (table1, callback) <- expectRight (allocateClientObject alphaInterface 1 table0)
  table2 <- expectRight (destroyObjectByEvent callback table1)
  assertEqual "destructor event tombstone" 1 (retiredObjectCount table2)
  table3 <- expectRight (acknowledgeDeleteId 2 table2)
  assertEqual "tombstone removed" 1 (objectCount table3)

testActiveDeleteId :: Assertion
testActiveDeleteId = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  (table1, object) <- expectRight (allocateClientObject alphaInterface 1 table0)
  table2 <- expectRight (acknowledgeDeleteId 2 table1)
  assertEqual "active object removed" Nothing (lookupObjectInfo (objectId object) table2)

testInvalidDeleteId :: Assertion
testInvalidDeleteId = do
  (table, _) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  expectLeft (InvalidDeleteId 0) (acknowledgeDeleteId 0 table)
  expectLeft DeleteIdForDisplay (acknowledgeDeleteId 1 table)
  expectLeft (UnknownObject (requireObjectId 7)) (acknowledgeDeleteId 7 table)

testServerObjects :: Assertion
testServerObjects = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  let serverId = requireObjectId serverObjectIdMinimum
  (table1, serverObject) <- expectRight (registerServerObject serverId alphaInterface 2 table0)
  assertEqual "server ID" serverObjectIdMinimum (objectIdValue (objectId serverObject))
  serverInfo <- expectRight (requireActiveObject serverId table1)
  assertEqual "server origin" ServerCreatedObject (objectInfoOrigin serverInfo)
  expectLeft (DeleteIdForServerObject serverId) (acknowledgeDeleteId serverObjectIdMinimum table1)
  expectLeft (ObjectIdAlreadyRegistered serverId) (registerServerObject serverId betaInterface 1 table1)
  expectLeft (ServerObjectIdOutOfRange 2) (registerServerObject (requireObjectId 2) betaInterface 1 table1)
  table2 <- expectRight (destroyObjectByRequest serverObject table1)
  assertEqual "server object removed immediately" Nothing (lookupObjectInfo serverId table2)

testInterfaceMismatch :: Assertion
testInterfaceMismatch = do
  (table0, _) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  (table1, object) <- expectRight (allocateClientObject alphaInterface 1 table0)
  expectLeft
    (ObjectInterfaceMismatch (objectId object) "other_alpha" "alpha")
    (validateObject otherAlphaInterface object table1)

testClose :: Assertion
testClose = do
  (table0, display) <- expectRight (newObjectTable defaultObjectLimit displayInterface)
  let closed = closeObjectTable table0
  assertEqual "closed table is empty" 0 (objectCount closed)
  assertEqual "close is idempotent" closed (closeObjectTable closed)
  expectLeft ObjectTableClosed (allocateClientObject alphaInterface 1 closed)
  expectLeft ObjectTableClosed (validateObject displayInterface display closed)

propObjectLifecycleModel :: [Bool] -> Property
propObjectLifecycleModel commands =
  case firstShow (newObjectTable defaultObjectLimit displayInterface) >>= \(table, _) -> runModel (take 200 commands) table [] of
    Left message -> counterexample message False
    Right () -> counterexample "object table matched model" True

runModel :: [Bool] -> ObjectTable -> [Object Alpha] -> Either String ()
runModel [] table objects = checkModel table objects
runModel (allocate : commands) table objects
  | allocate || null objects = do
      (nextTable, object) <- firstShow (allocateClientObject alphaInterface 1 table)
      checkModel nextTable (object : objects)
      runModel commands nextTable (object : objects)
  | object : remaining <- objects = do
      retired <- firstShow (destroyObjectByRequest object table)
      released <- firstShow (acknowledgeDeleteId (objectIdValue (objectId object)) retired)
      checkModel released remaining
      runModel commands released remaining
  | otherwise = Left "unreachable empty object model"

checkModel :: ObjectTable -> [Object Alpha] -> Either String ()
checkModel table objects
  | objectCount table /= length objects + 1 = Left "object count diverged"
  | length rawIds /= length (nub rawIds) = Left "duplicate active object ID"
  | all (\object -> isRight (validateObject alphaInterface object table)) objects = Right ()
  | otherwise = Left "active model object did not validate"
 where
  rawIds = map (objectIdValue . objectId) objects

firstShow :: (Show errorValue) => Either errorValue value -> Either String value
firstShow = either (Left . show) Right

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

expectLeft :: (Eq errorValue, Show errorValue, Show value) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result = case result of
  Left actual -> assertEqual "error" expected actual
  Right value -> assertFailure ("expected error, received: " <> show value)

requireObjectId :: Word32 -> ObjectId
requireObjectId value = case mkObjectId value of
  Nothing -> error "test object ID must be non-zero"
  Just identifier -> identifier

displayInterface :: Interface Display
displayInterface = Interface "wl_display" 1

alphaInterface :: Interface Alpha
alphaInterface = Interface "alpha" 3

otherAlphaInterface :: Interface Alpha
otherAlphaInterface = Interface "other_alpha" 3

betaInterface :: Interface Beta
betaInterface = Interface "beta" 1
