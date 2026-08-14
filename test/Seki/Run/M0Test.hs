{-# LANGUAGE PatternSynonyms #-}

module Seki.Run.M0Test (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Inventory (
  RiverWindowState (..),
  activateRiverWindow,
  addRiverWindow,
  emptyRiverInventory,
 )
import Seki.Run.M0 (
  M0Config (..),
  M0ConfigError (..),
  defaultM0Config,
  m0CloseTarget,
  m0FocusTarget,
  m0RuntimeHandlers,
 )
import Seki.Run.Runtime (
  RiverRuntimeHandlers (..),
  RiverRuntimeSnapshot (..),
  RiverSeatBindings (..),
 )
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Connection.ObjectTable (
  Object,
  allocateClientObject,
  defaultObjectLimit,
  newObjectTable,
  objectId,
  registerServerObject,
 )
import Seki.Wire.Types (mkObjectId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "M0 policy"
    [ testCase "uses xmonad-style default bindings" testDefaultBindings
    , testCase "rejects conflicting bindings" testBindingConflict
    , testCase "selects the active window and first seat" testTargets
    , testCase "launches the terminal action asynchronously" testTerminalAction
    ]

testDefaultBindings :: Assertion
testDefaultBindings = do
  let terminal = m0TerminalBinding defaultM0Config
      close = m0CloseBinding defaultM0Config
  assertEqual "terminal keysym" 0xff0d (riverKeyBindingKeysym terminal)
  assertEqual "close keysym" 0x63 (riverKeyBindingKeysym close)
  assertEqual "terminal modifiers" (River.RiverSeatV1ModifiersFlags 65) (riverKeyBindingModifiers terminal)
  assertEqual "close modifiers" (River.RiverSeatV1ModifiersFlags 65) (riverKeyBindingModifiers close)
  assertEqual "terminal layout" (Just 0) (riverKeyBindingLayoutOverride terminal)
  assertEqual "close layout" (Just 0) (riverKeyBindingLayoutOverride close)

testBindingConflict :: Assertion
testBindingConflict = do
  let binding = m0TerminalBinding defaultM0Config
      config = defaultM0Config{m0CloseBinding = binding}
  case m0RuntimeHandlers config of
    Left errorValue -> assertEqual "configuration error" (M0BindingsConflict binding) errorValue
    Right _ -> assertFailure "expected conflicting bindings to fail"

testTargets :: Assertion
testTargets = do
  first <- makeServerObject River.riverWindowV1Interface 0xff000050
  second <- makeServerObject River.riverWindowV1Interface 0xff000051
  seat <- makeServerObject River.riverSeatV1Interface 0xff000052
  inventory <-
    expectRight
      ( addRiverWindow first emptyRiverInventory
          >>= addRiverWindow second
          >>= activateRiverWindow (objectId first)
      )
  let snapshot = RiverRuntimeSnapshot inventory [RiverSeatBindings seat []]
  assertEqual "close target" (Just (objectId first)) (objectId . riverWindowObject <$> m0CloseTarget snapshot)
  assertEqual
    "focus target"
    (Just (objectId seat, objectId first))
    ((\(targetSeat, window) -> (objectId targetSeat, objectId (riverWindowObject window))) <$> m0FocusTarget snapshot)

testTerminalAction :: Assertion
testTerminalAction = do
  launched <- newEmptyMVar
  bindingObject <- makeClientObject Xkb.riverXkbBindingV1Interface
  let config = defaultM0Config{m0TerminalProcess = putMVar launched ()}
  handlers <- case m0RuntimeHandlers config of
    Left errorValue -> assertFailure ("unexpected configuration error: " <> show errorValue)
    Right value -> pure value
  _ <-
    handleRiverKeyBindingEvent
      handlers
      (m0TerminalBinding config)
      bindingObject
      Xkb.RiverXkbBindingV1Pressed
  takeMVar launched

makeServerObject :: Interface interface -> Word32 -> IO (Object interface)
makeServerObject interface rawIdentifier = do
  identifier <- maybe (assertFailure "invalid test object ID") pure (mkObjectId rawIdentifier)
  (initialTable, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (_, object) <-
    expectRight
      ( registerServerObject
          identifier
          interface
          (interfaceMaximumVersion interface)
          initialTable
      )
  pure object

makeClientObject :: Interface interface -> IO (Object interface)
makeClientObject interface = do
  (initialTable, _) <- expectRight (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
  (_, object) <-
    expectRight
      (allocateClientObject interface (interfaceMaximumVersion interface) initialTable)
  pure object

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure
