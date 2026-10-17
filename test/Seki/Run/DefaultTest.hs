module Seki.Run.DefaultTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket_)
import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Default (
  DefaultConfig (..),
  DefaultConfigError (..),
  DefaultRunError (..),
  defaultCloseTarget,
  defaultConfig,
  defaultFocusTarget,
  defaultRuntimeHandlers,
  runDefault,
 )
import Seki.Run.Inventory (
  RiverWindowState (..),
  activateRiverWindow,
  addRiverWindow,
  emptyRiverInventory,
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
import System.Environment (getEnv, setEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, fileExist, setFileMode)
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), closeFd, defaultFileFlags, fdWrite, openFd)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "default policy"
    [ testCase "uses xmonad-style default bindings" testDefaultBindings
    , testCase "rejects conflicting bindings" testBindingConflict
    , testCase "rejects invalid configuration before Wayland startup" testRunConfigFailure
    , testCase "selects the active window and first seat" testTargets
    , testCase "launches the terminal action asynchronously" testTerminalAction
    , testCase "keeps running while the default terminal is open" testDefaultTerminalDoesNotBlock
    ]

testDefaultBindings :: Assertion
testDefaultBindings = do
  let terminal = defaultTerminalBinding defaultConfig
      close = defaultCloseBinding defaultConfig
  assertEqual "terminal keysym" 0xff0d (riverKeyBindingKeysym terminal)
  assertEqual "close keysym" 0x63 (riverKeyBindingKeysym close)
  assertEqual "terminal modifiers" (River.RiverSeatV1ModifiersFlags 65) (riverKeyBindingModifiers terminal)
  assertEqual "close modifiers" (River.RiverSeatV1ModifiersFlags 65) (riverKeyBindingModifiers close)
  assertEqual "terminal layout" (Just 0) (riverKeyBindingLayoutOverride terminal)
  assertEqual "close layout" (Just 0) (riverKeyBindingLayoutOverride close)

testBindingConflict :: Assertion
testBindingConflict = do
  let binding = defaultTerminalBinding defaultConfig
      config = defaultConfig{defaultCloseBinding = binding}
  case defaultRuntimeHandlers config of
    Left errorValue -> assertEqual "configuration error" (DefaultBindingsConflict binding) errorValue
    Right _ -> assertFailure "expected conflicting bindings to fail"

testRunConfigFailure :: Assertion
testRunConfigFailure = do
  let binding = defaultTerminalBinding defaultConfig
      config = defaultConfig{defaultCloseBinding = binding}
  assertEqual
    "run error"
    (Left (DefaultRunConfigError (DefaultBindingsConflict binding)))
    =<< runDefault config

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
  assertEqual "close target" (Just (objectId first)) (objectId . riverWindowObject <$> defaultCloseTarget snapshot)
  assertEqual
    "focus target"
    (Just (objectId seat, objectId first))
    ((\(targetSeat, window) -> (objectId targetSeat, objectId (riverWindowObject window))) <$> defaultFocusTarget snapshot)

testTerminalAction :: Assertion
testTerminalAction = do
  launched <- newEmptyMVar
  bindingObject <- makeClientObject Xkb.riverXkbBindingV1Interface
  let config = defaultConfig{defaultTerminalProcess = putMVar launched ()}
  handlers <- case defaultRuntimeHandlers config of
    Left errorValue -> assertFailure ("unexpected configuration error: " <> show errorValue)
    Right value -> pure value
  _ <-
    handleRiverKeyBindingEvent
      handlers
      (defaultTerminalBinding config)
      bindingObject
      Xkb.RiverXkbBindingV1Pressed
  takeMVar launched

-- The test suite, like the seki executable, uses GHC's non-threaded runtime,
-- where a thread blocked waiting for a child process stops every thread.
testDefaultTerminalDoesNotBlock :: Assertion
testDefaultTerminalDoesNotBlock =
  withSystemTempDirectory "seki-default-terminal" $ \dir -> do
    let fifo = dir <> "/go"
        started = dir <> "/started"
        done = dir <> "/done"
        foot = dir <> "/foot"
    createNamedPipe fifo 0o600
    writeFile foot . unlines $
      [ "#!/bin/sh"
      , "exec 3<> " <> fifo
      , "touch " <> started
      , "timeout 10 head -n 1 <&3 > /dev/null"
      , "touch " <> done
      ]
    setFileMode foot 0o755
    bindingObject <- makeClientObject Xkb.riverXkbBindingV1Interface
    handlers <- case defaultRuntimeHandlers defaultConfig of
      Left errorValue -> assertFailure ("unexpected configuration error: " <> show errorValue)
      Right value -> pure value
    path <- getEnv "PATH"
    bracket_ (setEnv "PATH" (dir <> ":" <> path)) (setEnv "PATH" path) $ do
      _ <-
        handleRiverKeyBindingEvent
          handlers
          (defaultTerminalBinding defaultConfig)
          bindingObject
          Xkb.RiverXkbBindingV1Pressed
      waitFor "the terminal to start" started
      assertBool "the terminal is still open while other threads run" . not =<< fileExist done
      fd <- openFd fifo WriteOnly defaultFileFlags{nonBlock = True}
      _ <- fdWrite fd "go\n"
      closeFd fd
      waitFor "the terminal to exit" done

waitFor :: String -> FilePath -> Assertion
waitFor label path = go (500 :: Int)
 where
  go 0 = assertFailure ("timed out waiting for " <> label)
  go n = fileExist path >>= \exists -> if exists then pure () else threadDelay 10000 >> go (n - 1)

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
