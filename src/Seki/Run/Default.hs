{-# LANGUAGE LambdaCase #-}

-- | Concrete Full-layout policy used by the default executable.
module Seki.Run.Default (
  DefaultConfig (..),
  DefaultConfigError (..),
  DefaultRunError (..),
  defaultConfig,
  defaultRuntimeHandlers,
  runDefault,
  prepareDefaultManagePlan,
  prepareDefaultRenderPlan,
  defaultFocusTarget,
  defaultCloseTarget,
)
where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, catch)
import Control.Monad (void)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Full (prepareFullManagePlan, prepareFullRenderPlan)
import Seki.Run.Inventory (
  RiverWindowState (..),
  riverInventoryActiveWindow,
 )
import Seki.Run.Phase (ManageM, RenderM, closeWindow, focusWindow)
import Seki.Run.River (RiverWindowManagerHandlers (..))
import Seki.Run.Runtime (
  RiverManageAction (..),
  RiverRuntimeError,
  RiverRuntimeHandlers (..),
  RiverRuntimeSnapshot (..),
  RiverSeatBindings (..),
  noRiverManageAction,
  openRiverSession,
  runRiverSession,
 )
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (Object)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

data DefaultConfig = DefaultConfig
  { defaultTerminalBinding :: RiverKeyBinding
  , defaultCloseBinding :: RiverKeyBinding
  , defaultTerminalProcess :: IO ()
  }

newtype DefaultConfigError
  = DefaultBindingsConflict RiverKeyBinding
  deriving (Eq, Show)

data DefaultRunError
  = DefaultRunConfigError DefaultConfigError
  | DefaultRunRuntimeError RiverRuntimeError
  deriving (Eq, Show)

{- | xmonad-style defaults: Mod4-Shift-Return launches @foot@ and
Mod4-Shift-c closes the active window.
-}
defaultConfig :: DefaultConfig
defaultConfig =
  DefaultConfig
    { defaultTerminalBinding = keyBinding 0xff0d
    , defaultCloseBinding = keyBinding 0x63
    , defaultTerminalProcess = callProcess "foot" []
    }
 where
  keyBinding keysym =
    RiverKeyBinding
      { riverKeyBindingKeysym = keysym
      , riverKeyBindingModifiers = River.RiverSeatV1ModifiersFlags 65
      , riverKeyBindingLayoutOverride = Just 0
      }

-- | Assemble the default runtime policy after validating binding identity.
defaultRuntimeHandlers :: DefaultConfig -> Either DefaultConfigError RiverRuntimeHandlers
defaultRuntimeHandlers config
  | defaultTerminalBinding config == defaultCloseBinding config =
      Left (DefaultBindingsConflict (defaultTerminalBinding config))
  | otherwise =
      Right
        RiverRuntimeHandlers
          { riverProtocolHandlers = noOpProtocolHandlers
          , riverKeyBindings = [defaultTerminalBinding config, defaultCloseBinding config]
          , handleRiverKeyBindingEvent = handleBindingEvent
          , prepareRiverManagePlan = prepareDefaultManagePlan
          , prepareRiverRenderPlan = prepareDefaultRenderPlan
          }
 where
  handleBindingEvent definition _ = \case
    Xkb.RiverXkbBindingV1Pressed
      | definition == defaultTerminalBinding config -> do
          launchAsync (defaultTerminalProcess config)
          pure noRiverManageAction
      | definition == defaultCloseBinding config ->
          pure (RiverManageAction prepareClosePlan)
    _ -> pure noRiverManageAction

-- | Open the default Wayland display and run the policy until shutdown.
runDefault :: DefaultConfig -> IO (Either DefaultRunError ())
runDefault config =
  case defaultRuntimeHandlers config of
    Left errorValue -> pure (Left (DefaultRunConfigError errorValue))
    Right handlers ->
      openRiverSession handlers >>= \case
        Left errorValue -> pure (Left (DefaultRunRuntimeError errorValue))
        Right session ->
          either (Left . DefaultRunRuntimeError) Right <$> runRiverSession session

-- | Full placement plus keyboard focus for the active window on the first seat.
prepareDefaultManagePlan :: RiverRuntimeSnapshot -> ManageM ()
prepareDefaultManagePlan snapshot = do
  prepareFullManagePlan (riverRuntimeInventory snapshot)
  case defaultFocusTarget snapshot of
    Nothing -> pure ()
    Just (seat, window) -> focusWindow seat (riverWindowObject window)

prepareDefaultRenderPlan :: RiverRuntimeSnapshot -> RenderM ()
prepareDefaultRenderPlan = prepareFullRenderPlan . riverRuntimeInventory

defaultFocusTarget ::
  RiverRuntimeSnapshot ->
  Maybe (Object River.RiverSeatV1, RiverWindowState)
defaultFocusTarget snapshot = do
  seatState <- case riverRuntimeSeatBindings snapshot of
    [] -> Nothing
    first : _ -> Just first
  window <- defaultCloseTarget snapshot
  pure (riverBindingsSeat seatState, window)

defaultCloseTarget :: RiverRuntimeSnapshot -> Maybe RiverWindowState
defaultCloseTarget = riverInventoryActiveWindow . riverRuntimeInventory

prepareClosePlan :: RiverRuntimeSnapshot -> ManageM ()
prepareClosePlan snapshot =
  case defaultCloseTarget snapshot of
    Nothing -> pure ()
    Just window -> closeWindow (riverWindowObject window)

launchAsync :: IO () -> IO ()
launchAsync action =
  void . forkIO $
    action `catch` \errorValue ->
      hPutStrLn stderr ("seki: terminal launch failed: " <> show (errorValue :: IOException))

noOpProtocolHandlers :: RiverWindowManagerHandlers
noOpProtocolHandlers =
  RiverWindowManagerHandlers
    { handleRiverWindowManagerEvent = \_ _ -> pure ()
    , handleRiverWindowCreated = \_ -> pure ()
    , handleRiverWindowEvent = \_ _ -> pure ()
    , handleRiverOutputCreated = \_ -> pure ()
    , handleRiverOutputEvent = \_ _ -> pure ()
    , handleRiverSeatCreated = \_ -> pure ()
    , handleRiverSeatEvent = \_ _ -> pure ()
    }
