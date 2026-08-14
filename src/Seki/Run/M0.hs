{-# LANGUAGE LambdaCase #-}

-- | Concrete Full-layout policy used by the M0 validation executable.
module Seki.Run.M0 (
  M0Config (..),
  M0ConfigError (..),
  defaultM0Config,
  m0RuntimeHandlers,
  prepareM0ManagePlan,
  prepareM0RenderPlan,
  m0FocusTarget,
  m0CloseTarget,
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
  RiverRuntimeHandlers (..),
  RiverRuntimeSnapshot (..),
  RiverSeatBindings (..),
  noRiverManageAction,
 )
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (Object)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

data M0Config = M0Config
  { m0TerminalBinding :: RiverKeyBinding
  , m0CloseBinding :: RiverKeyBinding
  , m0TerminalProcess :: IO ()
  }

data M0ConfigError
  = M0BindingsConflict RiverKeyBinding
  deriving (Eq, Show)

{- | xmonad-style defaults: Mod4-Shift-Return launches @foot@ and
Mod4-Shift-c closes the active window.
-}
defaultM0Config :: M0Config
defaultM0Config =
  M0Config
    { m0TerminalBinding = keyBinding 0xff0d
    , m0CloseBinding = keyBinding 0x63
    , m0TerminalProcess = callProcess "foot" []
    }
 where
  keyBinding keysym =
    RiverKeyBinding
      { riverKeyBindingKeysym = keysym
      , riverKeyBindingModifiers = River.RiverSeatV1ModifiersFlags 65
      , riverKeyBindingLayoutOverride = Just 0
      }

-- | Assemble the complete M0 runtime policy after validating binding identity.
m0RuntimeHandlers :: M0Config -> Either M0ConfigError RiverRuntimeHandlers
m0RuntimeHandlers config
  | m0TerminalBinding config == m0CloseBinding config =
      Left (M0BindingsConflict (m0TerminalBinding config))
  | otherwise =
      Right
        RiverRuntimeHandlers
          { riverProtocolHandlers = noOpProtocolHandlers
          , riverKeyBindings = [m0TerminalBinding config, m0CloseBinding config]
          , handleRiverKeyBindingEvent = handleBindingEvent
          , prepareRiverManagePlan = prepareM0ManagePlan
          , prepareRiverRenderPlan = prepareM0RenderPlan
          }
 where
  handleBindingEvent definition _ = \case
    Xkb.RiverXkbBindingV1Pressed
      | definition == m0TerminalBinding config -> do
          launchAsync (m0TerminalProcess config)
          pure noRiverManageAction
      | definition == m0CloseBinding config ->
          pure (RiverManageAction prepareClosePlan)
    _ -> pure noRiverManageAction

-- | Full placement plus keyboard focus for the active window on the first seat.
prepareM0ManagePlan :: RiverRuntimeSnapshot -> ManageM ()
prepareM0ManagePlan snapshot = do
  prepareFullManagePlan (riverRuntimeInventory snapshot)
  case m0FocusTarget snapshot of
    Nothing -> pure ()
    Just (seat, window) -> focusWindow seat (riverWindowObject window)

prepareM0RenderPlan :: RiverRuntimeSnapshot -> RenderM ()
prepareM0RenderPlan = prepareFullRenderPlan . riverRuntimeInventory

m0FocusTarget ::
  RiverRuntimeSnapshot ->
  Maybe (Object River.RiverSeatV1, RiverWindowState)
m0FocusTarget snapshot = do
  seatState <- case riverRuntimeSeatBindings snapshot of
    [] -> Nothing
    first : _ -> Just first
  window <- m0CloseTarget snapshot
  pure (riverBindingsSeat seatState, window)

m0CloseTarget :: RiverRuntimeSnapshot -> Maybe RiverWindowState
m0CloseTarget = riverInventoryActiveWindow . riverRuntimeInventory

prepareClosePlan :: RiverRuntimeSnapshot -> ManageM ()
prepareClosePlan snapshot =
  case m0CloseTarget snapshot of
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
