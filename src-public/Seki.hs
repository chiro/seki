{-# LANGUAGE PatternSynonyms #-}

-- | Configure and run seki with pure actions and layouts.
module Seki (
  seki,
  (.|.),
  Config (..),
  BorderConfig (..),
  BorderColor (..),
  StdLayouts,
  Action,
  Bindings,
  Mods (..),
  pattern Shift,
  pattern Control,
  pattern Mod1,
  pattern Mod3,
  pattern Mod4,
  pattern Mod5,
  Keysym (..),
  KeyBinding (..),
  def,
  defaultKeys,
  windows,
  sendMessage,
  spawn,
  kill,
  ManageHook,
  Query,
  appId,
  title,
  isDialog,
  (=?),
  (-->),
  composeAll,
  idHook,
  doShift,
  doSink,
  module Seki.Core.Geometry,
  module Seki.Core.Layout,
  module Seki.Layout.Full,
  module Seki.Layout.Tall,
  StackSet,
  Stack (..),
  WindowId,
  OutputId,
  focusUp,
  focusDown,
  focusMaster,
  swapUp,
  swapDown,
  swapMaster,
  shiftMaster,
  greedyView,
  view,
  shift,
  shiftWin,
  peek,
) where

import Data.Bits ((.|.))
import Seki.Core.Geometry
import Seki.Core.Layout
import Seki.Core.StackSet (OutputId, Stack (..), StackSet, WindowId, focusDown, focusMaster, focusUp, greedyView, peek, shift, shiftMaster, shiftWin, swapDown, swapMaster, swapUp, view)
import Seki.Internal.Config (Action, Bindings, BorderColor (..), BorderConfig (..), Config (..), KeyBinding (..), Keysym (..), ManageHook, Mods (..), StdLayouts, def, defaultKeys, pattern Control, pattern Mod1, pattern Mod3, pattern Mod4, pattern Mod5, pattern Shift)
import Seki.Internal.Core.Action qualified as Core
import Seki.Internal.Core.ManageHook (Query, appId, composeAll, idHook, isDialog, title, (-->), (=?))
import Seki.Internal.Core.ManageHook qualified as Core
import Seki.Layout.Full
import Seki.Layout.Tall
import Seki.Run.Config (runConfig)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Validate the configuration, then run until river disconnects.
seki :: (LayoutClass l) => Config l -> IO ()
seki config = runConfig config >>= either failWith pure
 where
  failWith err = hPutStrLn stderr ("seki: " ++ show err) >> exitFailure

-- | Change the workspace and window state and reclaim keyboard focus.
windows :: (StackSet String l -> StackSet String l) -> Action l ()
windows = Core.windows

-- | Send a message to the current workspace's layout.
sendMessage :: (LayoutClass l, Message m) => m -> Action l ()
sendMessage = Core.sendMessage

-- | Launch a shell command without waiting for it.
spawn :: String -> Action l ()
spawn = Core.spawn

-- | Ask river to close the focused window.
kill :: Action l ()
kill = Core.kill

-- | Move the new window to the workspace with this tag, keeping focus where it was.
doShift :: String -> ManageHook l
doShift = Core.doShift

-- | Tile the new window.
doSink :: ManageHook l
doSink = Core.doSink
