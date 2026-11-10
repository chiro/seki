{-# LANGUAGE PatternSynonyms #-}

-- | Configure and run seki with pure actions and layouts.
module Seki (
  seki,
  module Seki.Input,
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
  Button (..),
  pattern Button1,
  pattern Button2,
  pattern Button3,
  PointerBinding (..),
  PointerAction (..),
  PointerBindings,
  def,
  defaultKeys,
  defaultPointerBindings,
  additionalKeys,
  removeKeys,
  windows,
  sendMessage,
  spawn,
  spawnOnce,
  kill,
  withFocused,
  float,
  sink,
  toggleFullscreen,
  restart,
  exitSession,
  screenWorkspace,
  Scratchpad (..),
  toggleScratchpad,
  resetScratchpad,
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
  doFloat,
  defaultManageHook,
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
import Seki.Input
import Seki.Internal.Config (Action, Bindings, BorderColor (..), BorderConfig (..), Button (..), Config (..), KeyBinding (..), Keysym (..), ManageHook, Mods (..), PointerAction (..), PointerBinding (..), PointerBindings, StdLayouts, additionalKeys, def, defaultKeys, defaultManageHook, defaultPointerBindings, removeKeys, pattern Button1, pattern Button2, pattern Button3, pattern Control, pattern Mod1, pattern Mod3, pattern Mod4, pattern Mod5, pattern Shift)
import Seki.Internal.Core.Action qualified as Core
import Seki.Internal.Core.ManageHook (Query, appId, composeAll, idHook, isDialog, title, (-->), (=?))
import Seki.Internal.Core.ManageHook qualified as Core
import Seki.Internal.Core.Scratchpad.Types (Scratchpad (..))
import Seki.Layout.Full
import Seki.Layout.Tall
import Seki.Run.Config (runConfig)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

{- | Validate the configuration, then run until river disconnects. The command
line is empty, or @--restore FILE@ when a restart started this process.

With exactly @--check@, validate this configuration using the runtime policy
guards and exit successfully without starting a session. This allocates policy
state but does not discover Wayland, consume restore files, run hooks or start
subprocesses or IPC. Usage and policy failures print the current error and
exit with status 1; a successful check exits with status 0.

This checks policy values, not all Haskell code. User code is trusted; IO before
this call and unsafe evaluation are not sandboxed. There is no checker timeout.
-}
seki :: (LayoutClass l) => Config l -> IO ()
seki config = getArgs >>= (`runConfig` config) >>= either failWith pure
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

-- | Launch a shell command unless 'spawnOnce' already launched the same command string.
spawnOnce :: String -> Action l ()
spawnOnce = Core.spawnOnce

-- | Ask river to close the focused window.
kill :: Action l ()
kill = Core.kill

-- | Move the new window to the workspace with this tag, keeping focus where it was.
doShift :: String -> ManageHook l
doShift = Core.doShift

-- | Tile the new window.
doSink :: ManageHook l
doSink = Core.doSink

-- | Float the new window at the size its client picks, centered on its output.
doFloat :: ManageHook l
doFloat = Core.doFloat

-- | Run an action on the focused window, if there is one.
withFocused :: (WindowId -> Action l ()) -> Action l ()
withFocused = Core.withFocused

{- | Float a window. A tiled window keeps the size its client picks and is
centered on its output; a floating window keeps its place.
-}
float :: WindowId -> Action l ()
float = Core.float

-- | Tile a floating window again.
sink :: WindowId -> Action l ()
sink = Core.sink

{- | Make a window fullscreen on its workspace's output, or end its fullscreen.
Only a workspace's focused window can be fullscreen, and focusing another
window on that workspace ends it.
-}
toggleFullscreen :: WindowId -> Action l ()
toggleFullscreen = Core.toggleFullscreen

{- | Restart seki in place, as @Mod-q@ does by default. seki first checks that
the executable it started from is still executable and that it can reconnect;
if not, it logs why and keeps running. Otherwise it saves the window state,
replaces itself with that executable, and restores the state there.
-}
restart :: Action l ()
restart = Core.restart

{- | End the Wayland session, as @Mod-Shift-q@ does by default. river exits and
closes every client, seki included, as xmonad's default @Mod-Shift-q@ ends
the X session. A river whose window-management protocol is older than
version 4 has no request for this, and the action does nothing there.
-}
exitSession :: Action l ()
exitSession = Core.exitSession

{- | The workspace on the screen at this index, as xmonad's @screenWorkspace@
gives it. Screens count from 0, left to right by where river places each
output, then top to bottom. An index with no screen, or an output still
waiting for a workspace, gives 'Nothing'. By default @Mod-w@, @Mod-e@ and
@Mod-r@ view the workspace on screens 0, 1 and 2, and with @Shift@ move the
focused window there:

> screenWorkspace 1 >>= mapM_ (windows . view)
-}
screenWorkspace :: Int -> Action l (Maybe String)
screenWorkspace = Core.screenWorkspace

-- | Open, hide or bring a named pad. Unresolved launches coalesce.
toggleScratchpad :: String -> Action l ()
toggleScratchpad = Core.toggleScratchpad

-- | Clear an unresolved attempt after operator cleanup; no process is killed.
resetScratchpad :: String -> Action l ()
resetScratchpad = Core.resetScratchpad
