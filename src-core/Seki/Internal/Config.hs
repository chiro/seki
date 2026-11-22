{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Configuration shared by the public API and the runtime adapter.
module Seki.Internal.Config (
  Config (..),
  BorderConfig (..),
  BorderColor (..),
  StdLayouts,
  Action,
  Bindings,
  ManageHook,
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
  defaultManageHook,
  additionalKeys,
  removeKeys,
) where

import Data.Bits (Bits, (.|.))
import Data.Function (on)
import Data.Int (Int32)
import Data.List (nubBy)
import Data.Word (Word32)
import Seki.Internal.Core.Action qualified as Core
import Seki.Internal.Core.Layout (ChangeLayout (..), Choose, IncMasterN (..), LayoutClass, Mirror (..), Resize (..), (|||))
import Seki.Internal.Core.ManageHook (isDialog, (-->))
import Seki.Internal.Core.ManageHook qualified as Core
import Seki.Internal.Core.Scene (BorderColor (..))
import Seki.Internal.Core.Scratchpad.Types (Scratchpad)
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Internal.Input (InputRule)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))

{- | What a binding or hook does to seki's state, for layout type @l@. An action
is pure: it changes workspaces, windows, and layouts, and records effects such
as launching a command, which seki runs after the action returns.
-}
type Action l a = Core.Action String l a

-- | Key bindings as pairs of a key and the action it runs.
type Bindings l = [(KeyBinding, Action l ())]

{- | A rule that runs on each new window and can move, float, or tile it. Build
one from 'Query' predicates with '(-->)' and 'composeAll'.
-}
type ManageHook l = Core.ManageHook String l

-- | The layout type of 'def': Tall, Mirror Tall, and Full, cycled with 'NextLayout'.
type StdLayouts = Choose Tall (Choose (Mirror Tall) Full)

-- | XKB modifier bits. Combine modifiers with @(.|.)@.
newtype Mods = Mods Word32
  deriving stock (Eq, Show)
  deriving newtype (Bits)

{- | The named XKB modifiers. 'Mod1' is usually Alt and 'Mod4' the logo key.
There is no @Mod2@: it is usually Num Lock, and river ignores locked modifiers
when it matches a binding.
-}
pattern Shift, Control, Mod1, Mod3, Mod4, Mod5 :: Mods
pattern Shift = Mods 1
pattern Control = Mods 4
pattern Mod1 = Mods 8
pattern Mod3 = Mods 32
pattern Mod4 = Mods 64
pattern Mod5 = Mods 128

-- | An XKB keysym number, such as @Keysym 0xff0d@ for Return.
newtype Keysym = Keysym Word32 deriving (Eq, Show)

-- | A key chord: the modifiers that must be held and the key pressed with them.
data KeyBinding = KeyBinding
  { bindingModifiers :: Mods
  -- ^ The modifiers held with the key, such as @'Mod4' .|. 'Shift'@, or @Mods 0@ for none.
  , bindingKeysym :: Keysym
  -- ^ The key, as the keysym it produces.
  }
  deriving (Eq, Show)

-- | A Linux input event code for a pointer button.
newtype Button = Button Word32 deriving (Eq, Show)

-- | The left, middle, and right buttons: @BTN_LEFT@, @BTN_MIDDLE@, and @BTN_RIGHT@.
pattern Button1, Button2, Button3 :: Button
pattern Button1 = Button 0x110
pattern Button2 = Button 0x112
pattern Button3 = Button 0x111

-- | A pointer chord: the modifiers that must be held and the button pressed with them.
data PointerBinding = PointerBinding
  { pointerModifiers :: Mods
  -- ^ The modifiers held with the button, combined with @(.|.)@.
  , pointerButton :: Button
  -- ^ The button that starts the drag.
  }
  deriving (Eq, Show)

{- | Drag the window under the pointer: move it, or resize it from its
bottom-right corner. Either focuses the window and floats a tiled one where
it is shown, once the pointer moves.
-}
data PointerAction = MoveWindow | ResizeWindow deriving (Eq, Show)

-- | Pointer bindings as pairs of a chord and the drag it starts.
type PointerBindings = [(PointerBinding, PointerAction)]

-- | The borders river draws around every window seki shows.
data BorderConfig = BorderConfig
  { borderWidth :: Int32
  -- ^ The width in logical pixels, drawn outside the window's content. 0 draws none, and a negative width fails validation.
  , focusedBorderColor :: BorderColor
  -- ^ The color of the focused window's border.
  , unfocusedBorderColor :: BorderColor
  -- ^ The color of every other window's border.
  }
  deriving (Eq, Show)

{- | A complete seki configuration for layout type @l@. Start from 'def' and
override fields with a record update, then pass the result to @seki@.
seki validates it before it connects to river.
-}
data Config l = Config
  { scratchpads :: [Scratchpad]
  -- ^ Named scratchpads. The first one whose query matches a new window owns it.
  , inputRules :: [InputRule]
  -- ^ Input device settings, applied in order. See "Seki.Input".
  , modMask :: Mods
  -- ^ The modifier that 'defaultKeys' and 'defaultPointerBindings' put in every chord.
  , terminal :: String
  -- ^ The shell command that Mod-Shift-Return launches in 'defaultKeys'.
  , workspaces :: [String]
  -- ^ The workspace tags, in order. The list must be non-empty and hold no duplicate tags.
  , layoutHook :: l
  -- ^ The layout every workspace starts with.
  , keys :: Config l -> Bindings l
  -- ^ The key bindings. The function receives the final configuration, so the bindings see every other override.
  , borders :: BorderConfig
  , bindingsUseFixedLayout :: Bool
  -- ^ When 'True', river reads a binding's keysym from the keyboard's first layout, whichever layout is active, so each binding stays on the same physical key. When 'False', river reads it from the active layout.
  , manageHook :: ManageHook l
  -- ^ Runs on every new window, after it is inserted above the focus.
  , startupHook :: Action l ()
  -- ^ Runs once per seki process, when the first output arrives. Use
  -- 'Core.spawnOnce' for launches that must not repeat after a restart. Its
  -- type names the layout, so an update that changes the layout type sets it
  -- again, as it does 'keys' and 'manageHook'.
  , pointerBindings :: Config l -> PointerBindings
  -- ^ The pointer bindings. Like 'keys', the function receives the final configuration.
  }

-- | Nine workspaces, Tall, Mirror Tall, Full, xmonad-style key and pointer bindings, floating dialogs, and no startup hook.
def :: Config StdLayouts
def =
  Config
    { scratchpads = []
    , inputRules = []
    , modMask = Mod4
    , terminal = "foot"
    , workspaces = map show [1 .. 9 :: Int]
    , layoutHook = tall ||| Mirror tall ||| Full
    , keys = defaultKeys
    , borders = BorderConfig 2 (BorderColor 0 0x80000000 0xffffffff 0xffffffff) (BorderColor 0x40000000 0x40000000 0x40000000 0xffffffff)
    , bindingsUseFixedLayout = True
    , manageHook = defaultManageHook
    , startupHook = pure ()
    , pointerBindings = defaultPointerBindings
    }
 where
  tall = Tall 1 (3 / 100) (1 / 2)

{- | Add bindings, as EZConfig's @additionalKeys@ does. A new binding replaces
an existing one with the same identity, and among repeated new identities the
last wins. The existing bindings still see the final configuration.
-}
additionalKeys :: Config l -> Bindings l -> Config l
additionalKeys config new = config{keys = \final -> added ++ filter ((`notElem` map fst added) . fst) (keys config final)}
 where
  added = reverse (nubBy ((==) `on` fst) (reverse new))

-- | Remove bindings by identity, as EZConfig's @removeKeys@ does.
removeKeys :: Config l -> [KeyBinding] -> Config l
removeKeys config removed = config{keys = filter ((`notElem` removed) . fst) . keys config}

-- | Build bindings from the final configuration, including record overrides.
defaultKeys :: (LayoutClass l) => Config l -> Bindings l
defaultKeys config =
  [ key Shift 0xff0d (Core.spawn (terminal config))
  , key Shift 0x63 Core.kill
  , key (Mods 0) 0x20 (Core.sendMessage NextLayout)
  , key (Mods 0) 0x6a (Core.windows StackSet.focusDown)
  , key (Mods 0) 0x6b (Core.windows StackSet.focusUp)
  , key (Mods 0) 0xff0d (Core.windows StackSet.swapMaster)
  , key (Mods 0) 0x68 (Core.sendMessage Shrink)
  , key (Mods 0) 0x6c (Core.sendMessage Expand)
  , key (Mods 0) 0x2c (Core.sendMessage (IncMasterN 1))
  , key (Mods 0) 0x2e (Core.sendMessage (IncMasterN (-1)))
  , key (Mods 0) 0x74 (Core.withFocused Core.sink)
  , key (Mods 0) 0x71 Core.restart
  , key Shift 0x71 Core.exitSession
  ]
    ++ concat
      [ [key (Mods 0) symbol (Core.windows (StackSet.greedyView tag)), key Shift symbol (Core.windows (StackSet.shift tag))]
      | (tag, symbol) <- zip (workspaces config) [0x31 .. 0x39]
      ]
    ++ concat
      [ [key (Mods 0) symbol (onScreen StackSet.view), key Shift symbol (onScreen StackSet.shift)]
      | (screen, symbol) <- zip [0 ..] [0x77, 0x65, 0x72]
      , let onScreen f = Core.screenWorkspace screen >>= mapM_ (Core.windows . f)
      ]
 where
  key modifiers symbol action = (KeyBinding (modMask config .|. modifiers) (Keysym symbol), action)

-- | Mod and button 1 moves the window under the pointer, and mod and button 3 resizes it.
defaultPointerBindings :: Config l -> PointerBindings
defaultPointerBindings config = [(PointerBinding (modMask config) Button1, MoveWindow), (PointerBinding (modMask config) Button3, ResizeWindow)]

{- | Float dialogs, the windows with a parent, as 'def' does.

> manageHook = composeAll [myHook, defaultManageHook]
-}
defaultManageHook :: ManageHook l
defaultManageHook = isDialog --> Core.doFloat
