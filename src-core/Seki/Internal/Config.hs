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
  def,
  defaultKeys,
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
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))

type Action l a = Core.Action String l a
type Bindings l = [(KeyBinding, Action l ())]
type ManageHook l = Core.ManageHook String l
type StdLayouts = Choose Tall (Choose (Mirror Tall) Full)

-- | XKB modifier bits. Combine modifiers with @(.|.)@.
newtype Mods = Mods Word32
  deriving stock (Eq, Show)
  deriving newtype (Bits)

pattern Shift, Control, Mod1, Mod3, Mod4, Mod5 :: Mods
pattern Shift = Mods 1
pattern Control = Mods 4
pattern Mod1 = Mods 8
pattern Mod3 = Mods 32
pattern Mod4 = Mods 64
pattern Mod5 = Mods 128

-- | An XKB keysym number, such as @Keysym 0xff0d@ for Return.
newtype Keysym = Keysym Word32 deriving (Eq, Show)

data KeyBinding = KeyBinding
  { bindingModifiers :: Mods
  , bindingKeysym :: Keysym
  }
  deriving (Eq, Show)

data BorderConfig = BorderConfig
  { borderWidth :: Int32
  , focusedBorderColor :: BorderColor
  , unfocusedBorderColor :: BorderColor
  }
  deriving (Eq, Show)

data Config l = Config
  { modMask :: Mods
  , terminal :: String
  , workspaces :: [String]
  , layoutHook :: l
  , keys :: Config l -> Bindings l
  , borders :: BorderConfig
  , bindingsUseFixedLayout :: Bool
  , manageHook :: ManageHook l
  -- ^ Runs on every new window, after it is inserted above the focus.
  , startupHook :: Action l ()
  -- ^ Runs once per seki process, when the first output arrives. Use
  -- 'Core.spawnOnce' for launches that must not repeat after a restart. Its
  -- type names the layout, so an update that changes the layout type sets it
  -- again, as it does 'keys' and 'manageHook'.
  }

-- | Nine workspaces, Tall, Mirror Tall, Full, xmonad-style bindings, floating dialogs, and no startup hook.
def :: Config StdLayouts
def =
  Config
    { modMask = Mod4
    , terminal = "foot"
    , workspaces = map show [1 .. 9 :: Int]
    , layoutHook = tall ||| Mirror tall ||| Full
    , keys = defaultKeys
    , borders = BorderConfig 2 (BorderColor 0 0x80000000 0xffffffff 0xffffffff) (BorderColor 0x40000000 0x40000000 0x40000000 0xffffffff)
    , bindingsUseFixedLayout = True
    , manageHook = defaultManageHook
    , startupHook = pure ()
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
  ]
    ++ concat
      [ [key (Mods 0) symbol (Core.windows (StackSet.greedyView tag)), key Shift symbol (Core.windows (StackSet.shift tag))]
      | (tag, symbol) <- zip (workspaces config) [0x31 .. 0x39]
      ]
 where
  key modifiers symbol action = (KeyBinding (modMask config .|. modifiers) (Keysym symbol), action)

{- | Float dialogs, the windows with a parent, as 'def' does.

> manageHook = composeAll [myHook, defaultManageHook]
-}
defaultManageHook :: ManageHook l
defaultManageHook = isDialog --> Core.doFloat
