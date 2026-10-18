module ConfigMain (configuredMain, customConfig, fullConfig, hookedConfig, mediaConfig, raiseVolume) where

import Seki

newtype CustomLayout = CustomLayout Full
  deriving (Show)

instance LayoutClass CustomLayout where
  pureLayout (CustomLayout layout) = pureLayout layout
  description _ = "Custom"

customConfig :: Config CustomLayout
customConfig =
  def
    { layoutHook = CustomLayout Full
    , keys = \config -> (KeyBinding (modMask config .|. Shift) (Keysym 0xffbe), windows focusDown >> kill >> spawn "true") : defaultKeys config
    , manageHook = idHook
    , startupHook = spawnOnce "true"
    }

fullConfig :: Config Full
fullConfig = def{layoutHook = Full, keys = defaultKeys, manageHook = idHook, startupHook = pure ()}

hookedConfig :: Config StdLayouts
hookedConfig =
  def
    { manageHook =
        composeAll
          [ appId =? "mpv" --> doShift "2"
          , title =? "scratch" --> doShift "9"
          , isDialog --> doSink
          ]
    }

raiseVolume :: KeyBinding
raiseVolume = KeyBinding (Mods 0) (Keysym 0x1008FF13)

mediaConfig :: Config StdLayouts
mediaConfig =
  additionalKeys
    def{startupHook = spawnOnce "dunst"}
    [(raiseVolume, spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")]

configuredMain :: IO ()
configuredMain = seki customConfig
