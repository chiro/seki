module ConfigMain (configuredMain, customConfig, fullConfig, hookedConfig, mediaConfig, raiseVolume, tolerantConfig) where

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
    , keys = \config -> (KeyBinding (modMask config .|. Shift) (Keysym 0xffbe), windows focusDown >> kill >> spawn "true") : (KeyBinding (modMask config) (Keysym 0x66), withFocused float) : (KeyBinding (modMask config .|. Shift) (Keysym 0x74), withFocused sink) : (KeyBinding (modMask config .|. Shift) (Keysym 0x66), withFocused toggleFullscreen) : defaultKeys config
    , manageHook = idHook
    , startupHook = spawnOnce "true"
    }

fullConfig :: Config Full
fullConfig = def{layoutHook = Full, keys = defaultKeys, manageHook = composeAll [appId =? "mpv" --> doShift "2", defaultManageHook], startupHook = pure ()}

hookedConfig :: Config StdLayouts
hookedConfig =
  def
    { manageHook =
        composeAll
          [ appId =? "mpv" --> doShift "2"
          , title =? "scratch" --> doShift "9"
          , title =? "picture-in-picture" --> doFloat
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

tolerantConfig :: Config (ModifiedLayout Tolerant StdLayouts)
tolerantConfig = def{layoutHook = tolerant (layoutHook def), keys = defaultKeys, manageHook = defaultManageHook, startupHook = pure ()}

configuredMain :: IO ()
configuredMain = seki customConfig
