module ConfigMain (configuredMain, customConfig, fullConfig, hookedConfig) where

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
    }

fullConfig :: Config Full
fullConfig = def{layoutHook = Full, keys = defaultKeys, manageHook = idHook}

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

configuredMain :: IO ()
configuredMain = seki customConfig
