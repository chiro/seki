module ConfigMain (configuredMain, customConfig, fullConfig) where

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
    }

fullConfig :: Config Full
fullConfig = def{layoutHook = Full, keys = defaultKeys}

configuredMain :: IO ()
configuredMain = seki customConfig
