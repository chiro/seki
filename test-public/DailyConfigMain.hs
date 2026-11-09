{-# LANGUAGE LambdaCase #-}

module Main (main) where

import PortedConfig (dailyConfig)
import Seki (seki)
import System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  configHome <-
    lookupEnv "XDG_CONFIG_HOME" >>= \case
      Just directory | not (null directory) -> pure directory
      _ -> (<> "/.config") <$> getEnv "HOME"
  either fail seki (dailyConfig (configHome <> "/seki/keymap.xkb"))
