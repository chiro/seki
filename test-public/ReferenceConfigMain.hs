module Main (main) where

import PortedConfig (portedConfig)
import Seki (seki)

main :: IO ()
main = seki portedConfig
