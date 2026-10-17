module Main (main) where

import Seki.Integration.Harness (mainWith)
import Seki.Integration.M1 (m1Scenarios)

main :: IO ()
main = mainWith m1Scenarios
