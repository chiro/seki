module Main (main) where

import Seki.Integration.Harness (mainWith)
import Seki.Integration.M1 (m1Scenarios)
import Seki.Integration.M2 (m2Scenarios)

main :: IO ()
main = mainWith (m1Scenarios <> m2Scenarios)
