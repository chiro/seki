module Main (main) where

import Seki.Integration.Harness (mainWith)
import Seki.Integration.M1 (m1Scenarios)
import Seki.Integration.M2 (m2Scenarios)
import Seki.Integration.M3 (ipcScenarios, m3Scenarios)
import Seki.Integration.Reference (referenceScenarios)
import Seki.Integration.Restart (restartScenarios)

main :: IO ()
main = mainWith (m1Scenarios <> m2Scenarios <> restartScenarios <> referenceScenarios <> m3Scenarios <> ipcScenarios)
