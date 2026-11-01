module Main (main) where

import Seki.Integration.Harness (mainWith)
import Seki.Integration.Input (inputScenarios, inputWorkerMain)
import Seki.Integration.M1 (m1Scenarios)
import Seki.Integration.M2 (m2Scenarios)
import Seki.Integration.M3 (ipcScenarios, m3Scenarios)
import Seki.Integration.Reference (referenceScenarios)
import Seki.Integration.Restart (restartScenarios)
import Seki.Integration.Scratchpad (scratchpadScenarios)
import Seki.Integration.ScratchpadRestore (scratchpadRestoreScenarios)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--input-worker", path] -> inputWorkerMain path
    _ -> mainWith (m1Scenarios <> m2Scenarios <> restartScenarios <> referenceScenarios <> m3Scenarios <> ipcScenarios <> inputScenarios <> scratchpadScenarios <> scratchpadRestoreScenarios)
