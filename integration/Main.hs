module Main (main) where

import Seki.Integration.Daily (dailyScenarios)
import Seki.Integration.Harness (mainWith)
import Seki.Integration.Input (inputScenarios, inputWorkerMain)
import Seki.Integration.M1 (m1Scenarios)
import Seki.Integration.M2 (m2Scenarios)
import Seki.Integration.M3 (ipcScenarios, m3Scenarios, recompileScenarios)
import Seki.Integration.M4 (crashingImage, m4Scenarios)
import Seki.Integration.Reference (referenceScenarios)
import Seki.Integration.Restart (restartScenarios)
import Seki.Integration.Scratchpad (scratchpadScenarios)
import Seki.Integration.ScratchpadRestore (scratchpadRestoreScenarios)
import Seki.Run.Config (parseSekiArgs)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--input-worker", path] -> inputWorkerMain path
    -- A seki command line: the launcher or a session started this executable as a built configuration.
    _ : _ | Right _ <- parseSekiArgs args -> crashingImage args
    _ -> mainWith (m1Scenarios <> m2Scenarios <> restartScenarios <> referenceScenarios <> m3Scenarios <> recompileScenarios <> ipcScenarios <> inputScenarios <> scratchpadScenarios <> scratchpadRestoreScenarios <> dailyScenarios <> m4Scenarios)
