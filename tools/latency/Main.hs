{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt))
import Data.Version (showVersion)
import Seki.Integration.Harness (mainWith)
import Seki.Latency.Live (runLive, runWM)
import Seki.Latency.Pure (runPure)
import Seki.Latency.Workload (parseWorkloadName)
import System.Environment (getArgs)
import System.Info (compilerName, compilerVersion)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import Text.Read (readMaybe)

main :: IO ()
main = do
  thread <- myThreadId
  _ <- installHandler sigTERM (Catch (throwTo thread UserInterrupt)) Nothing
  args <- getArgs
  case args of
    ["info"] -> putStrLn (compilerName <> " " <> showVersion compilerVersion)
    ["--river-init", _] -> mainWith []
    ["wm", socket, directory, workload] -> runWM (named workload) socket directory
    ["pure", path, seed, windows, steps, workload] -> runPure path (named workload) (number seed) (number windows) (number steps)
    ["live", directory, seed, steps, collector, workload] -> runLive directory (named workload) (number seed) (number steps) collector
    _ -> fail "usage: seki-latency pure FILE SEED WINDOWS STEPS m1|m2|m3 | live DIRECTORY SEED STEPS normal|nonmoving|profile m1|m2|m3 | wm SOCKET DIRECTORY m1|m2|m3"
 where
  number value = maybe (error ("invalid number: " <> value)) id (readMaybe value)
  named value = maybe (error ("invalid workload: " <> value)) id (parseWorkloadName value)
