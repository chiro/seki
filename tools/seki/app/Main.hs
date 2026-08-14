{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Seki.Run.Default (defaultConfig, runDefault)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  runDefault defaultConfig >>= \case
    Left errorValue -> do
      hPutStrLn stderr ("seki: " <> show errorValue)
      exitFailure
    Right () -> pure ()
