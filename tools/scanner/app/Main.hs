module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Either (partitionEithers)
import Seki.Scanner.Parse (ScannerError, parseProtocolFile, validateProtocols)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  paths <- getArgs
  when (null paths) $ do
    hPutStrLn stderr "usage: seki-scanner PROTOCOL.xml..."
    exitFailure
  results <- mapM parseProtocolFile paths
  let (parseErrors, protocols) = partitionEithers results
  unless (null parseErrors) $ do
    forM_ parseErrors (hPutStrLn stderr . formatError)
    exitFailure
  case validateProtocols protocols of
    Left errors -> do
      forM_ errors (hPutStrLn stderr . formatError)
      exitFailure
    Right validated -> putStrLn ("validated " <> show (length validated) <> " protocol file(s)")

formatError :: ScannerError -> String
formatError = show
