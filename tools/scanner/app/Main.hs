module Main (main) where

import Control.Monad (forM_, unless)
import Data.Either (partitionEithers)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Seki.Scanner.Generate (GenerationConfig (..), generateProtocolModule)
import Seki.Scanner.IR (Protocol)
import Seki.Scanner.Parse (ScannerError, parseProtocolFile, validateProtocols)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hPrint, hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    "validate" : paths | not (null paths) -> validateCommand paths
    "generate" : moduleName : sourceLabel : sourcePin : outputPath : primaryPath : referencePaths ->
      generateCommand moduleName sourceLabel sourcePin outputPath primaryPath referencePaths
    _ -> usage

validateCommand :: [FilePath] -> IO ()
validateCommand paths = do
  protocols <- parseProtocols paths
  case validateProtocols protocols of
    Left errors -> do
      forM_ errors (hPutStrLn stderr . formatError)
      exitFailure
    Right validated -> putStrLn ("validated " <> show (length validated) <> " protocol file(s)")

generateCommand :: String -> String -> String -> FilePath -> FilePath -> [FilePath] -> IO ()
generateCommand moduleName sourceLabel sourcePin outputPath primaryPath referencePaths = do
  protocols <- parseProtocols (primaryPath : referencePaths)
  target <- case protocols of
    [] -> fail "generate command requires a primary protocol"
    protocol : _ -> pure protocol
  let config =
        GenerationConfig
          { generationModuleName = Text.pack moduleName
          , generationSourcePath = Text.pack sourceLabel
          , generationSourcePin = Text.pack sourcePin
          }
  generated <- case generateProtocolModule config protocols target of
    Left errorValue -> do
      hPrint stderr errorValue
      exitFailure
    Right source -> pure source
  createDirectoryIfMissing True (takeDirectory outputPath)
  TextIO.writeFile outputPath generated

parseProtocols :: [FilePath] -> IO [Protocol]
parseProtocols paths = do
  results <- mapM parseProtocolFile paths
  let (parseErrors, protocols) = partitionEithers results
  unless (null parseErrors) $ do
    forM_ parseErrors (hPutStrLn stderr . formatError)
    exitFailure
  pure protocols

usage :: IO value
usage = do
  hPutStrLn stderr "usage: seki-scanner validate PROTOCOL.xml..."
  hPutStrLn stderr "       seki-scanner generate MODULE SOURCE PIN OUTPUT PRIMARY.xml [REFERENCE.xml...]"
  exitFailure

formatError :: ScannerError -> String
formatError = show
