module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Seki.Fixture.LayerSurface (
  newFixtureControl,
  parseFixtureArguments,
  readyLine,
  requestStop,
  runLayerSurfaceFixture,
  stopOnEndOfInput,
 )
import Seki.Wire.Transport (discoverConnectionTarget)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdin, stdout)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)

main :: IO ()
main = do
  config <- getArgs >>= either (failWith 2) pure . parseFixtureArguments
  target <- discoverConnectionTarget >>= either (failWith 1 . ("Wayland discovery failed: " <>) . show) pure
  control <- newFixtureControl
  void (installHandler sigTERM (Catch (requestStop control)) Nothing)
  void (forkIO (stopOnEndOfInput stdin control))
  result <- runLayerSurfaceFixture control config target $ \ready -> do
    putStrLn (readyLine ready)
    hFlush stdout
  either (failWith 1 . ("layer-surface fixture failed: " <>) . show) (const (pure ())) result

failWith :: Int -> String -> IO value
failWith code message = do
  hPutStrLn stderr message
  exitWith (ExitFailure code)
