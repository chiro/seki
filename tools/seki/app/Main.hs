{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Seki.Run.ConfigBuild
import Seki.Run.IPCClient (runClient)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)

data Options = Start | Check | Restore FilePath

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> launch Start
    ["--check"] -> launch Check
    ["--recompile"] -> runClient ["recompile"]
    ["--resources"] -> try environmentResources >>= either (\(err :: IOException) -> diagnostic ("seki: " <> show err) >> exitFailure) (putStr . renderResources)
    ["--restore", file] | take 2 file /= "--" -> launch (Restore file)
    _ -> diagnostic "usage: seki [--check | --recompile | --resources | --restore FILE]" >> exitFailure

launch :: Options -> IO ()
launch option = do
  result <- try $ case option of
    Restore file -> do
      state <- stateDirectory
      executeFile (state </> "profile") False ["--restore", file] Nothing
    Check -> do
      resources <- environmentResources
      config <- xdg "XDG_CONFIG_HOME" ".config"
      state <- stateDirectory
      privateDirectory state
      candidate <- build resources (config </> "seki") state
      diagnostic ("seki: candidate validated: " <> validatedPath candidate)
    Start -> do
      installed <- environmentResources
      state <- stateDirectory
      privateDirectory state
      startImage installed state >>= \case
        ProfileImage profile candidate -> markUnproven state (Unproven candidate FirstTrial) >> executeFile profile False startupArguments Nothing
        DefaultImage gap -> do
          diagnostic ("seki: starting the shipped default WM while the configuration builds (" <> describeGap gap <> ")")
          defaultRuntime
  case result of
    Right () -> pure ()
    Left (err :: IOException) -> do
      diagnostic ("seki: " <> show err)
      case option of
        Start -> do
          diagnostic "seki: attempting shipped default WM"
          fallback <- try defaultRuntime
          case fallback of
            Left (e :: IOException) -> diagnostic ("seki: default exec failed: " <> show e)
            Right () -> pure ()
          exitFailure
        _ -> exitFailure
 where
  defaultRuntime = resource "DEFAULT" >>= \path -> executeFile path False startupArguments Nothing

diagnostic :: String -> IO ()
diagnostic message = void (try @IOException (hPutStrLn stderr message))
