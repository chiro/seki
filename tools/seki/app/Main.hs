{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (IOException, onException, try)
import Control.Monad (void)
import Seki.Run.ConfigBuild
import Seki.Run.IPCClient (runClient)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Process (executeFile)

-- Launcher arguments never reach a user configuration except for Restore.
data Options = Start | Check | Restore FilePath

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> launch Start
    ["--check"] -> launch Check
    ["--recompile"] -> runClient ["recompile"]
    ["--restore", file] | take 2 file /= "--" -> launch (Restore file)
    _ -> diagnostic "usage: seki [--check | --recompile | --restore FILE]" >> exitFailure

launch :: Options -> IO ()
launch option = do
  result <- try $ case option of
    Restore file -> do
      state <- stateDirectory
      executeFile (state </> "profile") False ["--restore", file] Nothing
    _ -> do
      resources <- Resources <$> resource "ARCHIVE" <*> resource "GHC" <*> resource "CABAL" <*> resource "ENV" <*> resource "TEMPLATES"
      config <- xdg "XDG_CONFIG_HOME" ".config"
      state <- stateDirectory
      privateDirectory state
      generate resources (config </> "seki")
      candidate <- build resources (config </> "seki") state
      case option of
        Check -> diagnostic ("seki: candidate validated: " <> validatedPath candidate)
        Start -> activate state (validatedPath candidate)
  case result of
    Right () -> pure ()
    Left (err :: IOException) -> do
      diagnostic ("seki: " <> show err)
      case option of
        Start -> do
          diagnostic "seki: attempting shipped default WM"
          fallback <- try (resource "DEFAULT" >>= \path -> executeFile path False [] Nothing)
          case fallback of
            Left (e :: IOException) -> diagnostic ("seki: default exec failed: " <> show e)
            Right () -> pure ()
          exitFailure
        _ -> exitFailure

activate :: FilePath -> FilePath -> IO ()
activate state candidate = do
  let profile = state </> "profile"
  previous <- try (readSymbolicLink profile)
  atomicLink candidate profile
  executeFile profile False [] Nothing `onException` case previous of
    Right old -> atomicLink old profile
    Left (e :: IOException) | isDoesNotExistError e -> removeFile profile
    Left e -> ioError e

diagnostic :: String -> IO ()
diagnostic message = void (try @IOException (hPutStrLn stderr message))
