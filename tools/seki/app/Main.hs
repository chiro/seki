{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, catch, onException, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Handle (hDuplicate)
import System.Directory hiding (isSymbolicLink)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Posix.Files
import System.Posix.IO (OpenMode (WriteOnly), creat, defaultFileFlags, exclusive, fdToHandle, openFd)
import System.Posix.Process (executeFile)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process

-- Launcher arguments never reach a user configuration except for Restore.
data Options = Start | Check | Restore FilePath

data Resources = Resources {archive :: FilePath, compiler :: FilePath, cabal :: FilePath, environment :: FilePath, templates :: FilePath}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> launch Start
    ["--check"] -> launch Check
    ["--restore", file] | take 2 file /= "--" -> launch (Restore file)
    _ -> hPutStrLn stderr "usage: seki [--check | --restore FILE]" >> exitFailure

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
        Check -> hPutStrLn stderr ("seki: candidate validated: " <> candidate)
        Start -> activate state candidate
  case result of
    Right () -> pure ()
    Left (err :: IOException) -> do
      hPutStrLn stderr ("seki: " <> show err)
      case option of
        Start -> do
          hPutStrLn stderr "seki: attempting shipped default WM"
          fallback <- try (resource "DEFAULT" >>= \path -> executeFile path False [] Nothing)
          case fallback of
            Left (e :: IOException) -> hPutStrLn stderr ("seki: default exec failed: " <> show e)
            Right () -> pure ()
          exitFailure
        _ -> exitFailure

resource :: String -> IO FilePath
resource name = do
  value <- lookupEnv ("SEKI_CONFIG_" <> name)
  case value of
    Just path | isAbsolute path -> pure path
    _ -> ioError (userError ("SEKI_CONFIG_" <> name <> " must name an absolute installed resource"))

xdg :: String -> FilePath -> IO FilePath
xdg name suffix = do
  value <- lookupEnv name
  case value of
    Just path | isAbsolute path -> pure path
    Just "" -> home
    Nothing -> home
    _ -> ioError (userError (name <> " must be absolute"))
 where
  home = getHomeDirectory >>= \path -> pure (path </> suffix)

stateDirectory :: IO FilePath
stateDirectory = (</> "seki") <$> xdg "XDG_STATE_HOME" ".local/state"

privateDirectory :: FilePath -> IO ()
privateDirectory path = do
  createDirectoryIfMissing True path
  status <- getSymbolicLinkStatus path
  unless (isDirectory status && not (isSymbolicLink status)) (ioError (userError ("unsafe state directory: " <> path)))
  setFileMode path 0o700

generate :: Resources -> FilePath -> IO ()
generate resources directory = do
  createDirectoryIfMissing True directory
  mapM_ missing ["Main.hs", "seki-user.cabal", "cabal.project"]
 where
  missing name = do
    content <- readFile (templates resources </> name)
    let rendered = replace "@ARCHIVE@" (show (archive resources)) $ replace "@COMPILER@" (compiler resources) content
        path = directory </> name
    -- O_EXCL also preserves dangling links and concurrently created files.
    outcome <- try (openFd path WriteOnly defaultFileFlags{exclusive = True, creat = Just 0o600})
    case outcome of
      Left e | isAlreadyExistsError e -> pure ()
      Left e -> ioError e
      Right fd -> do
        bracket (fdToHandle fd) hClose (\handle -> hPutStr handle rendered)
        hPutStrLn stderr ("seki: created " <> path)

replace :: String -> String -> String -> String
replace needle value = go
 where
  go [] = []
  go text | take (length needle) text == needle = value <> go (drop (length needle) text)
  go (c : rest) = c : go rest

build :: Resources -> FilePath -> FilePath -> IO FilePath
build resources config state = bracket (openBinaryTempFile state "build.log") (hClose . snd) $ \(logPath, logHandle) -> do
  setFileMode logPath 0o600
  hPutStrLn stderr ("seki: build log: " <> logPath)
  cabalConfig <-
    lookupEnv "SEKI_CONFIG_CABAL_CONFIG" >>= \case
      Just path | isAbsolute path -> pure path
      Just _ -> ioError (userError "SEKI_CONFIG_CABAL_CONFIG must be absolute")
      Nothing -> do
        let path = state </> "cabal.config"
        present <- doesPathExist path
        unless present $ writeFile path ("active-repositories: :none\nremote-repo-cache: " <> (state </> "cache") <> "\nstore-dir: " <> (state </> "store") <> "\n")
        pure path
  let command args = (proc (environment resources) (cabal resources : ("--config-file=" <> cabalConfig) : args)){cwd = Just config}
      flags = ["exe:seki-user", "--offline", "--enable-optimization=2", "-j1"]
  ( do
      runLogged logPath logHandle Nothing (command ("build" : flags))
      (binLog, binHandle) <- openBinaryTempFile state "list-bin.log"
      resolved <- bracket (pure binHandle) hClose $ \handle -> do
        runLogged binLog handle (Just (30, 65536)) ((command ("list-bin" : flags)){std_err = UseHandle logHandle})
        hFlush handle
        hSeek handle AbsoluteSeek 0
        B8.unpack . B8.strip <$> BS.hGet handle 65536
      unless (isAbsolute resolved && '\n' `notElem` resolved) (ioError (userError ("invalid Cabal list-bin result: " <> binLog)))
      (candidate, handle) <- openBinaryTempFile state "candidate"
      hClose handle
      copyFile resolved candidate
      setFileMode candidate 0o500
      runLogged logPath logHandle (Just (10, 1048576)) (proc candidate ["--check"])
      hFlush logHandle
      pure candidate
    )
    `catch` \(e :: IOException) -> do
      hFlush logHandle
      hPutStrLn stderr (show e)
      size <- hFileSize logHandle
      hSeek logHandle AbsoluteSeek (max 0 (size - 8192))
      BS.hGet logHandle 8192 >>= BS.hPut stderr
      ioError (userError (show e <> "; full log: " <> logPath))

runLogged :: FilePath -> Handle -> Maybe (Integer, Integer) -> CreateProcess -> IO ()
runLogged path handle bound command = do
  hFlush handle
  start <- getMonotonicTimeNSec
  initialSize <- hFileSize handle
  let errors = case std_err command of
        Inherit -> UseHandle handle
        stream -> stream
      output = command{std_in = NoStream, std_out = UseHandle handle, std_err = errors, create_group = True}
  let acquire = do
        outCopy <- hDuplicate handle
        errCopy <- case errors of
          UseHandle errHandle -> hDuplicate errHandle
          _ -> hDuplicate handle
        created@(_, _, _, process) <-
          createProcess output{std_out = UseHandle outCopy, std_err = UseHandle errCopy}
            `onException` (hClose outCopy >> hClose errCopy)
        hClose outCopy
        hClose errCopy
        pid <- getPid process
        pure (created, pid)
  bracket acquire cleanup $ \((_, _, _, process), _) -> do
    let poll = do
          code <- getProcessExitCode process
          size <- hFileSize handle
          now <- getMonotonicTimeNSec
          when (maybe False (\(seconds, bytes) -> toInteger (now - start) > seconds * 1000000000 || size - initialSize > bytes) bound) $
            ioError (userError ("subprocess time/output limit exceeded: " <> path))
          case code of
            Nothing -> threadDelay 10000 >> poll
            Just ExitSuccess -> pure ()
            Just failure -> ioError (userError ("subprocess failed: " <> show failure <> "; " <> path))
    poll
 where
  cleanup ((_, _, _, process), pid) = do
    mapM_ (\p -> signalProcessGroup sigKILL p `catch` \(_ :: IOException) -> pure ()) pid
    let reap :: Int -> IO ()
        reap remaining =
          getProcessExitCode process >>= \code -> case code of
            Nothing | remaining > 0 -> threadDelay 10000 >> reap (remaining - 1)
            Nothing -> ioError (userError "subprocess did not exit after SIGKILL")
            Just _ -> pure ()
    reap 100

activate :: FilePath -> FilePath -> IO ()
activate state candidate = do
  let profile = state </> "profile"
  previous <- try (readSymbolicLink profile)
  atomicLink candidate profile
  executeFile profile False [] Nothing `onException` case previous of
    Right old -> atomicLink old profile
    Left (e :: IOException) | isDoesNotExistError e -> removeFile profile
    Left e -> ioError e

atomicLink :: FilePath -> FilePath -> IO ()
atomicLink target profile = do
  (temporary, handle) <- openTempFile (takeDirectory profile) "profile-link"
  hClose handle
  removeFile temporary
  createSymbolicLink target temporary
  renameFile temporary profile `onException` removeFile temporary
