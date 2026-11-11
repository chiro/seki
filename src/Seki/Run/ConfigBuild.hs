{-# LANGUAGE LambdaCase #-}

module Seki.Run.ConfigBuild (
  Resources (..),
  environmentResources,
  resourceVariables,
  renderResources,
  parseResources,
  resource,
  xdg,
  stateDirectory,
  privateDirectory,
  generate,
  build,
  buildWithLog,
  ValidatedTarget,
  validatedPath,
  validatedLog,
  validatedProfile,
  validatedResources,
  atomicLink,
  installedContext,
  BuildContext (..),
  Resolution (..),
  resolve,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, catch, finally, onException, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Char (isDigit)
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TextIO
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Handle (hDuplicate)
import System.Directory hiding (isSymbolicLink)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Posix.Files
import System.Posix.IO (OpenMode (WriteOnly), closeFd, creat, defaultFileFlags, exclusive, fdToHandle, openFd)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process

data Resources = Resources {archive :: FilePath, compiler :: FilePath, cabal :: FilePath, environment :: FilePath, templates :: FilePath} deriving (Eq, Show)
data ValidatedTarget = ValidatedTarget {validatedPath :: FilePath, validatedLog :: FilePath, validatedProfile :: FilePath, validatedResources :: Resources} deriving (Eq, Show)
data BuildContext = BuildContext {sessionResources :: Resources, startedThrough :: Maybe FilePath, configDirectory :: FilePath, buildStateDirectory :: FilePath}

installedContext :: IO (Maybe BuildContext)
installedContext =
  lookupEnv "SEKI_CONFIG_ARCHIVE" >>= \case
    Nothing -> pure Nothing
    Just _ -> do
      resources <- environmentResources
      launcher <- lookupEnv "SEKI_CONFIG_LAUNCHER" >>= traverse (either (ioError . userError) pure . resourcePath "LAUNCHER" . Just)
      config <- xdg "XDG_CONFIG_HOME" ".config"
      state <- stateDirectory
      pure (Just (BuildContext resources launcher (config </> "seki") state))

data Resolution = Resolution {resolvedResources :: Resources, resolvedWarning :: Maybe String}

resolve :: BuildContext -> IO Resolution
resolve (BuildContext running Nothing _ _) = pure (Resolution running Nothing)
resolve (BuildContext running (Just launcher) _ state) =
  try (privateDirectory state >> installedResources state launcher) >>= \answer -> pure $ case answer of
    Right installed
      | installed == running -> Resolution installed Nothing
      | otherwise -> Resolution installed (Just ("this session does not run the installed delivery (installed archive " <> archive installed <> ", session archive " <> archive running <> "); a recompile builds against the installed delivery"))
    Left (e :: IOException) -> Resolution running (Just ("cannot read the installed delivery through " <> launcher <> " (" <> show e <> "); builds use " <> archive running <> ", which this session runs"))

installedResources :: FilePath -> FilePath -> IO Resources
installedResources state launcher = do
  (path, handle) <- openBinaryTempFile state "resources.log"
  answer <- flip finally (hClose handle) $ do
    withoutSessionResources <- filter (not . List.isPrefixOf "SEKI_CONFIG_" . fst) <$> getEnvironment
    runLogged path handle (Just (30, 65536)) (proc launcher ["--resources"]){env = Just withoutSessionResources}
    hFlush handle
    hSeek handle AbsoluteSeek 0
    B8.unpack <$> BS.hGet handle 65536
  resources <- either (\message -> ioError (userError (message <> "; " <> path))) pure (parseResources answer)
  removeFile path
  pure resources

resourcesFrom :: (String -> Maybe String) -> Either String Resources
resourcesFrom variable = Resources <$> path "ARCHIVE" <*> path "GHC" <*> path "CABAL" <*> path "ENV" <*> path "TEMPLATES"
 where
  path name = resourcePath name (variable ("SEKI_CONFIG_" <> name))

resourceVariables :: Resources -> [(String, FilePath)]
resourceVariables resources = [("SEKI_CONFIG_" <> name, path resources) | (name, path) <- [("ARCHIVE", archive), ("GHC", compiler), ("CABAL", cabal), ("ENV", environment), ("TEMPLATES", templates)]]

renderResources :: Resources -> String
renderResources = unlines . map (\(name, path) -> name <> "=" <> path) . resourceVariables

parseResources :: String -> Either String Resources
parseResources answer = resourcesFrom (`lookup` [(name, drop 1 path) | (name, path) <- map (break (== '=')) (lines answer)])

environmentResources :: IO Resources
environmentResources = getEnvironment >>= either (ioError . userError) pure . resourcesFrom . flip lookup

resource :: String -> IO FilePath
resource name = lookupEnv ("SEKI_CONFIG_" <> name) >>= either (ioError . userError) pure . resourcePath name

resourcePath :: String -> Maybe String -> Either String FilePath
resourcePath name = \case
  Just path | isAbsolute path -> Right path
  _ -> Left ("SEKI_CONFIG_" <> name <> " must name an absolute installed resource")

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
  followInstalled resources (directory </> "cabal.project")
 where
  missing name = do
    let path = directory </> name
    -- O_EXCL also preserves dangling links and concurrently created files.
    outcome <- try (openFd path WriteOnly defaultFileFlags{exclusive = True, creat = Just 0o600})
    case outcome of
      Left e | isAlreadyExistsError e -> pure ()
      Left e -> ioError e
      Right fd -> do
        bracket (fdToHandle fd `onException` closeFd fd) hClose $ \handle -> do
          content <- Text.unpack <$> TextIO.readFile (templates resources </> name)
          let rendered = replace "@ARCHIVE@" (show (archive resources)) $ replace "@COMPILER@" (compiler resources) content
          hPutStr handle rendered
        hPutStrLn stderr ("seki: created " <> path)

{- | Point an existing project's seki archive and compiler at the installed
resources. The project outlives the delivery that generated it, and an
upgraded delivery installs both at new paths. A quoted @seki-VERSION.tar.gz@
path and a @with-compiler@ path with the installed compiler's file name are
another delivery's. Every other byte stays.
-}
followInstalled :: Resources -> FilePath -> IO ()
followInstalled resources path = do
  outcome <- try (BS.readFile path)
  case outcome of
    Left e | isDoesNotExistError e -> pure ()
    Left e -> ioError e
    Right content -> do
      let updated = B8.intercalate (B8.singleton '\n') (map line (B8.split '\n' content))
      when (updated /= content) $ do
        BS.writeFile path updated
        hPutStrLn stderr ("seki: pointed " <> path <> " at the installed seki archive and compiler")
 where
  encoded = encodeUtf8 . Text.pack
  installedCompiler = encoded (compiler resources)
  line text = case B8.strip <$> B8.stripPrefix (B8.pack "with-compiler:") text of
    Just value | value /= installedCompiler && takeFileName (B8.unpack value) == takeFileName (compiler resources) -> B8.pack "with-compiler: " <> installedCompiler
    _ -> quoted text
  -- Odd fields of a split on quotes are quoted strings.
  quoted text = B8.intercalate (B8.singleton '"') (zipWith field [0 :: Int ..] (B8.split '"' text))
  field index value
    | odd index && isArchive (takeFileName (B8.unpack value)) = encoded (archive resources)
    | otherwise = value
  isArchive name = case stripSuffix ".tar.gz" =<< List.stripPrefix "seki-" name of
    Just version -> not (null version) && all (\c -> isDigit c || c == '.') version
    Nothing -> False
  stripSuffix suffix = fmap reverse . List.stripPrefix (reverse suffix) . reverse

replace :: String -> String -> String -> String
replace needle value = go
 where
  go [] = []
  go text | take (length needle) text == needle = value <> go (drop (length needle) text)
  go (c : rest) = c : go rest

build :: Resources -> FilePath -> FilePath -> IO ValidatedTarget
build resources config state = buildWithLog (Resolution resources Nothing) config state (const (pure ()))

buildWithLog :: Resolution -> FilePath -> FilePath -> (FilePath -> IO ()) -> IO ValidatedTarget
buildWithLog (Resolution resources warning) config state reportLog = bracket (openBinaryTempFile state "build.log") (hClose . snd) $ \(logPath, logHandle) -> do
  setFileMode logPath 0o600
  reportLog logPath
  hPutStrLn stderr ("seki: build log: " <> logPath)
  mapM_ (BS.hPut logHandle . encodeUtf8 . Text.pack . (<> "\n") . ("seki: " <>)) (("building against " <> archive resources) : maybeToList warning)
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
      pure (ValidatedTarget candidate logPath (state </> "profile") resources)
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
        errCopy <-
          ( case errors of
              UseHandle errHandle -> hDuplicate errHandle
              _ -> hDuplicate handle
            )
            `onException` hClose outCopy
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

atomicLink :: FilePath -> FilePath -> IO ()
atomicLink target profile = do
  (temporary, handle) <- openTempFile (takeDirectory profile) "profile-link"
  hClose handle
  removeFile temporary
  createSymbolicLink target temporary
  renameFile temporary profile `onException` removeFile temporary
