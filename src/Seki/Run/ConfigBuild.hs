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
  installedResources,
  StartImage (..),
  ProfileGap (..),
  describeGap,
  startImage,
  startupArguments,
  Unproven (..),
  Trial (..),
  markUnproven,
  readUnproven,
  clearUnproven,
  withTrial,
  sameBytes,
  discardCandidate,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, catch, finally, onException, try)
import Control.Monad (unless, when)
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Char (isDigit)
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TextIO
import Foreign.C.Error (eINTR, eWOULDBLOCK, getErrno, throwErrno)
import Foreign.C.Types (CInt (..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.Handle (hDuplicate)
import System.Directory hiding (isSymbolicLink)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Posix.Files
import System.Posix.IO (OpenMode (WriteOnly), cloexec, closeFd, creat, defaultFileFlags, exclusive, fdToHandle, openFd)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process

data Resources = Resources {archive :: FilePath, compiler :: FilePath, cabal :: FilePath, environment :: FilePath, templates :: FilePath} deriving (Eq, Show)
data ValidatedTarget = ValidatedTarget {validatedPath :: FilePath, validatedLog :: FilePath, validatedProfile :: FilePath, validatedResources :: Resources} deriving (Eq, Show)
data BuildContext = BuildContext {sessionResources :: Resources, startedThrough :: Maybe (Either String FilePath), configDirectory :: FilePath, buildStateDirectory :: FilePath}

installedContext :: IO (Maybe BuildContext)
installedContext =
  lookupEnv "SEKI_CONFIG_ARCHIVE" >>= \case
    Nothing -> pure Nothing
    Just _ -> do
      resources <- environmentResources
      launcher <- fmap (resourcePath "LAUNCHER" . Just) <$> lookupEnv "SEKI_CONFIG_LAUNCHER"
      config <- xdg "XDG_CONFIG_HOME" ".config"
      state <- stateDirectory
      pure (Just (BuildContext resources launcher (config </> "seki") state))

data Resolution = Resolution {resolvedResources :: Resources, resolvedWarning :: Maybe String}

resolve :: BuildContext -> IO Resolution
resolve (BuildContext running Nothing _ _) = pure (Resolution running Nothing)
resolve (BuildContext running (Just (Left refusal)) _ _) = pure (Resolution running (Just (refusal <> "; builds use " <> archive running <> ", which this session runs")))
resolve (BuildContext running (Just (Right launcher)) _ state) =
  try (privateDirectory state >> installedResources 30 state launcher) >>= \answer -> pure $ case answer of
    Right installed
      | installed == running -> Resolution installed Nothing
      | otherwise -> Resolution installed (Just ("this session does not run the installed delivery (installed archive " <> archive installed <> ", session archive " <> archive running <> "); a recompile builds against the installed delivery"))
    Left (e :: IOException) -> Resolution running (Just ("cannot read the installed delivery through " <> launcher <> " (" <> show e <> "); builds use " <> archive running <> ", which this session runs"))

installedResources :: Integer -> FilePath -> FilePath -> IO Resources
installedResources seconds state launcher = do
  (path, handle) <- openBinaryTempFile state "resources.log"
  answer <- flip finally (hClose handle) $ do
    withoutSessionResources <- filter (not . List.isPrefixOf "SEKI_CONFIG_" . fst) <$> getEnvironment
    runLogged path handle (Just (seconds, 65536)) (proc launcher ["--resources"]){env = Just withoutSessionResources}
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

{- | Create missing project files, point the project at these resources,
build it, and validate a private copy of the result. The copy gets a record
of the resources, which 'startImage' reads on a later start.
-}
build :: Resources -> FilePath -> FilePath -> IO ValidatedTarget
build resources config state = buildWithLog (Resolution resources Nothing) config state (const (pure ()))

buildWithLog :: Resolution -> FilePath -> FilePath -> (FilePath -> IO ()) -> IO ValidatedTarget
buildWithLog resolution config state reportLog =
  withBuildLock state (hPutStrLn stderr "seki: waiting for another seki build of this configuration") $
    generate (resolvedResources resolution) config >> buildLocked resolution config state reportLog

buildLocked :: Resolution -> FilePath -> FilePath -> (FilePath -> IO ()) -> IO ValidatedTarget
buildLocked (Resolution resources warning) config state reportLog = bracket (openBinaryTempFile state "build.log") (hClose . snd) $ \(logPath, logHandle) -> do
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
        BS.hGet handle 65536 >>= pathText . B8.strip
      unless (isAbsolute resolved && '\n' `notElem` resolved) (ioError (userError ("invalid Cabal list-bin result: " <> binLog)))
      (candidate, handle) <- openBinaryTempFile state "candidate"
      hClose handle
      copyFile resolved candidate
      setFileMode candidate 0o500
      runLogged logPath logHandle (Just (10, 1048576)) (proc candidate ["--check"])
      hFlush logHandle
      pathBytes (renderResources resources) >>= BS.writeFile (recordPath candidate)
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

-- | The image a fresh start execs. Building is never part of the answer.
data StartImage
  = -- | The profile link, and the candidate it names.
    ProfileImage FilePath FilePath
  | DefaultImage ProfileGap
  deriving (Eq, Show)

-- | Why a fresh start does not exec the profile.
data ProfileGap
  = NoProfile
  | -- | The candidate has no readable record of the resources it was built from.
    NoBuildRecord FilePath
  | -- | The candidate was built from this archive, not the installed one.
    OtherDelivery FilePath
  | -- | The candidate was started and has not proved healthy.
    HeldBack Unproven
  deriving (Eq, Show)

describeGap :: ProfileGap -> String
describeGap = \case
  NoProfile -> "no profile yet"
  NoBuildRecord candidate -> candidate <> " has no record of the delivery it was built from"
  OtherDelivery built -> "the profile was built from " <> built <> ", not from the installed delivery"
  HeldBack unproven -> unprovenCandidate unproven <> " died before it proved healthy"

-- | What the launcher passes to the image it starts. The image then owes one build.
startupArguments :: [String]
startupArguments = ["--startup-build"]

{- | Choose what a fresh start execs. It reads the profile link, the record
next to its candidate, and the unproven marker. It never builds or writes.
-}
startImage :: Resources -> FilePath -> IO StartImage
startImage installed state = do
  let profile = state </> "profile"
  link <- try @IOException (readSymbolicLink profile)
  case link of
    Left _ -> pure (DefaultImage NoProfile)
    Right target -> do
      let candidate = state </> target
      record <- try @IOException (BS.readFile (recordPath candidate) >>= pathText)
      unproven <- readUnproven state
      pure $ case parseResources <$> record of
        Right (Right built)
          | built /= installed -> DefaultImage (OtherDelivery (archive built))
          | Just held <- unproven, unprovenCandidate held == candidate -> DefaultImage (HeldBack held)
          | otherwise -> ProfileImage profile candidate
        _ -> DefaultImage (NoBuildRecord candidate)

recordPath :: FilePath -> FilePath
recordPath candidate = candidate <> ".resources"

unprovenPath :: FilePath -> FilePath
unprovenPath state = state </> "unproven"

-- | The candidate that was exec'd and has not proved healthy, and what is known of its trial.
data Unproven = Unproven {unprovenCandidate :: FilePath, unprovenTrial :: Trial}
  deriving (Eq, Show)

-- | What is known of a candidate's trial.
data Trial
  = -- | Exec'd once. It runs now, or it ended and left no record of why.
    FirstTrial
  | -- | Exec'd again, after a first trial of the same bytes ended with no record.
    SecondTrial
  | -- | Its own image recorded that its session failed.
    Crashed
  deriving (Eq, Show, Enum, Bounded)

trialWord :: Trial -> BS.ByteString
trialWord = \case
  FirstTrial -> B8.pack "first"
  SecondTrial -> B8.pack "second"
  Crashed -> B8.pack "crashed"

{- | Name the candidate that is about to be exec'd as on trial. Whoever execs
a profile writes this first, so a candidate that dies leaves its name behind.
The file holds the trial's word, a line feed, and the candidate's path.
-}
markUnproven :: FilePath -> Unproven -> IO ()
markUnproven state (Unproven candidate trial) = do
  path <- pathBytes candidate
  writeAtomic (unprovenPath state) (trialWord trial <> B8.singleton '\n' <> path)

readUnproven :: FilePath -> IO (Maybe Unproven)
readUnproven state =
  try @IOException (BS.readFile (unprovenPath state)) >>= \case
    Right content
      | (word, rest) <- B8.break (== '\n') content
      , Just trial <- List.find ((== word) . trialWord) [minBound ..]
      , Just (_, path) <- B8.uncons rest
      , not (BS.null path) ->
          Just . (`Unproven` trial) <$> pathText path
    _ -> pure Nothing

-- | Replace a file in one step, so a reader sees its old bytes or its new ones.
writeAtomic :: FilePath -> BS.ByteString -> IO ()
writeAtomic path bytes = do
  (temporary, handle) <- openBinaryTempFile (takeDirectory path) (takeFileName path <> "-next")
  (BS.hPut handle bytes >> hClose handle >> renameFile temporary path) `onException` (hClose handle >> removeFile temporary)

{- | The bytes the operating system knows a path by. The marker and the build
record hold their paths as these bytes, so a path that is not ASCII reads
back as the 'FilePath' it was written from.
-}
pathBytes :: String -> IO BS.ByteString
pathBytes text = getFileSystemEncoding >>= \encoding -> Foreign.withCStringLen encoding text BS.packCStringLen

pathText :: BS.ByteString -> IO String
pathText bytes = getFileSystemEncoding >>= \encoding -> BS.useAsCStringLen bytes (Foreign.peekCStringLen encoding)

{- | Remove the marker when it names this image, compared by device and inode
because the running executable's path is resolved and the marker's is not.
-}
clearUnproven :: FilePath -> FilePath -> IO ()
clearUnproven state image =
  readUnproven state
    >>= mapM_
      ( \(Unproven marked _) -> do
          same <- try @IOException ((,) <$> getFileStatus marked <*> getFileStatus image)
          case same of
            Right (a, b) | (deviceID a, fileID a) == (deviceID b, fileID b) -> removeFile (unprovenPath state) `catch` \(_ :: IOException) -> pure ()
            _ -> pure ()
      )

{- | Run a handover's exec with its candidate on trial. The exec returns only
when it failed. The profile link is back on the earlier candidate by then, so
that candidate's trial comes back too: a profile that died stays marked.
-}
withTrial :: (String -> IO ()) -> ValidatedTarget -> IO a -> IO a
withTrial report target exec = do
  let state = takeDirectory (validatedProfile target)
      attempt action = try @IOException action >>= either (\e -> report ("cannot record the trial of " <> validatedPath target <> " (" <> show e <> ")")) pure
  earlier <- readUnproven state
  attempt (markUnproven state (Unproven (validatedPath target) FirstTrial))
  result <- exec
  attempt (maybe (clearUnproven state (validatedPath target)) (markUnproven state) earlier)
  pure result

-- | Whether two files hold the same bytes. A file that cannot be read equals nothing.
sameBytes :: FilePath -> FilePath -> IO Bool
sameBytes first second =
  either (\(_ :: IOException) -> False) id <$> try (withBinaryFile first ReadMode (withBinaryFile second ReadMode . compareHandles))
 where
  compareHandles a b = do
    sizes <- (==) <$> hFileSize a <*> hFileSize b
    if sizes then chunks a b else pure False
  chunks a b = do
    left <- BS.hGet a 65536
    right <- BS.hGet b 65536
    if left /= right then pure False else if BS.null left then pure True else chunks a b

-- | Remove a candidate that nothing will run, with its record.
discardCandidate :: ValidatedTarget -> IO ()
discardCandidate target = mapM_ (\path -> removeFile path `catch` \(_ :: IOException) -> pure ()) [validatedPath target, recordPath (validatedPath target)]

{- | One build at a time per state directory. The launcher's check and a
session's build share the project directory and the private store. The lock
is @flock@ on a raw descriptor: it excludes a second build in this process
too, and GHC refuses a second write handle on one file. It is polled, because
a blocking call would stop every Haskell thread of the non-threaded default
runtime.
-}
withBuildLock :: FilePath -> IO () -> IO a -> IO a
withBuildLock state waiting action =
  bracket (openFd (state </> "build.lock") WriteOnly defaultFileFlags{creat = Just 0o600, cloexec = True}) closeFd $ \fd -> do
    free <- tryLock fd
    unless free (waiting >> poll fd)
    action
 where
  poll fd = threadDelay 50000 >> tryLock fd >>= \free -> unless free (poll fd)
  tryLock fd = do
    result <- c_flock (fromIntegral fd) (lockExclusive .|. lockNonBlocking)
    if result == 0
      then pure True
      else do
        errno <- getErrno
        if errno == eWOULDBLOCK || errno == eINTR then pure False else throwErrno "flock"
  lockExclusive = 2
  lockNonBlocking = 4

foreign import ccall unsafe "flock" c_flock :: CInt -> CInt -> IO CInt

atomicLink :: FilePath -> FilePath -> IO ()
atomicLink target profile = do
  (temporary, handle) <- openTempFile (takeDirectory profile) "profile-link"
  hClose handle
  removeFile temporary
  createSymbolicLink target temporary
  renameFile temporary profile `onException` removeFile temporary
