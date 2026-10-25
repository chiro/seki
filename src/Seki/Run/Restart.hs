{-# LANGUAGE LambdaCase #-}

{- | Replacing the running seki with the executable it started from (M2-D06).

The facts a restart needs are recorded once at startup in a 'Launch'. The
check runs before @stop@, so a refused restart leaves the session running.
After @finished@ the state is written to a private file, and the process
execs the recorded path with @--restore@ and that file. If the exec fails,
the caller reconnects in the same process.
-}
module Seki.Run.Restart (
  RestartRefusal (..),
  describeRefusal,
  Launch (..),
  discoverLaunch,
  targetFromArgv0,
  checkRestart,
  writeRestoreFile,
  RestoreLoadError (..),
  describeLoadError,
  loadRestoreFile,
  execRestart,
  reapInheritedChildren,
) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Void (Void, absurd)
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Internal.Core.Restore (RestoreError (..), RestoreState, decodeRestoreState, encodeRestoreState)
import Seki.Wire.Transport (ConnectionDiscoveryError, ConnectionTarget, discoverConnectionTarget, resolveConnectionTarget)
import System.Directory (createDirectoryIfMissing, findExecutable, getCurrentDirectory, removeFile, renameFile)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.FilePath (isAbsolute, normalise, (</>))
import System.IO (hFlush, stderr, stdout)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (fileAccess, getFileStatus, isRegularFile, setFileMode)
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.IO.ByteString (fdWrite)
import System.Posix.Process (executeFile, getAnyProcessStatus, getProcessID)

-- | Why a restart was refused before @stop@. seki keeps running.
data RestartRefusal
  = -- | Neither argv[0] nor the running executable gave a path at startup.
    TargetUnrecorded
  | TargetMissing FilePath
  | TargetNotRegularFile FilePath
  | TargetNotExecutable FilePath
  | -- | No socket path to reconnect to, so neither the new process nor a fallback could connect.
    NoReconnectTarget ConnectionDiscoveryError
  | -- | @XDG_RUNTIME_DIR@ is unset or relative, so the state has nowhere to go.
    NoStateDirectory
  deriving (Eq, Show)

describeRefusal :: RestartRefusal -> String
describeRefusal = \case
  TargetUnrecorded -> "no executable path was recorded at startup"
  TargetMissing path -> path <> " does not exist"
  TargetNotRegularFile path -> path <> " is not a regular file"
  TargetNotExecutable path -> path <> " is not executable"
  NoReconnectTarget e -> "no Wayland socket path to reconnect to: " <> show e
  NoStateDirectory -> "XDG_RUNTIME_DIR is unset or not absolute"

-- | Facts recorded once at startup, before connecting.
data Launch = Launch
  { launchFirstTarget :: Either ConnectionDiscoveryError ConnectionTarget
  -- ^ The usual discovery, which honors @WAYLAND_SOCKET@.
  , launchReconnectTarget :: Either ConnectionDiscoveryError ConnectionTarget
  -- ^ The socket path alone. An inherited socket is gone once its connection
  -- closes, so a fallback session and the next process connect here.
  , launchTarget :: Either RestartRefusal FilePath
  , launchStateDir :: Either RestartRefusal FilePath
  , launchExec :: FilePath -> [String] -> [(String, String)] -> IO Void
  -- ^ Replace the process image; it returns only by throwing.
  }

discoverLaunch :: IO Launch
discoverLaunch = do
  first <- discoverConnectionTarget
  reconnect <- resolveConnectionTarget Nothing <$> lookupEnv "WAYLAND_DISPLAY" <*> lookupEnv "XDG_RUNTIME_DIR"
  target <- recordRestartTarget
  runtime <- lookupEnv "XDG_RUNTIME_DIR"
  pure
    Launch
      { launchFirstTarget = first
      , launchReconnectTarget = reconnect
      , launchTarget = target
      , launchStateDir = case runtime of
          Just dir | isAbsolute dir -> Right (dir </> "seki")
          _ -> Left NoStateDirectory
      , launchExec = \path args env -> executeFile path False args (Just env)
      }

{- | The path to exec, recorded without resolving symlinks, so a profile
symlink that now points at a new build reaches it. argv[0] comes from
@/proc/self/cmdline@, which holds it as the process was started. The running
executable's resolved path is the last resort, since after a rebuild it
names the old file.
-}
recordRestartTarget :: IO (Either RestartRefusal FilePath)
recordRestartTarget = do
  cmdline <- try (ByteString.readFile "/proc/self/cmdline")
  cwd <- getCurrentDirectory
  fromArgv <- either (\(_ :: IOException) -> pure Nothing) (targetFromArgv0 cwd findExecutable . Char8.unpack . Char8.takeWhile (/= '\NUL')) cmdline
  case fromArgv of
    Just path -> pure (Right path)
    Nothing -> either (\(_ :: IOException) -> Left TargetUnrecorded) Right <$> try getExecutablePath

{- | argv[0] made absolute against the startup directory when it holds a
slash, otherwise looked up on PATH as @execvp@ would, without following
symlinks.
-}
targetFromArgv0 :: FilePath -> (String -> IO (Maybe FilePath)) -> String -> IO (Maybe FilePath)
targetFromArgv0 cwd search argv0
  | null argv0 = pure Nothing
  | '/' `elem` argv0 = pure (Just (absolute argv0))
  | otherwise = fmap absolute <$> search argv0
 where
  absolute path = normalise (if isAbsolute path then path else cwd </> path)

{- | Before @stop@: the target, following symlinks, is a regular file seki may
execute, a socket path to reconnect to exists, and so does a state directory.
-}
checkRestart :: Launch -> IO (Either RestartRefusal ())
checkRestart launch = case launchTarget launch of
  Left refusal -> pure (Left refusal)
  Right path ->
    try (getFileStatus path) >>= \case
      Left e
        | isDoesNotExistError e -> pure (Left (TargetMissing path))
        | otherwise -> pure (Left (TargetNotExecutable path))
      Right status
        | not (isRegularFile status) -> pure (Left (TargetNotRegularFile path))
        | otherwise -> do
            executable <- either (\(_ :: IOException) -> False) id <$> try (fileAccess path False False True)
            pure $
              if not executable
                then Left (TargetNotExecutable path)
                else do
                  _ <- either (Left . NoReconnectTarget) Right (launchReconnectTarget launch)
                  _ <- launchStateDir launch
                  Right ()

{- | Write the state to @restore-<pid>-<ns>.state@ in a 0700 directory: a 0600
temporary file renamed into place, so a reader never sees a partial file.
The pid alone repeats across exec, hence the monotonic time.
-}
writeRestoreFile :: FilePath -> RestoreState String -> IO (Either IOException FilePath)
writeRestoreFile dir saved = try $ do
  createDirectoryIfMissing True dir
  setFileMode dir 0o700
  pid <- getProcessID
  now <- getMonotonicTimeNSec
  let path = dir </> ("restore-" <> show pid <> "-" <> show now <> ".state")
      temporary = path <> ".tmp"
  bracket (openFd temporary WriteOnly defaultFileFlags{creat = Just 0o600, trunc = True, exclusive = True}) closeFd $ \fd ->
    writeAll fd (Char8.pack (encodeRestoreState saved))
  renameFile temporary path
  pure path
 where
  writeAll fd bytes = unless (ByteString.null bytes) $ do
    written <- fdWrite fd bytes
    writeAll fd (ByteString.drop (fromIntegral written) bytes)

data RestoreLoadError = RestoreFileUnreadable IOException | RestoreDecodeError RestoreError
  deriving (Eq, Show)

describeLoadError :: RestoreLoadError -> String
describeLoadError = \case
  RestoreFileUnreadable e -> "cannot read the state file (" <> show e <> "), starting fresh"
  RestoreDecodeError (RestoreUnknownVersion v) -> "unknown state version " <> v <> ", starting fresh"
  RestoreDecodeError RestoreUnreadable -> "the state file does not read, starting fresh"

{- | Read, delete, and decode a state file. It is deleted whatever it holds,
since a seki that river's init loop starts after a crash never passes
@--restore@ and would otherwise leave it behind.
-}
loadRestoreFile :: FilePath -> IO (Either RestoreLoadError (RestoreState String))
loadRestoreFile path =
  try (ByteString.readFile path) >>= \case
    Left e -> pure (Left (RestoreFileUnreadable e))
    Right bytes -> do
      _ <- try @IOException (removeFile path)
      pure (either (Left . RestoreDecodeError) Right (decodeRestoreState (Char8.unpack bytes)))

{- | Exec the target with @--restore FILE@ and the environment minus
@WAYLAND_SOCKET@, whose descriptor is closed by now. It returns only when
the exec failed, which 'Void' makes a type fact.
-}
execRestart :: Launch -> FilePath -> IO IOException
execRestart launch file = case launchTarget launch of
  Left refusal -> pure (userError (describeRefusal refusal))
  Right target -> do
    env <- filter ((/= "WAYLAND_SOCKET") . fst) <$> getEnvironment
    hFlush stdout
    hFlush stderr
    either id absurd <$> try (launchExec launch target ["--restore", file] env)

{- | Reap children the previous process image left behind: launch shells that
exited after its last reap. Runs before seki starts anything of its own.
-}
reapInheritedChildren :: IO ()
reapInheritedChildren =
  try (getAnyProcessStatus False False) >>= \case
    Right (Just _) -> reapInheritedChildren
    Right Nothing -> pure ()
    Left (_ :: IOException) -> pure ()
