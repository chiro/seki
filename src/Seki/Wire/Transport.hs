{-# LANGUAGE LambdaCase #-}

{- | Foundations for the Wayland Unix transport.

This module resolves the standard Wayland connection environment and owns raw
file descriptors explicitly. Socket IO and ancillary-data transfer are added
on top of these types.
-}
module Seki.Wire.Transport (
  ConnectionTarget (..),
  ConnectionDiscoveryError (..),
  maxUnixSocketPathBytes,
  discoverConnectionTarget,
  resolveConnectionTarget,
  OwnedFd,
  OwnedFdError (..),
  adoptFd,
  withOwnedFd,
  releaseOwnedFd,
  closeOwnedFd,
)
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (mask, mask_, onException)
import Data.ByteString qualified as ByteString
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Foreign.C.Types (CInt)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, (</>))
import System.Posix.IO (FdOption (CloseOnExec), closeFd, setFdOption)
import System.Posix.Types (Fd (..))

-- | The source from which a Wayland connection should be opened.
data ConnectionTarget
  = InheritedSocket Fd
  | UnixSocketPath FilePath
  deriving (Eq, Show)

-- | A malformed or incomplete Wayland connection environment.
data ConnectionDiscoveryError
  = WaylandSocketEmpty
  | WaylandSocketNotDecimal String
  | WaylandSocketOutOfRange Integer
  | WaylandDisplayEmpty
  | WaylandDisplayContainsNul
  | XdgRuntimeDirMissing
  | XdgRuntimeDirNotAbsolute FilePath
  | UnixSocketPathContainsNul
  | UnixSocketPathTooLong Int
  deriving (Eq, Show)

-- | Maximum pathname bytes in Linux's @sockaddr_un.sun_path@, excluding NUL.
maxUnixSocketPathBytes :: Int
maxUnixSocketPathBytes = 107

-- | Read and resolve the standard Wayland connection environment variables.
discoverConnectionTarget :: IO (Either ConnectionDiscoveryError ConnectionTarget)
discoverConnectionTarget = do
  waylandSocket <- lookupEnv "WAYLAND_SOCKET"
  waylandDisplay <- lookupEnv "WAYLAND_DISPLAY"
  runtimeDirectory <- lookupEnv "XDG_RUNTIME_DIR"
  pure (resolveConnectionTarget waylandSocket waylandDisplay runtimeDirectory)

{- | Resolve connection variables using libwayland's precedence rules.

An inherited @WAYLAND_SOCKET@ takes precedence. Otherwise an absolute
@WAYLAND_DISPLAY@ is used directly, while a display name is resolved beneath
@XDG_RUNTIME_DIR@. An unset display defaults to @wayland-0@.
-}
resolveConnectionTarget :: Maybe String -> Maybe FilePath -> Maybe FilePath -> Either ConnectionDiscoveryError ConnectionTarget
resolveConnectionTarget (Just socketValue) _ _ = InheritedSocket <$> parseInheritedFd socketValue
resolveConnectionTarget Nothing displayValue runtimeDirectory = do
  display <- validateDisplay (fromMaybe "wayland-0" displayValue)
  if isAbsolute display
    then UnixSocketPath <$> validateSocketPath display
    else do
      runtime <- validateRuntimeDirectory runtimeDirectory
      UnixSocketPath <$> validateSocketPath (runtime </> display)

parseInheritedFd :: String -> Either ConnectionDiscoveryError Fd
parseInheritedFd value
  | null value = Left WaylandSocketEmpty
  | not (all isDigit value) = Left (WaylandSocketNotDecimal value)
  | parsed > toInteger (maxBound :: CInt) = Left (WaylandSocketOutOfRange parsed)
  | otherwise = Right (Fd (fromInteger parsed))
 where
  parsed = read value

validateDisplay :: FilePath -> Either ConnectionDiscoveryError FilePath
validateDisplay display
  | null display = Left WaylandDisplayEmpty
  | '\0' `elem` display = Left WaylandDisplayContainsNul
  | otherwise = Right display

validateRuntimeDirectory :: Maybe FilePath -> Either ConnectionDiscoveryError FilePath
validateRuntimeDirectory Nothing = Left XdgRuntimeDirMissing
validateRuntimeDirectory (Just runtime)
  | null runtime = Left XdgRuntimeDirMissing
  | not (isAbsolute runtime) = Left (XdgRuntimeDirNotAbsolute runtime)
  | otherwise = Right runtime

validateSocketPath :: FilePath -> Either ConnectionDiscoveryError FilePath
validateSocketPath path
  | '\0' `elem` path = Left UnixSocketPathContainsNul
  | pathBytes > maxUnixSocketPathBytes = Left (UnixSocketPathTooLong pathBytes)
  | otherwise = Right path
 where
  pathBytes = ByteString.length (TextEncoding.encodeUtf8 (Text.pack path))

-- | A descriptor that is either owned exactly once or has been released.
newtype OwnedFd = OwnedFd
  { ownedFdState :: MVar (Maybe Fd)
  }

-- | An operation attempted after descriptor ownership was transferred or closed.
data OwnedFdError = OwnedFdAlreadyReleased
  deriving (Eq, Show)

{- | Take ownership of a descriptor and mark it close-on-exec.

If setting close-on-exec fails, the descriptor is closed before the exception
is rethrown.
-}
adoptFd :: Fd -> IO OwnedFd
adoptFd descriptor =
  mask $ \restore -> do
    restore (setFdOption descriptor CloseOnExec True) `onException` closeFd descriptor
    OwnedFd <$> newMVar (Just descriptor)

{- | Borrow the descriptor for an action without transferring ownership.

The borrow holds the ownership lock, so a concurrent close or release waits for
the action. The action must not close the borrowed descriptor itself.
-}
withOwnedFd :: OwnedFd -> (Fd -> IO value) -> IO (Either OwnedFdError value)
withOwnedFd owned action =
  withMVar (ownedFdState owned) $ \case
    Nothing -> pure (Left OwnedFdAlreadyReleased)
    Just descriptor -> Right <$> action descriptor

-- | Transfer ownership to the caller. A second transfer fails.
releaseOwnedFd :: OwnedFd -> IO (Either OwnedFdError Fd)
releaseOwnedFd owned =
  modifyMVar (ownedFdState owned) $ \case
    Nothing -> pure (Nothing, Left OwnedFdAlreadyReleased)
    Just descriptor -> pure (Nothing, Right descriptor)

{- | Close an owned descriptor.

Returns 'False' when ownership had already been transferred or closed. The
state is cleared before calling @close(2)@ so a close error cannot later cause a
reused descriptor number to be closed accidentally.
-}
closeOwnedFd :: OwnedFd -> IO Bool
closeOwnedFd owned = mask_ $ do
  descriptor <-
    modifyMVar (ownedFdState owned) $ \state ->
      pure (Nothing, state)
  case descriptor of
    Nothing -> pure False
    Just value -> closeFd value >> pure True
