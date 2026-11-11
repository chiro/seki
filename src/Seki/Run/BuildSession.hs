{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.BuildSession (
  BuildSession,
  BuildState (..),
  BuildEvent (..),
  newBuildSession,
  newUnavailableBuildSession,
  transitionBuild,
  requestBuild,
  closeBuildSession,
  buildStatus,
  recordRecoveryFailure,
  readyTarget,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, stateTVar)
import Control.Exception (IOException, finally, mask_, try)
import Control.Monad (forever, void)
import Data.Aeson (ToJSON, Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as Text
import Seki.Run.ConfigBuild
import System.Environment (getExecutablePath)
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hPutStrLn, hSeek, stderr, withBinaryFile)

newtype BuildGeneration = BuildGeneration Integer deriving (Eq, Show, Num, ToJSON)

data BuildState
  = BuildIdle
  | BuildRunning BuildGeneration Bool (Maybe FilePath)
  | BuildFailed BuildGeneration String (Maybe FilePath)
  | BuildReady BuildGeneration ValidatedTarget
  deriving (Eq, Show)

data BuildEvent
  = BuildRequested
  | BuildLogged FilePath
  | BuildFinished (Either (String, Maybe FilePath) ValidatedTarget)
  deriving (Eq, Show)

transitionBuild :: BuildState -> BuildEvent -> BuildState
transitionBuild state event = case (state, event) of
  (BuildRunning generation _ logPath, BuildRequested) -> BuildRunning generation True logPath
  (BuildRunning generation queued _, BuildLogged path) -> BuildRunning generation queued (Just path)
  (BuildRunning generation True _, BuildFinished _) -> start (generation + 1)
  (BuildRunning generation False _, BuildFinished (Left (message, logPath))) -> BuildFailed generation message logPath
  (BuildRunning generation False _, BuildFinished (Right target)) -> BuildReady generation target
  (BuildIdle, BuildRequested) -> start 1
  (BuildFailed generation _ _, BuildRequested) -> start (generation + 1)
  (BuildReady generation _, BuildRequested) -> start (generation + 1)
  _ -> state
 where
  start generation = BuildRunning generation False Nothing

data BuildSession = BuildSession (TVar BuildState) ThreadId (MVar ())

newBuildSession :: IO () -> BuildContext -> IO BuildSession
newBuildSession wake = newSession wake . Right

newUnavailableBuildSession :: IO () -> String -> IO BuildSession
newUnavailableBuildSession wake = newSession wake . Left

{- | One thread runs every build of the session in order, so a result always
belongs to the build the state shows as running. It reports each change
through @wake@ rather than the command queue, which may be full.
-}
newSession :: IO () -> Either String BuildContext -> IO BuildSession
newSession wake context = do
  state <- newTVarIO BuildIdle
  done <- newEmptyMVar
  thread <- mask_ (forkIOWithUnmask (\unmask -> unmask (forever (runNextBuild context state wake)) `finally` putMVar done ()))
  pure (BuildSession state thread done)

runNextBuild :: Either String BuildContext -> TVar BuildState -> IO () -> IO ()
runNextBuild context state wake = do
  atomically (readTVar state >>= \case BuildRunning{} -> pure (); _ -> retry)
  logPath <- newIORef Nothing
  let step event = atomically (stateTVar state (\current -> let next = transitionBuild current event in (next, next)))
      report path = writeIORef logPath (Just path) >> step (BuildLogged path) >> wake
  result <- try @IOException $ case context of
    Left message -> ioError (userError message)
    Right installed@(BuildContext _ _ config directory) -> do
      resolution <- resolve installed
      mapM_ (\warning -> void (try @IOException (hPutStrLn stderr ("seki: " <> warning)))) (resolvedWarning resolution)
      privateDirectory directory >> generate (resolvedResources resolution) config >> buildWithLog resolution config directory report
  path <- readIORef logPath
  outcome <- case result of
    Left err -> do
      tailBytes <- maybe (pure BS.empty) diagnosticTail path
      pure (Left (show err <> "\n" <> map (toEnum . fromEnum) (BS.unpack tailBytes), path))
    Right target -> pure (Right target)
  step (BuildFinished outcome) >>= \case
    BuildFailed _ message _ -> void (try @IOException (hPutStrLn stderr ("seki: configuration build failed: " <> take 8192 message)))
    _ -> pure ()
  wake

requestBuild :: BuildSession -> IO ()
requestBuild (BuildSession state _ _) = atomically (modifyTVar' state (`transitionBuild` BuildRequested))

closeBuildSession :: BuildSession -> IO ()
closeBuildSession (BuildSession _ thread done) = mask_ (killThread thread >> readMVar done)

readyTarget :: BuildSession -> IO (Maybe ValidatedTarget)
readyTarget (BuildSession state _ _) =
  readTVarIO state >>= \case
    BuildReady _ target -> pure (Just target)
    _ -> pure Nothing

buildStatus :: BuildSession -> IO Value
buildStatus (BuildSession state _ _) = do
  image <- getExecutablePath
  status <- readStatus
  pure $ case status of
    Object fields -> Object (KeyMap.insert "active_image" (toJSON image) fields)
    other -> other
 where
  readStatus =
    readTVarIO state >>= \case
      BuildIdle -> pure (value "idle" 0 Nothing Nothing)
      BuildRunning generation queued logPath -> pure (object ["log_path" .= logPath, "state" .= ("building" :: String), "generation" .= generation, "queued" .= if queued then Just (generation + 1) else Nothing])
      BuildFailed generation message logPath -> pure (value "failed" generation (Just (take 8192 message)) logPath)
      BuildReady generation target -> pure (value "validated" generation Nothing (Just (validatedLog target)))
  value :: String -> BuildGeneration -> Maybe String -> Maybe FilePath -> Value
  value status generation message logPath = object ["state" .= status, "generation" .= generation, "error" .= fmap Text.pack message, "log_path" .= logPath]

diagnosticTail :: FilePath -> IO BS.ByteString
diagnosticTail path =
  either (const BS.empty) id
    <$> try @IOException
      ( withBinaryFile path ReadMode $ \handle -> do
          size <- hFileSize handle
          hSeek handle AbsoluteSeek (max 0 (size - 8192))
          BS.hGet handle 8192
      )

recordRecoveryFailure :: BuildSession -> String -> IO ()
recordRecoveryFailure (BuildSession state _ _) message =
  atomically $
    modifyTVar' state $ \case
      BuildIdle -> BuildFailed 0 message Nothing
      current -> current
