{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.BuildSession (
  BuildSession,
  BuildState (..),
  BuildEvent (..),
  BuildEffect (..),
  newBuildSession,
  newUnavailableBuildSession,
  transitionBuild,
  requestBuild,
  completeBuild,
  closeBuildSession,
  buildStatus,
  recordRecoveryFailure,
  readyTarget,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (IOException, finally, mask, try)
import Control.Monad (void)
import Data.Aeson (ToJSON, Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Run.ConfigBuild
import Seki.Run.External (Admission (..))
import System.Environment (getExecutablePath)
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hPutStrLn, hSeek, stderr, withBinaryFile)

newtype BuildEpoch = BuildEpoch Integer deriving (Eq, Show, Num, ToJSON)
newtype BuildGeneration = BuildGeneration Integer deriving (Eq, Show, Num, ToJSON)
data BuildState
  = BuildIdle BuildEpoch
  | BuildRunning BuildEpoch BuildGeneration (Maybe BuildGeneration) (Maybe FilePath)
  | BuildFailed BuildEpoch BuildGeneration String (Maybe FilePath)
  | BuildReady BuildEpoch BuildGeneration ValidatedTarget
  | BuildUnchanged BuildEpoch BuildGeneration ValidatedTarget
  | BuildClosed
  deriving (Eq, Show)

data BuildEvent
  = BuildRequested
  | BuildCompleted BuildEpoch BuildGeneration (Either (String, Maybe FilePath) ValidatedTarget)
  | BuildLog BuildEpoch BuildGeneration FilePath
  | BuildSameImage BuildEpoch BuildGeneration ValidatedTarget
  deriving (Eq, Show)
data BuildEffect = StartBuild BuildEpoch BuildGeneration | ActivateBuild | NoBuildEffect deriving (Eq, Show)

transitionBuild :: BuildState -> BuildEvent -> (BuildState, BuildEffect)
transitionBuild state event = case (state, event) of
  (BuildClosed, _) -> (state, NoBuildEffect)
  (BuildRunning epoch generation _ logPath, BuildRequested) -> (BuildRunning epoch generation (Just (generation + 1)) logPath, NoBuildEffect)
  (BuildRunning epoch generation queued _, BuildCompleted e g result)
    | epoch == e && generation == g -> case queued of
        Just next -> (BuildRunning epoch next Nothing Nothing, StartBuild epoch next)
        Nothing -> case result of
          Left (message, logPath) -> (BuildFailed epoch generation message logPath, NoBuildEffect)
          Right target -> (BuildReady epoch generation target, ActivateBuild)
  (BuildRunning epoch generation queued _, BuildLog e g path)
    | epoch == e && generation == g -> (BuildRunning epoch generation queued (Just path), NoBuildEffect)
  (BuildRunning epoch generation queued _, BuildSameImage e g target)
    | epoch == e && generation == g -> case queued of
        Just next -> (BuildRunning epoch next Nothing Nothing, StartBuild epoch next)
        Nothing -> (BuildUnchanged epoch generation target, NoBuildEffect)
  (BuildUnchanged epoch generation _, BuildRequested) -> start epoch (generation + 1)
  (BuildIdle epoch, BuildRequested) -> start epoch 1
  (BuildFailed epoch generation _ _, BuildRequested) -> start epoch (generation + 1)
  (BuildReady epoch generation _, BuildRequested) -> start epoch (generation + 1)
  _ -> (state, NoBuildEffect)
 where
  start epoch generation = (BuildRunning epoch generation Nothing Nothing, StartBuild epoch generation)

data Worker = Worker ThreadId (MVar ())
data WorkerSlot = WorkersOpen (Maybe Worker) | WorkersClosed
data BuildSession = BuildSession (Either String BuildContext) (IORef BuildState) (MVar WorkerSlot)

newBuildSession :: BuildContext -> IO BuildSession
newBuildSession = newSession . Right

newUnavailableBuildSession :: String -> IO BuildSession
newUnavailableBuildSession = newSession . Left

newSession :: Either String BuildContext -> IO BuildSession
newSession context = do
  epoch <- BuildEpoch . toInteger <$> getMonotonicTimeNSec
  BuildSession context <$> newIORef (BuildIdle epoch) <*> newMVar (WorkersOpen Nothing)

requestBuild :: BuildSession -> (BuildEvent -> IO Admission) -> IO ()
requestBuild session send = apply session send BuildRequested >> pure ()

completeBuild :: BuildSession -> (BuildEvent -> IO Admission) -> BuildEvent -> IO Bool
completeBuild = apply

apply :: BuildSession -> (BuildEvent -> IO Admission) -> BuildEvent -> IO Bool
apply session@(BuildSession _ state _) send event = do
  old <- readIORef state
  let (next, effect) = transitionBuild old event
  writeIORef state next
  case next of
    BuildFailed _ _ message _ | old /= next -> void (try @IOException (hPutStrLn stderr ("seki: configuration build failed: " <> take 8192 message)))
    _ -> pure ()
  case effect of
    StartBuild epoch generation -> startWorker session send epoch generation >> pure False
    ActivateBuild -> pure True
    NoBuildEffect -> pure False

startWorker :: BuildSession -> (BuildEvent -> IO Admission) -> BuildEpoch -> BuildGeneration -> IO ()
startWorker (BuildSession context _ slot) send epoch generation = modifyMVar_ slot $ \case
  WorkersClosed -> pure WorkersClosed
  WorkersOpen previous -> mask $ \_ -> do
    mapM_ (\(Worker _ done) -> readMVar done) previous
    done <- newEmptyMVar
    gate <- newEmptyMVar
    tid <- forkIOWithUnmask $ \unmask ->
      ( unmask $ do
          takeMVar gate
          logPath <- newIORef Nothing
          let deliver event =
                send event >>= \case
                  Full -> threadDelay 10000 >> deliver event
                  _ -> pure ()
              report path = writeIORef logPath (Just path) >> deliver (BuildLog epoch generation path)
          result <- try @IOException $ case context of
            Left message -> ioError (userError message)
            Right (BuildContext resources config directory) -> privateDirectory directory >> generate resources config >> buildWithLog resources config directory report
          path <- readIORef logPath
          event <- case result of
            Left err -> do
              tailBytes <- maybe (pure BS.empty) diagnosticTail path
              pure (BuildCompleted epoch generation (Left (show err <> "\n" <> map (toEnum . fromEnum) (BS.unpack tailBytes), path)))
            Right target -> do
              current <- getExecutablePath
              same <- either (const False) id <$> try @IOException (sameImage current (validatedPath target))
              pure (if same then BuildSameImage epoch generation target else BuildCompleted epoch generation (Right target))
          deliver event
      )
        `finally` putMVar done ()
    putMVar gate ()
    pure (WorkersOpen (Just (Worker tid done)))

closeBuildSession :: BuildSession -> IO ()
closeBuildSession (BuildSession _ _ slot) = mask $ \_ -> do
  worker <- swapMVar slot WorkersClosed
  case worker of
    WorkersClosed -> pure ()
    WorkersOpen active -> mapM_ (\(Worker tid done) -> killThread tid >> void (readMVar done)) active

readyTarget :: BuildSession -> IO (Maybe ValidatedTarget)
readyTarget (BuildSession _ state _) =
  readIORef state >>= \case
    BuildReady _ _ target -> pure (Just target)
    _ -> pure Nothing

buildStatus :: BuildSession -> IO Value
buildStatus (BuildSession _ state _) = do
  image <- getExecutablePath
  status <- readStatus
  pure $ case status of
    Object fields -> Object (KeyMap.insert "active_image" (toJSON image) fields)
    other -> other
 where
  readStatus =
    readIORef state >>= \case
      BuildIdle epoch -> pure (value "idle" epoch 0 Nothing Nothing)
      BuildRunning epoch generation queued logPath -> pure (object ["log_path" .= logPath, "state" .= ("building" :: String), "epoch" .= epoch, "generation" .= generation, "queued" .= queued])
      BuildFailed epoch generation message logPath -> pure (value "failed" epoch generation (Just (take 8192 message)) logPath)
      BuildReady epoch generation target -> pure (value "validated" epoch generation Nothing (Just (validatedLog target)))
      BuildUnchanged epoch generation target -> pure (value "unchanged" epoch generation Nothing (Just (validatedLog target)))
      BuildClosed -> pure (object ["state" .= ("closed" :: String)])
  value :: String -> BuildEpoch -> BuildGeneration -> Maybe String -> Maybe FilePath -> Value
  value status epoch generation message logPath = object ["state" .= status, "epoch" .= epoch, "generation" .= generation, "error" .= fmap Text.pack message, "log_path" .= logPath]

sameImage :: FilePath -> FilePath -> IO Bool
sameImage first second = withBinaryFile first ReadMode $ \a -> withBinaryFile second ReadMode $ \b -> do
  x <- hFileSize a
  y <- hFileSize b
  let loop = do
        left <- BS.hGet a 65536
        right <- BS.hGet b 65536
        if left /= right then pure False else if BS.null left then pure True else loop
  if x /= y then pure False else loop

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
recordRecoveryFailure (BuildSession _ state _) message =
  readIORef state >>= \case
    BuildIdle epoch -> writeIORef state (BuildFailed epoch 0 message Nothing)
    _ -> pure ()
