{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.BuildSession (
  BuildSession,
  BuildState (..),
  BuildPurpose (..),
  BuildEvent (..),
  Reproduced (..),
  StartupBuild (..),
  newBuildSession,
  transitionBuild,
  requestBuild,
  ownerManaged,
  refuseTarget,
  closeBuildSession,
  buildStatus,
  recordRecoveryFailure,
  readyTarget,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, stateTVar, writeTVar)
import Control.Exception (IOException, finally, mask_, try)
import Control.Monad (forever, void, when)
import Data.Aeson (ToJSON, Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as Text
import GHC.Fingerprint (getFileHash)
import Seki.Run.ConfigBuild
import System.Directory (getFileSize)
import System.Environment (getExecutablePath)
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hPutStrLn, hSeek, stderr, withBinaryFile)

newtype BuildGeneration = BuildGeneration Integer deriving (Eq, Show, Num, ToJSON)

-- | What the running build's result is for.
data BuildPurpose
  = -- | Nobody asked for it. A fresh start owes it, and only a new image replaces the running one.
    AtStartup
  | -- | The restart action or @recompile@ asked for it. Any validated candidate replaces the running image.
    OnRequest
  | -- | A request arrived during the build. The result is discarded and one more build runs.
    Superseded
  deriving (Eq, Show)

data BuildState
  = BuildIdle
  | BuildRunning BuildGeneration BuildPurpose (Maybe FilePath)
  | BuildFailed BuildGeneration String (Maybe FilePath)
  | BuildReady BuildGeneration ValidatedTarget
  | -- | A startup build reproduced the running image. Nothing restarts. The path is the build log.
    BuildUnchanged BuildGeneration FilePath
  deriving (Eq, Show)

-- | Which known image a validated candidate has the bytes of.
data Reproduced
  = NewImage
  | RunningImage
  | -- | The candidate that died before it proved healthy.
    UnprovenImage FilePath
  deriving (Eq, Show)

data BuildEvent
  = BuildRequested
  | BuildLogged FilePath
  | BuildFinished (Either (String, Maybe FilePath) (ValidatedTarget, Reproduced))
  | -- | The restart check refused the ready target.
    TargetRefused String
  deriving (Eq, Show)

-- | Whether a session owes one build that nobody asked for.
data StartupBuild = NoStartupBuild | StartupBuild
  deriving (Eq, Show)

transitionBuild :: BuildState -> BuildEvent -> BuildState
transitionBuild state event = case (state, event) of
  (BuildRunning generation _ logPath, BuildRequested) -> BuildRunning generation Superseded logPath
  (BuildRunning generation purpose _, BuildLogged path) -> BuildRunning generation purpose (Just path)
  (BuildRunning generation Superseded _, BuildFinished _) -> start (generation + 1)
  (BuildRunning generation _ _, BuildFinished (Left (message, logPath))) -> BuildFailed generation message logPath
  (BuildRunning generation AtStartup _, BuildFinished (Right (target, RunningImage))) -> BuildUnchanged generation (validatedLog target)
  (BuildRunning generation AtStartup _, BuildFinished (Right (target, UnprovenImage dead))) ->
    BuildFailed generation ("the build reproduced " <> dead <> ", which died before it proved healthy; change the configuration, or use the restart binding to try it again") (Just (validatedLog target))
  (BuildRunning generation _ _, BuildFinished (Right (target, _))) -> BuildReady generation target
  (BuildReady generation target, TargetRefused refusal) -> BuildFailed generation ("restart refused: " <> refusal) (Just (validatedLog target))
  (BuildIdle, BuildRequested) -> start 1
  (BuildFailed generation _ _, BuildRequested) -> start (generation + 1)
  (BuildReady generation _, BuildRequested) -> start (generation + 1)
  (BuildUnchanged generation _, BuildRequested) -> start (generation + 1)
  _ -> state
 where
  start generation = BuildRunning generation OnRequest Nothing

-- | How far the owner has come. A startup build and the proof of health wait on it.
data OwnerProgress
  = NotManaged
  | Managed
  | -- | A prepared manage sequence ran the startup hook.
    HookRan
  | Proven
  deriving (Eq)

data BuildSession = BuildSession
  { sessionState :: TVar BuildState
  , sessionWarning :: TVar (Maybe String)
  , sessionOwner :: TVar OwnerProgress
  , sessionVouch :: IO ()
  -- ^ Clear the unproven marker when it names this session's image.
  , sessionThread :: ThreadId
  , sessionDone :: MVar ()
  }

{- | One thread runs every build of the session in order, so a result always
belongs to the build the state shows as running. It reports each change
through @wake@ rather than the command queue, which may be full. The session
takes what the start owes and the executable it runs when it is made, so no
caller asks for the startup build.
-}
newBuildSession :: IO () -> StartupBuild -> IO FilePath -> Either String BuildContext -> IO BuildSession
newBuildSession wake startup image context = do
  state <- newTVarIO (case startup of StartupBuild -> BuildRunning 1 AtStartup Nothing; NoStartupBuild -> BuildIdle)
  warning <- newTVarIO Nothing
  owner <- newTVarIO NotManaged
  done <- newEmptyMVar
  let warnWhileIdle = mapM_ (resolveAndWarn warning wake) context
      vouch = mapM_ (\installed -> try @IOException (image >>= clearUnproven (buildStateDirectory installed))) context
  thread <- mask_ (forkIOWithUnmask (\unmask -> unmask (warnWhileIdle >> forever (runNextBuild context image state owner warning wake)) `finally` putMVar done ()))
  pure (BuildSession state warning owner (void vouch) thread done)

resolveAndWarn :: TVar (Maybe String) -> IO () -> BuildContext -> IO Resolution
resolveAndWarn warning wake context = do
  resolution <- resolve context
  let current = resolvedWarning resolution
  changed <- atomically (stateTVar warning (\previous -> (previous /= current, current)))
  when changed $ do
    mapM_ diagnostic current
    wake
  pure resolution

{- | A startup build waits until its owner has prepared a manage sequence. A
build on request already follows one.
-}
runNextBuild :: Either String BuildContext -> IO FilePath -> TVar BuildState -> TVar OwnerProgress -> TVar (Maybe String) -> IO () -> IO ()
runNextBuild context image state owner warning wake = do
  atomically $
    readTVar state >>= \case
      BuildRunning _ AtStartup _ -> readTVar owner >>= \progress -> when (progress == NotManaged) retry
      BuildRunning{} -> pure ()
      _ -> retry
  logPath <- newIORef Nothing
  let step event = atomically (stateTVar state (\current -> let next = transitionBuild current event in (next, next)))
      report path = writeIORef logPath (Just path) >> step (BuildLogged path) >> wake
  result <- try @IOException $ case context of
    Left message -> ioError (userError message)
    Right installed@(BuildContext _ _ config directory) -> do
      resolution <- resolveAndWarn warning wake installed
      target <- privateDirectory directory >> buildWithLog resolution config directory report
      (,) target <$> reproduced directory target
  path <- readIORef logPath
  outcome <- case result of
    Left err -> do
      tailBytes <- maybe (pure BS.empty) diagnosticTail path
      pure (Left (show err <> "\n" <> map (toEnum . fromEnum) (BS.unpack tailBytes), path))
    Right built -> pure (Right built)
  next <- step (BuildFinished outcome)
  case (outcome, next) of
    (_, BuildFailed _ message _) -> do
      mapM_ (discardCandidate . fst) outcome
      diagnostic ("configuration build failed: " <> take 8192 message)
    (Right (target, _), BuildUnchanged{}) -> discardCandidate target
    _ -> pure ()
  wake
 where
  reproduced directory target = do
    running <- image
    let candidate = validatedPath target
    known <-
      sameBytes running candidate >>= \case
        True -> pure RunningImage
        False ->
          readUnproven directory >>= \case
            Nothing -> pure NewImage
            Just dead -> bool NewImage (UnprovenImage dead) <$> sameBytes dead candidate
    built <- describe candidate
    current <- describe running
    diagnostic $ case known of
      NewImage -> "candidate " <> built <> " differs from the running image " <> current
      RunningImage -> "candidate " <> built <> " has the bytes of the running image " <> current
      UnprovenImage dead -> "candidate " <> built <> " has the bytes of " <> dead <> ", which died before it proved healthy"
    pure known
  describe file =
    try @IOException ((,) <$> getFileSize file <*> getFileHash file) >>= \case
      Right (size, hash) -> pure (file <> " (" <> show size <> " bytes, " <> show hash <> ")")
      Left _ -> pure (file <> " (unreadable)")

diagnostic :: String -> IO ()
diagnostic message = void (try @IOException (hPutStrLn stderr ("seki: " <> message)))

requestBuild :: BuildSession -> IO ()
requestBuild session = atomically (modifyTVar' (sessionState session) (`transitionBuild` BuildRequested))

-- | The restart check refused the ready target. It becomes a failure, so the check does not repeat.
refuseTarget :: BuildSession -> String -> IO ()
refuseTarget session refusal = atomically (modifyTVar' (sessionState session) (`transitionBuild` TargetRefused refusal))

{- | The owner prepared a manage sequence, and says whether its startup hook
has run by now. The first call releases a startup build. The image proves
healthy, and clears a marker that names it, at a call that follows the one
that first reported the hook, once the startup build has ended. A profile
that dies earlier leaves its marker, so the launcher starts the default
runtime, and a build of the current configuration still runs to its end.
-}
ownerManaged :: BuildSession -> Bool -> IO ()
ownerManaged session hookRan = do
  proven <- atomically $ do
    progress <- readTVar (sessionOwner session)
    startupBuildRuns <- (\case BuildRunning 1 _ _ -> True; _ -> False) <$> readTVar (sessionState session)
    let next = case progress of
          HookRan | not startupBuildRuns -> Proven
          NotManaged | hookRan -> HookRan
          Managed | hookRan -> HookRan
          NotManaged -> Managed
          other -> other
    writeTVar (sessionOwner session) next
    pure (progress == HookRan && next == Proven)
  when proven (sessionVouch session)

closeBuildSession :: BuildSession -> IO ()
closeBuildSession session = mask_ (killThread (sessionThread session) >> readMVar (sessionDone session))

readyTarget :: BuildSession -> IO (Maybe ValidatedTarget)
readyTarget session =
  readTVarIO (sessionState session) >>= \case
    BuildReady _ target -> pure (Just target)
    _ -> pure Nothing

buildStatus :: BuildSession -> IO Value
buildStatus session = do
  image <- getExecutablePath
  current <- readTVarIO (sessionWarning session)
  status <- readStatus
  pure $ case status of
    Object fields -> Object (KeyMap.insert "warning" (toJSON current) (KeyMap.insert "active_image" (toJSON image) fields))
    other -> other
 where
  readStatus =
    readTVarIO (sessionState session) >>= \case
      BuildIdle -> pure (value "idle" 0 Nothing Nothing)
      BuildRunning generation purpose logPath -> pure (object ["log_path" .= logPath, "state" .= ("building" :: String), "generation" .= generation, "queued" .= if purpose == Superseded then Just (generation + 1) else Nothing])
      BuildFailed generation message logPath -> pure (value "failed" generation (Just (take 8192 message)) logPath)
      BuildReady generation target -> pure (value "validated" generation Nothing (Just (validatedLog target)))
      BuildUnchanged generation logPath -> pure (value "unchanged" generation Nothing (Just logPath))
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
recordRecoveryFailure session message =
  atomically $
    modifyTVar' (sessionState session) $ \case
      BuildIdle -> BuildFailed 0 message Nothing
      current -> current
