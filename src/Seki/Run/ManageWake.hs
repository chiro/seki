{-# LANGUAGE LambdaCase #-}

-- | The runtime's single transport sender for coalesced manage requests.
module Seki.Run.ManageWake (
  ManageWake,
  newManageWake,
  startManageWake,
  signalManageWake,
  quiesceManageWake,
  abortManageWake,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, finally, mask_, try)
import Control.Monad (unless, void, when)
import Seki.Wire.Connection (ConnectionError (..))

data WakePhase = Starting | Running | Quiescing deriving (Eq)

data ManageWake = ManageWake
  { wakePhase :: TVar WakePhase
  , wakePending :: TMVar ()
  , wakeThread :: ThreadId
  , wakeDone :: MVar ()
  , wakeCancelled :: MVar Bool
  }

{- | The worker cannot send until startup has installed its ownership cleanup.
It takes tokens from @pending@, which an ingress may also fill directly.
-}
newManageWake :: TMVar () -> IO (Either ConnectionError ()) -> (ConnectionError -> IO ()) -> IO ManageWake
newManageWake pending send failed = mask_ $ do
  phase <- newTVarIO Starting
  done <- newEmptyMVar
  cancelled <- newMVar False
  let next =
        atomically $
          readTVar phase >>= \case
            Starting -> retry
            Running -> takeTMVar pending >> pure True
            Quiescing -> pure False
      report errorValue = do
        atomically $ writeTVar phase Quiescing >> void (tryTakeTMVar pending)
        failed errorValue
      loop =
        next >>= \case
          False -> pure (Right ())
          True -> send >>= either (pure . Left) (const loop)
      finish unmask = do
        result <- try @SomeException (unmask loop)
        case result of
          Left e -> do
            stopping <- atomically ((== Quiescing) <$> readTVar phase)
            unless stopping (report (ConnectionUnexpectedError (displayException e)))
          Right (Left errorValue) -> report errorValue
          Right (Right ()) -> pure ()
  thread <- forkIOWithUnmask $ \unmask -> finish unmask `finally` putMVar done ()
  pure (ManageWake phase pending thread done cancelled)

startManageWake :: ManageWake -> IO ()
startManageWake wake = atomically $ do
  phase <- readTVar (wakePhase wake)
  when (phase == Starting) (writeTVar (wakePhase wake) Running)

-- | Store at most one unit signal, with no transport or acknowledgement wait.
signalManageWake :: ManageWake -> IO Bool
signalManageWake wake = atomically $ do
  phase <- readTVar (wakePhase wake)
  unless (phase == Quiescing) (void (tryPutTMVar (wakePending wake) ()))
  pure True

disableManageWake :: ManageWake -> IO ()
disableManageWake wake = atomically $ do
  writeTVar (wakePhase wake) Quiescing
  void (tryTakeTMVar (wakePending wake))

-- | Normal stop waits for any in-flight send without interrupting the connection.
quiesceManageWake :: ManageWake -> IO ()
quiesceManageWake wake = disableManageWake wake >> readMVar (wakeDone wake)

-- | Abort interrupts a blocked send and joins before the connection is closed.
abortManageWake :: ManageWake -> IO ()
abortManageWake wake = mask_ $ do
  disableManageWake wake
  modifyMVar_ (wakeCancelled wake) $ \cancelled -> do
    unless cancelled (killThread (wakeThread wake))
    pure True
  readMVar (wakeDone wake)
