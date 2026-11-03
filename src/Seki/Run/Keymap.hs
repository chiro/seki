{-# LANGUAGE ForeignFunctionInterface #-}

module Seki.Run.Keymap (KeymapWorker, newKeymapWorker, enqueueKeymap, drainKeymaps, closeKeymapWorker, cancelKeymaps, keymapWake) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (IOException, finally, mask_, onException, try)
import Control.Monad (void)
import Data.IORef
import Foreign (Ptr, alloca, peek)
import Foreign.C
import Seki.Run.External
import Seki.Wire.Transport (OwnedFd, adoptFd, closeOwnedFd)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd (..))

foreign import ccall unsafe "seki_keymap_start" start :: CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "seki_keymap_receive" receive :: CInt -> IO CInt
foreign import ccall unsafe "seki_keymap_reap" reap :: CInt -> CInt -> IO CInt

data WorkerState token = WorkerState Bool [(token, FilePath)] [(token, Maybe OwnedFd)] (Maybe (token, IORef Bool))
data KeymapWorker token = KeymapWorker (MVar (WorkerState token)) (MVar ()) (ExternalIngress ()) (MVar ())

keymapWake :: KeymapWorker token -> ExternalLifecycle
keymapWake (KeymapWorker _ _ ingress _) = externalLifecycle ingress

newKeymapWorker :: IO (KeymapWorker token)
newKeymapWorker = do
  state <- newMVar (WorkerState False [] [] Nothing)
  done <- newEmptyMVar
  ingress <- newExternalIngress
  signal <- newEmptyMVar
  let worker = KeymapWorker state done ingress signal
      loop = do
        next <- modifyMVar state $ \s@(WorkerState closed tasks results _) -> case (closed, tasks) of
          (True, _) -> pure (s, Left ())
          (False, []) -> pure (s, Right Nothing)
          (False, task@(token, _) : rest) -> do
            cancelled <- newIORef False
            pure (WorkerState False rest results (Just (token, cancelled)), Right (Just (task, cancelled)))
        case next of
          Left () -> pure ()
          Right Nothing -> takeMVar signal >> loop
          Right (Just ((token, path), cancelled)) -> do
            loaded <- try @IOException (load state cancelled path)
            let fd = either (const Nothing) id loaded
            retained <- modifyMVar state $ \(WorkerState closed tasks results _) ->
              pure (WorkerState closed tasks (if closed then results else results <> [(token, fd)]) Nothing, not closed)
            if retained then void (submitExternal (externalProducer ingress) ()) else mapM_ (void . closeOwnedFd) fd
            loop
  _ <- forkIO (loop `finally` putMVar done ())
  pure worker

enqueueKeymap :: KeymapWorker token -> token -> FilePath -> IO Bool
enqueueKeymap (KeymapWorker state _ _ signal) token path = mask_ $ do
  accepted <- modifyMVar state $ \s@(WorkerState closed tasks results active) ->
    if closed || length tasks + length results >= 32
      then pure (s, False)
      else pure (WorkerState False (tasks <> [(token, path)]) results active, True)
  if accepted then void (tryPutMVar signal ()) else pure ()
  pure accepted

drainKeymaps :: KeymapWorker token -> IO [(token, Maybe OwnedFd)]
drainKeymaps (KeymapWorker state _ ingress _) = do
  _ <- drainExternal ingress
  modifyMVar state $ \(WorkerState closed tasks results active) -> pure (WorkerState closed tasks [] active, results)

closeKeymapWorker :: KeymapWorker token -> IO ()
closeKeymapWorker worker@(KeymapWorker state done ingress signal) = mask_ $ do
  closeExternal (keymapWake worker)
  results <- modifyMVar state $ \(WorkerState _ _ results active) -> pure (WorkerState True [] [] active, results)
  mapM_ (mapM_ (void . closeOwnedFd) . snd) results
  void (tryPutMVar signal ())
  readMVar done
  closeExternal (externalLifecycle ingress)

load :: MVar (WorkerState token) -> IORef Bool -> FilePath -> IO (Maybe OwnedFd)
load state cancelled path = withCString path $ \name -> alloca $ \pidPtr -> do
  socket <- start name pidPtr
  if socket < 0
    then pure Nothing
    else do
      pid <- peek pidPtr
      let closed = do
            stopped <- readIORef cancelled
            withMVar state $ \(WorkerState value _ _ _) -> pure (value || stopped)
          finish = do
            stop <- closed
            result <- reap pid (if stop then 1 else 0)
            if result /= 0 then pure () else threadDelay 1000 >> finish
          poll = do
            stop <- closed
            if stop
              then pure Nothing
              else do
                result <- receive socket
                if result == -2
                  then threadDelay 1000 >> poll
                  else if result < 0 then pure Nothing else Just <$> (adoptFd (Fd result) `onException` closeFd (Fd result))
      poll `finally` (closeFd (Fd socket) >> finish)

cancelKeymaps :: KeymapWorker token -> (token -> Bool) -> IO ()
cancelKeymaps (KeymapWorker state _ ingress _) matches = mask_ $ do
  notify <- modifyMVar state $ \(WorkerState closed tasks results active) -> do
    mapM_ (\(token, flag) -> if matches token then writeIORef flag True else pure ()) active
    let removed = filter (matches . fst) tasks
        retained = filter (not . matches . fst) tasks
    pure (WorkerState closed retained (results <> [(token, Nothing) | (token, _) <- removed]) active, not (null removed))
  if notify then void (submitExternal (externalProducer ingress) ()) else pure ()
