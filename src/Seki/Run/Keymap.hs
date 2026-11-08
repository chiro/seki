{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Seki.Run.Keymap (KeymapLoads, newKeymapLoads, loadKeymap, startKeymapLoad, drainKeymaps, closeKeymapLoads) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (IOException, onException, try)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Foreign.C
import Seki.Wire.Transport (OwnedFd, adoptFd, closeOwnedFd)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd (..))

-- Safe, so a file system that blocks open or read stalls only the loading thread.
foreign import ccall safe "seki_keymap_snapshot" snapshot :: CString -> IO CInt

newtype KeymapLoads token = KeymapLoads (MVar (Maybe [(token, Maybe OwnedFd)]))

newKeymapLoads :: IO (KeymapLoads token)
newKeymapLoads = KeymapLoads <$> newMVar (Just [])

loadKeymap :: FilePath -> IO (Maybe OwnedFd)
loadKeymap path = do
  fd <- withCString path snapshot
  if fd < 0 then pure Nothing else Just <$> (adoptFd (Fd fd) `onException` closeFd (Fd fd))

startKeymapLoad :: KeymapLoads token -> IO () -> token -> FilePath -> IO ()
startKeymapLoad (KeymapLoads results) wake token path = void . forkIO $ do
  loaded <- either (const Nothing) id <$> try @IOException (loadKeymap path)
  kept <- modifyMVar results $ \case
    Just pending -> pure (Just (pending <> [(token, loaded)]), True)
    Nothing -> pure (Nothing, False)
  if kept then wake else mapM_ (void . closeOwnedFd) loaded

drainKeymaps :: KeymapLoads token -> IO [(token, Maybe OwnedFd)]
drainKeymaps (KeymapLoads results) = modifyMVar results (pure . maybe (Nothing, []) (\pending -> (Just [], pending)))

closeKeymapLoads :: KeymapLoads token -> IO ()
closeKeymapLoads (KeymapLoads results) = do
  pending <- modifyMVar results (\current -> pure (Nothing, fromMaybe [] current))
  mapM_ (mapM_ (void . closeOwnedFd) . snd) pending
