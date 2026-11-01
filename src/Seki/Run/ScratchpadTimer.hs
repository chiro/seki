{- | One session-owned timer worker, bounded by configured names. Notifications
use the same bounded ingress as IPC and retry admission when it is full.
-}
module Seki.Run.ScratchpadTimer (ScratchpadTimer, newScratchpadTimer, newScratchpadTimerWithDelay, scheduleScratchpad, closeScratchpadTimer) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Run.External (Admission (..))

data TimerState = TimerState Bool (Maybe ThreadId) (Map.Map String (Integer, Word64))
data ScratchpadTimer = ScratchpadTimer Word64 (MVar TimerState)

newScratchpadTimer :: IO ScratchpadTimer
newScratchpadTimer = newScratchpadTimerWithDelay 10000000000

-- | Internal test seam; production always uses the ten second deadline.
newScratchpadTimerWithDelay :: Word64 -> IO ScratchpadTimer
newScratchpadTimerWithDelay delay = ScratchpadTimer delay <$> newMVar (TimerState False Nothing Map.empty)

scheduleScratchpad :: ScratchpadTimer -> (String -> Integer -> IO Admission) -> String -> Integer -> IO ()
scheduleScratchpad (ScratchpadTimer delay cell) deliver name generation = do
  now <- getMonotonicTimeNSec
  modifyMVar_ cell $ \st@(TimerState closed worker deadlines) ->
    if closed
      then pure st
      else do
        running <- case worker of
          Just tid -> pure tid
          Nothing -> forkIO loop
        pure (TimerState False (Just running) (Map.insert name (generation, now + delay) deadlines))
 where
  loop = do
    threadDelay 100000
    now <- getMonotonicTimeNSec
    due <- withMVar cell $ \(TimerState closed _ deadlines) ->
      pure (if closed then Map.empty else Map.filter ((<= now) . snd) deadlines)
    outcomes <- Map.traverseWithKey (\key (token, _) -> deliver key token) due
    again <- modifyMVar cell $ \st@(TimerState closed worker deadlines) ->
      if closed
        then pure (st, False)
        else do
          let retire key entry = case (Map.lookup key due, Map.lookup key outcomes) of
                (Just attempted, Just admitted) | attempted == entry && admitted /= Full -> Nothing
                _ -> Just entry
              remaining = Map.mapMaybeWithKey retire deadlines
          pure (TimerState False (if Map.null remaining then Nothing else worker) remaining, not (Map.null remaining))
    if again then loop else pure ()

closeScratchpadTimer :: ScratchpadTimer -> IO ()
closeScratchpadTimer (ScratchpadTimer _ cell) = do
  worker <- modifyMVar cell $ \(TimerState _ tid _) -> pure (TimerState True Nothing Map.empty, tid)
  void (traverse killThread worker)
