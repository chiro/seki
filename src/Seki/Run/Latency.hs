module Seki.Run.Latency (
  SequenceKind (..),
  SequenceSample (..),
  Recording (..),
  Recorder,
  newRecorder,
  recordSuccess,
  recordFailure,
  readRecording,
) where

import Data.IORef
import Data.Word (Word64)

data SequenceKind = ManageSequence | RenderSequence deriving (Eq, Show)
data SequenceSample = SequenceSample
  { sampleKind :: !SequenceKind
  , sampleStartedNS :: !Word64
  , sampleSubmittedNS :: !Word64
  }
  deriving (Eq, Show)
data Recording = Recording
  { recordingSamples :: ![SequenceSample]
  , recordingSuccessful :: !Int
  , recordingFailed :: !Int
  , recordingDropped :: !Int
  }
  deriving (Eq, Show)
data Recorder = Recorder !Int !(IORef Recording)

newRecorder :: Int -> IO Recorder
newRecorder capacity
  | capacity <= 0 = ioError (userError "sequence recorder capacity must be positive")
  | otherwise = Recorder capacity <$> newIORef (Recording [] 0 0 0)

recordSuccess :: Recorder -> SequenceKind -> Word64 -> Word64 -> IO ()
recordSuccess recorder@(Recorder capacity ref) kind started submitted
  | submitted < started = recordFailure recorder
  | otherwise = atomicModifyIORef' ref $ \r ->
      let full = recordingSuccessful r >= capacity
          sample = SequenceSample kind started submitted
          next =
            r
              { recordingSamples = if full then recordingSamples r else sample : recordingSamples r
              , recordingSuccessful = recordingSuccessful r + 1
              , recordingDropped = recordingDropped r + if full then 1 else 0
              }
       in sample `seq` next `seq` (next, ())

recordFailure :: Recorder -> IO ()
recordFailure (Recorder _ ref) = atomicModifyIORef' ref $ \r ->
  let next = r{recordingFailed = recordingFailed r + 1} in next `seq` (next, ())

readRecording :: Recorder -> IO Recording
readRecording (Recorder _ ref) = do
  r <- readIORef ref
  pure r{recordingSamples = reverse (recordingSamples r)}
