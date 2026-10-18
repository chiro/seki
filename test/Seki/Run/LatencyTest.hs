module Seki.Run.LatencyTest (tests) where

import Control.Exception (IOException, try)
import Seki.Run.Latency
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "sequence latency accounting"
    [ testCase "retains ordered exact timestamps for both sequence kinds" $ do
        recorder <- newRecorder 3
        recordSuccess recorder ManageSequence 20 29
        recordSuccess recorder RenderSequence 30 40
        r <- readRecording recorder
        recordingSamples r @?= [SequenceSample ManageSequence 20 29, SequenceSample RenderSequence 30 40]
        (recordingSuccessful r, recordingFailed r, recordingDropped r) @?= (2, 0, 0)
    , testCase "overflow retains bounded samples and counts every successful submission" $ do
        recorder <- newRecorder 1
        recordSuccess recorder ManageSequence 20 29
        recordSuccess recorder RenderSequence 30 40
        r <- readRecording recorder
        recordingSamples r @?= [SequenceSample ManageSequence 20 29]
        (recordingSuccessful r, recordingDropped r) @?= (2, 1)
    , testCase "failures and reversed clocks cannot contaminate success samples" $ do
        recorder <- newRecorder 3
        recordFailure recorder
        recordSuccess recorder ManageSequence 30 20
        r <- readRecording recorder
        recordingSamples r @?= []
        (recordingSuccessful r, recordingFailed r) @?= (0, 2)
    , testCase "zero capacity rejected" $ do
        result <- try @IOException (newRecorder 0)
        case result of Left _ -> pure (); Right _ -> assertFailure "accepted zero capacity"
    ]
