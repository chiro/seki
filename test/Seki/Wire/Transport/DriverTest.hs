{-# LANGUAGE TypeApplications #-}

module Seki.Wire.Transport.DriverTest (tests) where

import Control.Exception (IOException, throwIO, try)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Seki.Wire.Transport.Driver (
  ReceiveAttempt (..),
  SendAttempt (..),
  SendDriverError (..),
  driveReceive,
  driveSend,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland transport retry drivers"
    [ testCase "sends a partial prefix with descriptors only once" testPartialWrite
    , testCase "retries interruption without disposing descriptors" testInterruptedWrite
    , testCase "waits for writable after backpressure" testBlockedWrite
    , testCase "disposes descriptors after zero progress" testZeroProgress
    , testCase "disposes descriptors after syscall failure" testSendFailure
    , testCase "disposes descriptors after exceptions" testSendException
    , testCase "rejects impossible syscall progress" testInvalidProgress
    , testCase "retries interrupted and blocked receives" testReceiveRetries
    , testCase "repeated failures dispose each descriptor exactly once" testRepeatedFailureCleanup
    ]

testPartialWrite :: IO ()
testPartialWrite = do
  attempts <- newIORef ([] :: [(Int, [Int])])
  outcomes <- newIORef ([SendProgress 2, SendProgress 3] :: [SendAttempt String])
  disposals <- newDisposals
  result <-
    driveSend
      (ByteString.pack [1, 2, 3, 4, 5])
      [10, 20]
      (scriptedSend attempts outcomes)
      (pure ())
      (writeDisposal disposals)
  assertEqual "result" (Right ()) result
  assertEqual "attempts" [(5, [10, 20]), (3, [])] =<< readIORef attempts
  assertEqual "disposed once" [[10, 20]] =<< readIORef disposals

testInterruptedWrite :: IO ()
testInterruptedWrite = do
  attempts <- newIORef ([] :: [(Int, [Int])])
  outcomes <- newIORef ([SendInterrupted, SendProgress 1] :: [SendAttempt String])
  disposals <- newDisposals
  result <- driveSend (ByteString.singleton 1) [7] (scriptedSend attempts outcomes) (pure ()) (writeDisposal disposals)
  assertEqual "result" (Right ()) result
  assertEqual "descriptor repeated" [(1, [7]), (1, [7])] =<< readIORef attempts
  assertEqual "disposed once" [[7]] =<< readIORef disposals

testBlockedWrite :: IO ()
testBlockedWrite = do
  outcomes <- newIORef ([SendWouldBlock, SendProgress 1] :: [SendAttempt String])
  waits <- newIORef (0 :: Int)
  result <-
    driveSend
      (ByteString.singleton 1)
      ([] :: [Int])
      (scriptedSendDiscardingCalls outcomes)
      (modifyIORef' waits (+ 1))
      (const (pure ()))
  assertEqual "result" (Right ()) result
  assertEqual "one wait" 1 =<< readIORef waits

testZeroProgress :: IO ()
testZeroProgress = do
  disposals <- newDisposals
  result <- driveSend (ByteString.singleton 1) [3] (constantSend (SendProgress 0 :: SendAttempt String)) (pure ()) (writeDisposal disposals)
  assertEqual "no progress" (Left SendNoProgress) result
  assertEqual "disposed" [[3]] =<< readIORef disposals

testSendFailure :: IO ()
testSendFailure = do
  disposals <- newDisposals
  result <- driveSend (ByteString.singleton 1) [4] (constantSend (SendFailed "EPIPE")) (pure ()) (writeDisposal disposals)
  assertEqual "failure" (Left (SendDriverFailed "EPIPE")) result
  assertEqual "disposed" [[4]] =<< readIORef disposals

testSendException :: IO ()
testSendException = do
  disposals <- newDisposals
  result <-
    try @IOException
      ( driveSend
          (ByteString.singleton 1)
          [5]
          (\_ _ -> throwIO (userError "injected send failure") :: IO (SendAttempt String))
          (pure ())
          (writeDisposal disposals)
      )
  case result of
    Left _ -> pure ()
    Right value -> fail ("expected exception, got " <> show value)
  assertEqual "disposed" [[5]] =<< readIORef disposals

testInvalidProgress :: IO ()
testInvalidProgress = do
  disposals <- newDisposals
  result <- driveSend (ByteString.replicate 2 0) [6] (constantSend (SendProgress 3 :: SendAttempt String)) (pure ()) (writeDisposal disposals)
  assertEqual "invalid count" (Left (SendInvalidProgress 3 2)) result
  assertEqual "disposed" [[6]] =<< readIORef disposals

testReceiveRetries :: IO ()
testReceiveRetries = do
  outcomes <- newIORef [ReceiveInterrupted, ReceiveWouldBlock, ReceiveProgress "chunk"]
  waits <- newIORef (0 :: Int)
  result <- driveReceive (nextOutcome outcomes) (modifyIORef' waits (+ 1))
  assertEqual "result" (Right "chunk" :: Either String String) result
  assertEqual "one wait" 1 =<< readIORef waits

testRepeatedFailureCleanup :: IO ()
testRepeatedFailureCleanup = do
  disposals <- newDisposals
  results <-
    mapM
      ( \descriptor ->
          driveSend
            (ByteString.singleton 0)
            [descriptor]
            (constantSend (SendFailed ()))
            (pure ())
            (writeDisposal disposals)
      )
      [1 .. 100 :: Int]
  assertEqual "all failed" (replicate 100 (Left (SendDriverFailed ()))) results
  assertEqual "one disposal per descriptor" (map pure [1 .. 100]) =<< readIORef disposals

scriptedSend ::
  IORef [(Int, [descriptor])] ->
  IORef [SendAttempt errorValue] ->
  ByteString.ByteString ->
  [descriptor] ->
  IO (SendAttempt errorValue)
scriptedSend attempts outcomes bytes descriptors = do
  modifyIORef' attempts (<> [(ByteString.length bytes, descriptors)])
  nextOutcome outcomes

scriptedSendDiscardingCalls ::
  IORef [SendAttempt errorValue] ->
  ByteString.ByteString ->
  [descriptor] ->
  IO (SendAttempt errorValue)
scriptedSendDiscardingCalls outcomes _ _ = nextOutcome outcomes

constantSend :: SendAttempt errorValue -> ByteString.ByteString -> [descriptor] -> IO (SendAttempt errorValue)
constantSend outcome _ _ = pure outcome

nextOutcome :: IORef [outcome] -> IO outcome
nextOutcome outcomes = do
  remaining <- readIORef outcomes
  case remaining of
    [] -> fail "scripted syscall ran out of outcomes"
    outcome : rest -> do
      modifyIORef' outcomes (const rest)
      pure outcome

writeDisposal :: IORef [[descriptor]] -> [descriptor] -> IO ()
writeDisposal disposals descriptors = modifyIORef' disposals (<> [descriptors])

newDisposals :: IO (IORef [[Int]])
newDisposals = newIORef []
