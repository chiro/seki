module Seki.Run.ExternalTest (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception (ErrorCall (..), finally, throwIO, try)
import Control.Monad (replicateM, void)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import GHC.Conc (ThreadStatus (..), threadStatus)
import Seki.Run.External
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "external ingress"
    [ testCase "finite saturated burst preserves FIFO and makes bounded progress" $ do
        queue <- newExternalIngress
        wakes <- newIORef (0 :: Int)
        let lifecycle = externalLifecycle queue
            producer = externalProducer queue
        bindExternalWake lifecycle (modifyIORef' wakes (+ 1) >> pure True) >>= assertBool "bound"
        mapM (submitExternal producer) [1 .. 256 :: Int] >>= assertEqual "accepted" (replicate 256 Accepted)
        submitExternal producer 257 >>= assertEqual "overload" Full
        batches <- replicateM 8 $ do
          batch <- drainExternal queue
          wakeExternalPending lifecycle
          pure batch
        assertEqual "FIFO without loss or duplicates" [1 .. 256] (concatMap drainedInputs batches)
        assertEqual "batch cap" (replicate 8 32) (map (length . drainedInputs) batches)
        readIORef wakes >>= assertEqual "coalesced admission plus leftover wakes" 8
        submitExternal producer 257 >>= assertEqual "enqueue after final drain" Accepted
        readIORef wakes >>= assertEqual "final drain rearmed" 9
    , testCase "close and fresh session isolate retained producer" $ do
        old <- newExternalIngress
        let producer = externalProducer old
            lifecycle = externalLifecycle old
        submitExternal producer () >>= assertEqual "unbound refuses" IngressClosed
        bindExternalWake lifecycle (pure True) >>= assertBool "bound"
        submitExternal producer () >>= assertEqual "accepted" Accepted
        closeExternal lifecycle
        bindExternalWake lifecycle (pure True) >>= assertEqual "cannot rebind" False
        fresh <- newExternalIngress
        bindExternalWake (externalLifecycle fresh) (pure True) >>= assertBool "fresh bound"
        submitExternal producer () >>= assertEqual "old token refused" IngressClosed
        drainExternal old >>= assertEqual "closed discards pending" (Drain [] False)
        submitExternal (externalProducer fresh) () >>= assertEqual "fresh works" Accepted
    , testCase "failed or exceptional wake closes admission" $ do
        queue <- newExternalIngress
        bindExternalWake (externalLifecycle queue) (throwIO (ErrorCall "cancelled")) >>= assertBool "bound"
        result <- try @ErrorCall (submitExternal (externalProducer queue) ())
        case result of Left _ -> pure (); Right _ -> assertFailure "exception lost"
        submitExternal (externalProducer queue) () >>= assertEqual "closed after exception" IngressClosed
    , testCase "cancelling the wake owner closes pending admission" $ do
        queue <- newExternalIngress
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        done <- newEmptyMVar
        bindExternalWake (externalLifecycle queue) (putMVar entered () >> takeMVar blocked) >>= assertBool "bound"
        worker <- forkIO ((submitExternal (externalProducer queue) () >> pure ()) `finally` putMVar done ())
        takeMVar entered
        killThread worker
        takeMVar done
        submitExternal (externalProducer queue) () >>= assertEqual "cancelled owner closes" IngressClosed
    , testCase "transport failure explicitly closes admission" $ do
        queue <- newExternalIngress
        bindExternalWake (externalLifecycle queue) (pure False) >>= assertBool "bound"
        submitExternal (externalProducer queue) () >>= assertEqual "send failed" IngressClosed
        drainExternal queue >>= assertEqual "no stranded work" (Drain [] False)
    , testCase "pending-wake refusal discards leftovers" $ pendingWakeFailure (pure False)
    , testCase "pending-wake exception discards leftovers" $ pendingWakeFailure (throwIO (ErrorCall "pending wake failed"))
    , testCase "pending-wake owner cancellation closes ingress" $ do
        queue <- newExternalIngress
        notify <- newIORef (pure True)
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        done <- newEmptyMVar
        let lifecycle = externalLifecycle queue
            producer = externalProducer queue
        bindExternalWake lifecycle (readIORef notify >>= id) >>= assertBool "bound"
        mapM_ (submitExternal producer) [1 .. 40 :: Int]
        _ <- drainExternal queue
        writeIORef notify (putMVar entered () >> takeMVar blocked)
        owner <- forkIO (wakeExternalPending lifecycle `finally` putMVar done ())
        takeMVar entered
        killThread owner
        timeout 1000000 (takeMVar done) >>= assertEqual "owner stopped" (Just ())
        drainExternal queue >>= assertEqual "leftovers discarded" (Drain [] False)
        submitExternal producer 41 >>= assertEqual "closed" IngressClosed
    , testCase "cancelling a submitting waiter preserves accepted work" $
        cancelledWaiterPreservesIngress (\producer _ -> void (submitExternal producer 2))
    , testCase "cancelling a pending-wake waiter preserves accepted work" $
        cancelledWaiterPreservesIngress (\_ lifecycle -> wakeExternalPending lifecycle)
    ]

cancelledWaiterPreservesIngress :: (ExternalProducer Int -> ExternalLifecycle -> IO ()) -> IO ()
cancelledWaiterPreservesIngress operation = do
  queue <- newExternalIngress
  entered <- newEmptyMVar
  release <- newEmptyMVar
  ownerDone <- newEmptyMVar
  waiterDone <- newEmptyMVar
  let lifecycle = externalLifecycle queue
      producer = externalProducer queue
      notify = void (tryPutMVar entered ()) >> readMVar release >> pure True
  bindExternalWake lifecycle notify >>= assertBool "bound"
  void (forkIO (submitExternal producer 1 >>= putMVar ownerDone))
  takeMVar entered
  waiter <- forkIO (operation producer lifecycle `finally` putMVar waiterDone ())
  let waitBlocked =
        threadStatus waiter >>= \status -> case status of
          ThreadBlocked _ -> pure ()
          _ -> threadDelay 1000 >> waitBlocked
  blocked <- timeout 1000000 waitBlocked
  cancelled <- timeout 100000 (killThread waiter >> takeMVar waiterDone)
  putMVar release ()
  first <- takeMVar ownerDone
  case cancelled of
    Nothing -> takeMVar waiterDone
    Just () -> pure ()
  assertEqual "waiter reached the blocked operation" (Just ()) blocked
  assertEqual "wake owner accepted its input" Accepted first
  drainExternal queue >>= assertEqual "accepted work survives unrelated cancellation" (Drain [1] False)
  submitExternal producer 3 >>= assertEqual "ingress remains usable" Accepted
  assertEqual "cancellation completes without waiting for another producer" (Just ()) cancelled

pendingWakeFailure :: IO Bool -> IO ()
pendingWakeFailure failure = do
  queue <- newExternalIngress
  notify <- newIORef (pure True)
  let lifecycle = externalLifecycle queue
      producer = externalProducer queue
  bindExternalWake lifecycle (readIORef notify >>= id) >>= assertBool "bound"
  mapM_ (submitExternal producer) [1 .. 40 :: Int]
  _ <- drainExternal queue
  writeIORef notify failure
  _ <- try @ErrorCall (wakeExternalPending lifecycle)
  drainExternal queue >>= assertEqual "leftovers discarded" (Drain [] False)
  submitExternal producer 41 >>= assertEqual "closed" IngressClosed
