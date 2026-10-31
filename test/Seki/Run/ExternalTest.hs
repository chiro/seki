module Seki.Run.ExternalTest (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), finally, throwIO, try)
import Control.Monad (replicateM)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Seki.Run.External
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
    ]
