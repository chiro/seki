module Seki.Run.ExternalTest (tests) where

import Control.Concurrent.STM (atomically, tryTakeTMVar)
import Control.Monad (replicateM)
import Seki.Run.External
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "external ingress"
    [ testCase "finite saturated burst preserves FIFO and makes bounded progress" $ do
        queue <- newExternalIngress
        mapM (submitExternal queue) [1 .. 256 :: Int] >>= assertEqual "accepted" (replicate 256 Accepted)
        submitExternal queue 257 >>= assertEqual "overload" Full
        woken queue >>= assertEqual "admission woke the owner once" True
        batches <- replicateM 8 $ do
          batch <- drainExternal queue
          awake <- woken queue
          pure (batch, awake)
        assertEqual "FIFO without loss or duplicates" [1 .. 256] (concatMap fst batches)
        assertEqual "batch cap" (replicate 8 32) (map (length . fst) batches)
        assertEqual "owner stays awake until the final batch" (replicate 7 True <> [False]) (map snd batches)
        submitExternal queue 257 >>= assertEqual "enqueue after final drain" Accepted
        woken queue >>= assertEqual "final drain rearmed" True
    , testCase "signal wakes the owner without queueing input" $ do
        queue <- newExternalIngress
        signalExternal queue
        woken queue >>= assertEqual "empty queue still wakes" True
        drainExternal queue >>= assertEqual "nothing queued" ([] :: [Int])
        mapM_ (submitExternal queue) [1 .. 256]
        submitExternal queue 257 >>= assertEqual "queue is full" Full
        _ <- woken queue
        signalExternal queue
        woken queue >>= assertEqual "full queue still wakes" True
        closeIngress (ingressHandle queue)
        signalExternal queue
        woken queue >>= assertEqual "closed ingress does not wake" False
    , testCase "close discards pending input and isolates a retained ingress" $ do
        old <- newExternalIngress
        submitExternal old () >>= assertEqual "accepted" Accepted
        closeIngress (ingressHandle old)
        submitExternal old () >>= assertEqual "old ingress refused" IngressClosed
        drainExternal old >>= assertEqual "closed discards pending" []
        fresh <- newExternalIngress
        submitExternal fresh () >>= assertEqual "fresh works" Accepted
    , testCase "seal waits for accepted inputs and rejects later admission" $ do
        queue <- newExternalIngress
        submitExternal queue (1 :: Int) >>= assertEqual "accepted" Accepted
        sealExternal queue >>= assertEqual "pending input blocks the seal" False
        drainExternal queue >>= assertEqual "accepted input still drains" [1]
        sealExternal queue >>= assertEqual "empty queue seals" True
        submitExternal queue 2 >>= assertEqual "sealed ingress refuses" IngressClosed
        sealExternal queue >>= assertEqual "a closed ingress does not seal again" False
    ]

woken :: ExternalIngress a -> IO Bool
woken queue = maybe False (const True) <$> atomically (tryTakeTMVar (ingressWake (ingressHandle queue)))
