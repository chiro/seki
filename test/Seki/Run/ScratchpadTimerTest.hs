module Seki.Run.ScratchpadTimerTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.IORef
import Seki.Run.External (Admission (..))
import Seki.Run.ScratchpadTimer
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

await :: IO Bool -> IO ()
await predicate = do
  completed <- timeout 2000000 loop
  assertEqual "timer completion within test bound" (Just ()) completed
 where
  loop = predicate >>= \ready -> if ready then pure () else threadDelay 1000 >> loop

tests :: TestTree
tests =
  testGroup
    "Scratchpad session timer"
    [ testCase "full ingress retries; one accepted delivery" $ do
        timer <- newScratchpadTimerWithDelay 1
        attempts <- newIORef (0 :: Int)
        delivered <- newIORef []
        let notify name generation = do
              n <- atomicModifyIORef' attempts (\n -> (n + 1, n))
              if n == 0 then pure Full else modifyIORef' delivered (<> [(name, generation)]) >> pure Accepted
        ( do
            scheduleScratchpad timer notify "term" 1
            await ((== [("term", 1)]) <$> readIORef delivered)
            threadDelay 150000
            readIORef delivered >>= (@?= [("term", 1)])
          )
          `finally` closeScratchpadTimer timer
    , testCase "replacement keeps only current generation; close stops producer" $ do
        timer <- newScratchpadTimerWithDelay 100000000
        delivered <- newIORef []
        let notify name generation = modifyIORef' delivered (<> [(name, generation)]) >> pure Accepted
        scheduleScratchpad timer notify "term" 1
        scheduleScratchpad timer notify "term" 2
        await ((== [("term", 2)]) <$> readIORef delivered)
        scheduleScratchpad timer notify "term" 3
        closeScratchpadTimer timer
        scheduleScratchpad timer notify "term" 4
        threadDelay 250000
        readIORef delivered >>= (@?= [("term", 2)])
    , testCase "closed ingress retires deadline without retries" $ do
        timer <- newScratchpadTimerWithDelay 1
        attempts <- newIORef (0 :: Int)
        let notify _ _ = modifyIORef' attempts (+ 1) >> pure IngressClosed
        ( do
            scheduleScratchpad timer notify "term" 1
            await ((== 1) <$> readIORef attempts)
            threadDelay 200000
            readIORef attempts >>= (@?= 1)
          )
          `finally` closeScratchpadTimer timer
    ]
