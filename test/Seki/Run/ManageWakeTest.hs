module Seki.Run.ManageWakeTest (tests) where

import Control.Concurrent (forkFinally, killThread, yield)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, tryPutTMVar)
import Control.Exception (finally, throwIO)
import Control.Monad (replicateM_, void)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Seki.Internal.Core.Action qualified as Action
import Seki.Internal.Core.State (newCoreState)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Run.External qualified as External
import Seki.Run.IPC (CommandResult (..), submitCommand)
import Seki.Run.ManageWake
import Seki.Run.Policy (ExternalInput (..))
import Seki.Wire.Connection (ConnectionError (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "runtime manage wake"
    [ testCase "startup gate retains a signal without sending" $ do
        sent <- newEmptyMVar
        wake <- newManageWake (putMVar sent () >> pure (Right ())) unexpected
        flip finally (abortManageWake wake) $ do
          void (signalManageWake wake)
          tryReadMVar sent >>= assertEqual "no send before ownership is installed" Nothing
          startManageWake wake
          within "startup signal sent" (takeMVar sent)
          quiesceManageWake wake
    , testCase "abort joins a worker before its startup gate opens" $ do
        wake <- newManageWake (assertFailure "send before startup" >> pure (Right ())) unexpected
        void (signalManageWake wake)
        within "abort before start" (abortManageWake wake)
        startManageWake wake
        void (signalManageWake wake)
        within "repeated abort" (abortManageWake wake)
    , testCase "blocked sends coalesce a burst into one pending signal" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        count <- newIORef (0 :: Int)
        let send = do
              atomicModifyIORef' count (\n -> (n + 1, ()))
              putMVar entered ()
              takeMVar release
              pure (Right ())
        wake <- newManageWake send unexpected
        flip finally (abortManageWake wake) $ do
          startManageWake wake
          void (signalManageWake wake)
          within "first send" (takeMVar entered)
          replicateM_ 256 (signalManageWake wake)
          putMVar release ()
          within "one coalesced send" (takeMVar entered)
          putMVar release ()
          quiesceManageWake wake
          readIORef count >>= assertEqual "one in flight and one pending" 2
          void (signalManageWake wake)
          readIORef count >>= assertEqual "quiesced signals cannot restart worker" 2
    , testCase "graceful stop waits for its send and discards a redundant signal" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        count <- newIORef (0 :: Int)
        errors <- newIORef []
        wake <- newManageWake (atomicModifyIORef' count (\n -> (n + 1, ())) >> putMVar entered () >> takeMVar release >> pure (Right ())) (\e -> atomicModifyIORef' errors (\es -> (e : es, ())))
        flip finally (abortManageWake wake) $ do
          startManageWake wake
          void (signalManageWake wake)
          within "send entered" (takeMVar entered)
          void (signalManageWake wake)
          stopped <- newEmptyMVar
          _ <- forkFinally (quiesceManageWake wake) (putMVar stopped)
          timeout 20000 (readMVar stopped) >>= assertBool "graceful stop still waits" . maybe True (const False)
          putMVar release ()
          within "graceful join" (takeMVar stopped) >>= either (assertFailure . show) pure
          readIORef count >>= assertEqual "pending signal discarded" 1
          readIORef errors >>= assertEqual "normal stop did not interrupt transport" []
    , testCase "concurrent abort joins an interruptible send without reporting cancellation" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        errors <- newIORef []
        wake <- newManageWake (putMVar entered () >> takeMVar release >> pure (Right ())) (\e -> atomicModifyIORef' errors (\es -> (e : es, ())))
        startManageWake wake
        void (signalManageWake wake)
        within "send entered" (takeMVar entered)
        stops <- mapM (const newEmptyMVar) [1 .. 8 :: Int]
        mapM_ (\done -> forkFinally (abortManageWake wake) (putMVar done)) stops
        mapM_ (\done -> within "concurrent abort joined" (takeMVar done) >>= either (assertFailure . show) pure) stops
        readIORef errors >>= assertEqual "expected cancellation is silent" []
    , testCase "IPC handler cancellation preserves admitted work behind a blocked wake" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        queue <- External.newExternalIngress
        wake <- newManageWake (void (tryPutMVar entered ()) >> readMVar release >> pure (Right ())) unexpected
        flip finally (External.closeExternal (External.externalLifecycle queue) >> abortManageWake wake) $ do
          External.bindExternalWake (External.externalLifecycle queue) (signalManageWake wake) >>= assertBool "bound ingress"
          startManageWake wake
          abandoned <- newEmptyMVar
          handler <- forkFinally (submitCommand (External.externalProducer queue) (Action.spawn "first")) (putMVar abandoned)
          within "wake blocked after first admission" (takeMVar entered)
          killThread handler
          within "first handler cancelled" (takeMVar abandoned) >>= assertBool "cancellation reached handler" . either (const True) (const False)
          answer <- newEmptyMVar
          _ <- forkFinally (submitCommand (External.externalProducer queue) (Action.spawn "second")) (putMVar answer)
          let collect count inputs
                | length inputs == count = pure inputs
                | otherwise = do
                    batch <- External.drainedInputs <$> External.drainExternal queue
                    yield
                    collect count (inputs <> batch)
              apply revision (ExternalInput action receipt) = do
                state <- either (assertFailure . show) pure (newCoreState Full [1 :: Int])
                let (_, _, effects) = Action.runAction action state
                mapM_ (\cell -> atomically (void (tryPutTMVar cell revision))) receipt
                pure effects
          inputs <- within "both commands admitted while wake blocked" (collect 2 [])
          effects <- sequence (zipWith apply [1 ..] inputs)
          assertEqual "cancelled caller's work and later work execute once in FIFO" [[Action.Spawn "first"], [Action.Spawn "second"]] effects
          within "second command applied" (takeMVar answer) >>= either (assertFailure . show) (assertEqual "later receipt succeeds" (CommandApplied 2))
          putMVar release ()
          later <- newEmptyMVar
          _ <- forkFinally (submitCommand (External.externalProducer queue) (Action.spawn "third")) (putMVar later)
          [input] <- within "fresh command admitted" (collect 1 [])
          _ <- apply 3 input
          within "fresh receipt" (takeMVar later) >>= either (assertFailure . show) (assertEqual "continued service" (CommandApplied 3))
          quiesceManageWake wake
    , testCase "transport failure closes admission and reports the runtime error" $ failureCase (pure (Left ConnectionClosed)) ConnectionClosed
    , testCase "unexpected worker exception closes admission" $ failureCase (throwIO (userError "wake failed")) (ConnectionUnexpectedError "user error (wake failed)")
    ]
 where
  unexpected e = assertFailure ("unexpected wake failure: " <> show e)
  failureCase send expected = do
    queue <- External.newExternalIngress
    failed <- newEmptyMVar
    wake <- newManageWake send (\e -> External.closeExternal (External.externalLifecycle queue) >> putMVar failed e)
    flip finally (abortManageWake wake) $ do
      External.bindExternalWake (External.externalLifecycle queue) (signalManageWake wake) >>= assertBool "bound ingress"
      startManageWake wake
      External.submitExternal (External.externalProducer queue) () >>= assertEqual "initial admission" External.Accepted
      actual <- within "failure observed" (takeMVar failed)
      case (expected, actual) of
        (ConnectionUnexpectedError prefix, ConnectionUnexpectedError message) -> assertBool "runtime exception retained" (prefix `isPrefixOf` message)
        _ -> assertEqual "runtime error" expected actual
      External.submitExternal (External.externalProducer queue) () >>= assertEqual "failure closed admission" External.IngressClosed
      quiesceManageWake wake

within :: String -> IO a -> IO a
within description action = timeout 2000000 action >>= maybe (assertFailure (description <> " timed out")) pure
