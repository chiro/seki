module Seki.Run.External (
  ExternalIngress,
  ExternalProducer,
  ExternalLifecycle,
  Admission (..),
  Drain (..),
  newExternalIngress,
  externalProducer,
  externalLifecycle,
  submitExternal,
  drainExternal,
  bindExternalWake,
  wakeExternalPending,
  closeExternal,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (onException)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq

newtype ExternalIngress a = ExternalIngress (MVar (IngressState a))
newtype ExternalProducer a = ExternalProducer (ExternalIngress a)

data IngressState a = Dormant | Accepting (IO Bool) (Seq a) | Closed

data Admission = Accepted | Full | IngressClosed deriving (Eq, Show)
data Drain a = Drain {drainedInputs :: [a], drainHasMore :: Bool} deriving (Eq, Show)

data ExternalLifecycle = ExternalLifecycle
  { bindExternalWake :: IO Bool -> IO Bool
  , wakeExternalPending :: IO ()
  , closeExternal :: IO ()
  }

newExternalIngress :: IO (ExternalIngress a)
newExternalIngress = ExternalIngress <$> newMVar Dormant

externalProducer :: ExternalIngress a -> ExternalProducer a
externalProducer = ExternalProducer

externalLifecycle :: ExternalIngress a -> ExternalLifecycle
externalLifecycle (ExternalIngress state) = ExternalLifecycle bind wake close
 where
  bind notify = modifyMVar state $ \current -> case current of
    Dormant -> pure (Accepting notify Seq.empty, True)
    _ -> pure (current, False)
  close = modifyMVar state (const (pure (Closed, ())))
  wake = wakePending `onException` close
  wakePending = modifyMVar state $ \current -> case current of
    Accepting notify pending | not (Seq.null pending) -> do
      ok <- notify
      pure (if ok then current else Closed, ())
    _ -> pure (current, ())

-- | Accepted means queued, not applied. Session closure discards pending inputs.
submitExternal :: ExternalProducer a -> a -> IO Admission
submitExternal (ExternalProducer (ExternalIngress state)) input =
  -- A cancelled send must not restore a queue whose only wake was abandoned.
  modifyMVar state admit `onException` modifyMVar state (const (pure (Closed, ())))
 where
  admit current = case current of
    Accepting notify pending
      | Seq.length pending >= 256 -> pure (current, Full)
      | Seq.null pending -> do
          ok <- notify
          pure (if ok then Accepting notify (Seq.singleton input) else Closed, if ok then Accepted else IngressClosed)
      | otherwise -> pure (Accepting notify (pending |> input), Accepted)
    _ -> pure (current, IngressClosed)

drainExternal :: ExternalIngress a -> IO (Drain a)
drainExternal (ExternalIngress state) = modifyMVar state $ \current -> case current of
  Accepting notify pending ->
    let (batch, rest) = Seq.splitAt 32 pending
     in pure (Accepting notify rest, Drain (toList batch) (not (Seq.null rest)))
  _ -> pure (current, Drain [] False)
