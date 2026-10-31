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

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVarMasked, newMVar)
import Control.Exception (SomeException, throwIO, try)
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
  wake = do
    result <- modifyMVarMasked state $ \current -> case current of
      Accepting notify pending | not (Seq.null pending) -> do
        sent <- try @SomeException notify
        pure (case sent of Right True -> current; _ -> Closed, () <$ sent)
      _ -> pure (current, Right ())
    either throwIO pure result

-- | Accepted means queued, not applied. Session closure discards pending inputs.
submitExternal :: ExternalProducer a -> a -> IO Admission
submitExternal (ExternalProducer (ExternalIngress state)) input = do
  result <- modifyMVarMasked state admit
  either throwIO pure result
 where
  admit current = case current of
    Accepting notify pending
      | Seq.length pending >= 256 -> pure (current, Right Full)
      | Seq.null pending -> do
          sent <- try @SomeException notify
          pure
            ( case sent of Right True -> Accepting notify (Seq.singleton input); _ -> Closed
            , (\ok -> if ok then Accepted else IngressClosed) <$> sent
            )
      | otherwise -> pure (Accepting notify (pending |> input), Right Accepted)
    _ -> pure (current, Right IngressClosed)

drainExternal :: ExternalIngress a -> IO (Drain a)
drainExternal (ExternalIngress state) = modifyMVar state $ \current -> case current of
  Accepting notify pending ->
    let (batch, rest) = Seq.splitAt 32 pending
     in pure (Accepting notify rest, Drain (toList batch) (not (Seq.null rest)))
  _ -> pure (current, Drain [] False)
