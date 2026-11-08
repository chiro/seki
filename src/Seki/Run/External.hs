module Seki.Run.External (
  ExternalIngress,
  IngressHandle,
  Admission (..),
  newExternalIngress,
  ingressHandle,
  ingressWake,
  submitExternal,
  signalExternal,
  drainExternal,
  sealExternal,
  closeIngress,
) where

import Control.Concurrent.STM
import Control.Monad (unless, void, when)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq

{- | A bounded FIFO of external inputs for one session. Every operation is one
STM transaction, so a cancelled caller either completed its operation or left
the queue unchanged.
-}
data ExternalIngress a = ExternalIngress (TVar (Seq a)) IngressHandle

-- | What the runtime needs without knowing the input type: admission and the wake slot.
data IngressHandle = IngressHandle (TVar Bool) (TMVar ())

data Admission = Accepted | Full | IngressClosed deriving (Eq, Show)

newExternalIngress :: IO (ExternalIngress a)
newExternalIngress = ExternalIngress <$> newTVarIO Seq.empty <*> (IngressHandle <$> newTVarIO True <*> newEmptyTMVarIO)

ingressHandle :: ExternalIngress a -> IngressHandle
ingressHandle (ExternalIngress _ handle) = handle

-- | Holds at most one token; the runtime's wake worker takes it and asks river for a manage sequence.
ingressWake :: IngressHandle -> TMVar ()
ingressWake (IngressHandle _ wake) = wake

-- | Accepted means queued, not applied. Closing the ingress discards pending inputs.
submitExternal :: ExternalIngress a -> a -> IO Admission
submitExternal (ExternalIngress queue (IngressHandle open wake)) input = atomically $ do
  accepting <- readTVar open
  pending <- readTVar queue
  if not accepting
    then pure IngressClosed
    else
      if Seq.length pending >= 256
        then pure Full
        else writeTVar queue (pending |> input) >> void (tryPutTMVar wake ()) >> pure Accepted

-- | Wake the owner for a result kept outside the queue, whatever the queue holds.
signalExternal :: ExternalIngress a -> IO ()
signalExternal (ExternalIngress _ (IngressHandle open wake)) = atomically $ do
  accepting <- readTVar open
  when accepting (void (tryPutTMVar wake ()))

-- | Take at most 32 inputs, and keep the owner awake while more remain.
drainExternal :: ExternalIngress a -> IO [a]
drainExternal (ExternalIngress queue (IngressHandle open wake)) = atomically $ do
  accepting <- readTVar open
  if not accepting
    then pure []
    else do
      (batch, rest) <- Seq.splitAt 32 <$> readTVar queue
      writeTVar queue rest
      unless (Seq.null rest) (void (tryPutTMVar wake ()))
      pure (toList batch)

-- | Close only at an empty boundary, so stopping cannot discard accepted inputs.
sealExternal :: ExternalIngress a -> IO Bool
sealExternal (ExternalIngress queue (IngressHandle open _)) = atomically $ do
  accepting <- readTVar open
  pending <- readTVar queue
  if accepting && Seq.null pending then writeTVar open False >> pure True else pure False

closeIngress :: IngressHandle -> IO ()
closeIngress (IngressHandle open _) = atomically (writeTVar open False)
