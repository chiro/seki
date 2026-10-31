module Seki.Integration.M3 (m3Scenarios) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (forM_, void)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (threadStatus)
import Seki.Fixture.Probe (Color (..), openWindow)
import Seki.Integration.Fixture
import Seki.Integration.Harness
import Seki.Internal.Config (def)
import Seki.Internal.Core.Action (restart, spawn, windows)
import Seki.Internal.Core.StackSet qualified as SS
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Run.Config (policyConfigOf)
import Seki.Run.External
import Seki.Run.Inventory (riverInventoryOutputs)
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (ConnectionTarget (..))

m3Scenarios :: [(String, IO ())]
m3Scenarios = [("m3-external-wake", externalWake), ("m3-external-session", sessionIsolation)]

externalWake :: IO ()
externalWake = do
  applied <- newIORef []
  manageHook <- newIORef Nothing
  renderHook <- newIORef Nothing
  afterDrain <- newIORef Nothing
  let once ref = atomicModifyIORef' ref (Nothing,) >>= sequence_
      decorate handlers =
        handlers
          { prepareRiverManagePlan = \snapshot -> do
              once manageHook
              plan <- prepareRiverManagePlan handlers snapshot
              once afterDrain
              pure plan
          , prepareRiverRenderPlan = \snapshot -> once renderHook >> prepareRiverRenderPlan handlers snapshot
          }
      config = (policyConfigOf def){policyLaunch = \marker -> modifyIORef' applied (<> [marker])}
  withFixtureUsing decorate config "m3-external-wake" 1 $ \fixture probe -> do
    _ <- openWindow probe Red
    let producer = policyExternalProducer (mPolicy fixture)
        submit marker = submitExternal producer (ExternalCommand (spawn marker)) >>= assertEqual "accepted marker" Accepted
        expect markers = void (waitUntil "accepted commands applied in FIFO order" assertionTimeoutMicroseconds (readIORef applied) (== markers))
        inProducer action = do
          done <- newEmptyMVar
          void (forkIO (action >> putMVar done ()))
          takeMVar done
    idleStatus <- waitUntil "dispatcher sleeping on Wayland read" assertionTimeoutMicroseconds (show <$> threadStatus (mDispatcher fixture)) (== "ThreadBlocked BlockedOnOther")
    announce ("M3_IDLE_DISPATCHER " <> idleStatus)
    forM_ [1 .. 1000 :: Int] $ \n -> do
      threadDelay 1000
      let tag = if odd n then "2" else "1"
      started <- getMonotonicTimeNSec
      submitExternal producer (ExternalCommand (windows (SS.view tag))) >>= assertEqual "idle accepted" Accepted
      let observe = do
            state <- readPolicyState (mPolicy fixture)
            case coreWorkspaces state of
              AttachedState ss _ | SS.currentTag ss == tag -> getMonotonicTimeNSec
              _ -> threadDelay 100 >> observe
      ended <- observe
      assertEqual "idle completion below 50ms ceiling" True (ended - started < 50000000)
      announce ("M3_IDLE_NS " <> show (ended - started))
    saveCapture fixture "external-wake"
    atomicModifyIORef' manageHook (const (Just (inProducer (submit "during-manage")), ()))
    submit "trigger-manage"
    expect ["trigger-manage", "during-manage"]
    saveCapture fixture "manage-queue"
    atomicModifyIORef' renderHook (const (Just (inProducer (submit "during-render")), ()))
    submit "trigger-render"
    expect ["trigger-manage", "during-manage", "trigger-render", "during-render"]
    saveCapture fixture "render-queue"
    let markers = map show [1 .. 255 :: Int]
    atomicModifyIORef'
      manageHook
      ( const
          ( Just
              ( inProducer
                  ( do
                      forM_ markers submit
                      submitExternal producer (ExternalCompletion (spawn "overflow")) >>= assertEqual "explicit overload" Full
                  )
              )
          , ()
          )
      )
    submit "trigger-burst"
    expect (["trigger-manage", "during-manage", "trigger-render", "during-render", "trigger-burst"] <> markers)
    saveCapture fixture "queue-limit"
    saveCapture fixture "burst"

    atomicModifyIORef' afterDrain (const (Just (inProducer (submit "final-drain")), ()))
    submit "trigger-drain"
    expect (["trigger-manage", "during-manage", "trigger-render", "during-render", "trigger-burst"] <> markers <> ["trigger-drain", "final-drain"])
    saveCapture fixture "drain-race"
    forM_ (mOutputs fixture) $ \output -> void (runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", Text.unpack output, "--off"])
    void (waitUntil "no outputs" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (null . riverInventoryOutputs))
    submitExternal producer (ExternalCompletion (spawn "headless")) >>= assertEqual "headless accepted" Accepted
    void (waitUntil "headless completion" assertionTimeoutMicroseconds (readIORef applied) (elem "headless"))
    forM_ (mOutputs fixture) $ \output -> void (runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", Text.unpack output, "--on", "--custom-mode", "800x600"])
    void (waitUntil "output restored" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (not . null . riverInventoryOutputs))
    saveCapture fixture "headless"

sessionIsolation :: IO ()
sessionIsolation = withNestedRiver 1 $ \river -> do
  launched <- newIORef []
  let config = (policyConfigOf def){policyLaunch = \marker -> modifyIORef' launched (<> [marker]), policyCheckRestart = pure (Right ())}
      open policy = openRiverSessionAt (UnixSocketPath (riverSocket river)) defaultObjectLimit (policyRuntimeHandlers policy) >>= requireRight "session"
      run session = do
        done <- newEmptyMVar
        void (forkIO (runRiverSession session >>= putMVar done))
        pure done
      attached policy = void (waitUntil "session attached" assertionTimeoutMicroseconds (readPolicyState policy) (\state -> case coreWorkspaces state of AttachedState{} -> True; _ -> False))
  first <- newPolicy config >>= requireRight "first policy"
  session <- open first
  done <- run session
  let old = policyExternalProducer first
      accepted action = submitExternal old (ExternalCommand action) >>= assertEqual "first admission" Accepted
  ( do
      attached first
      accepted (windows (SS.view "2"))
      void (waitUntil "workspace before restart" assertionTimeoutMicroseconds (readPolicyState first) (\state -> case coreWorkspaces state of AttachedState ss _ -> SS.currentTag ss == "2"; _ -> False))
      accepted restart
      forM_ [1 .. 64 :: Int] (\n -> accepted (spawn (show n)))
      requireRuntimeExit done >>= assertEqual "stop handshake with pending work" (Right RiverStopped)
      submitExternal old (ExternalCompletion (windows (SS.view "3"))) >>= assertEqual "stopped session rejects completion" IngressClosed
      readIORef launched >>= assertEqual "stop closes leftover batch" (map show [1 .. 31 :: Int])
      saved <- readRestoreState first
      second <- newPolicy config{policyRestore = Just saved} >>= requireRight "restored policy"
      next <- open second
      nextDone <- run next
      ( do
          attached second
          submitExternal old (ExternalCompletion (windows (SS.view "3"))) >>= assertEqual "old token after restore" IngressClosed
          readPolicyState second >>= \state -> case coreWorkspaces state of
            AttachedState ss _ -> assertEqual "restored workspace unchanged by old token" "2" (SS.currentTag ss)
            _ -> failIntegration "restore headless"
          submitExternal (policyExternalProducer second) (ExternalCommand (windows (SS.view "1"))) >>= assertEqual "fresh producer" Accepted
          void (waitUntil "new session progresses" assertionTimeoutMicroseconds (readPolicyState second) (\state -> case coreWorkspaces state of AttachedState ss _ -> SS.currentTag ss == "1"; _ -> False))
        )
        `finally` (stopManagedProcess (riverProcess river) >> waitForRuntimeExit nextDone)
    )
    `finally` (stopManagedProcess (riverProcess river) >> waitForRuntimeExit done)
