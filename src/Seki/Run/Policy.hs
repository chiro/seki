{-# LANGUAGE LambdaCase #-}

{- | The runtime policy that owns 'CoreState' and executes action intents.

River callbacks and key-binding presses append core events to one queue, so
their arrival order is kept across kinds. Each @manage_start@ drains the
queue into the core state, launches spawned commands, and sends close
requests in the manage sequence. While an output is attached, every manage
sequence then marks the current screen's output as the layer-shell default.
The request is idempotent, and river forgets the default when that output's
layer-shell object is destroyed, so it is sent every time rather than on
change.
-}
module Seki.Run.Policy (
  PolicyConfig (..),
  PolicyError (..),
  Policy,
  newPolicy,
  policyRuntimeHandlers,
  readPolicyState,
  launchDetached,
)
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Seki.Internal.Core.Action (Action, Intent (..))
import Seki.Internal.Core.Event (CoreEvent (..), drainCore)
import Seki.Internal.Core.StackSet (OutputId (..), Screen (..), StackSet (..), WindowId (..))
import Seki.Internal.Core.State (CoreState (..), WorkspaceTagError, newCoreState)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Run.Inventory (RiverOutputState (..), RiverWindowState (..), lookupRiverOutput, lookupRiverWindow)
import Seki.Run.Phase (ManageM, closeWindow, setDefaultLayerShellOutput)
import Seki.Run.River (RiverWindowManagerHandlers (..))
import Seki.Run.Runtime (RiverRuntimeHandlers (..), RiverRuntimeSnapshot (..), noRiverManageAction)
import Seki.Run.Xkb (RiverKeyBinding)
import Seki.Wire.Connection.ObjectTable (Object, objectId)
import Seki.Wire.Types (mkObjectId, objectIdValue)
import System.Exit (ExitCode)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, getProcessExitCode, proc)

data PolicyConfig i l = PolicyConfig
  { policyLayout :: l
  , policyTags :: [i]
  , policyBindings :: [(RiverKeyBinding, Action i l ())]
  , policyLaunch :: String -> IO ()
  -- ^ Runs while a manage sequence is prepared, so it must not block; see 'launchDetached'.
  }

data PolicyError i
  = PolicyTagError (WorkspaceTagError i)
  | PolicyBindingConflict RiverKeyBinding
  deriving (Eq, Show)

data Policy i l = Policy
  { policyConfig :: PolicyConfig i l
  , policyState :: IORef (CoreState i l)
  , policyQueue :: IORef [CoreEvent i l]
  -- ^ Newest first.
  }

-- | Validate the tags and bindings before any Wayland connection is made.
newPolicy :: (Eq i) => PolicyConfig i l -> IO (Either (PolicyError i) (Policy i l))
newPolicy config = case newCoreState (policyLayout config) (policyTags config) of
  Left e -> pure (Left (PolicyTagError e))
  Right st -> case firstRepeat [] (map fst (policyBindings config)) of
    Just binding -> pure (Left (PolicyBindingConflict binding))
    Nothing -> Right <$> (Policy config <$> newIORef st <*> newIORef [])
 where
  firstRepeat _ [] = Nothing
  firstRepeat seen (x : xs)
    | x `elem` seen = Just x
    | otherwise = firstRepeat (x : seen) xs

policyRuntimeHandlers :: (Eq i) => Policy i l -> RiverRuntimeHandlers
policyRuntimeHandlers policy =
  RiverRuntimeHandlers
    { riverProtocolHandlers =
        RiverWindowManagerHandlers
          { handleRiverWindowManagerEvent = \_ _ -> pure ()
          , handleRiverWindowCreated = enqueue . WindowCreated . windowIdOf
          , handleRiverWindowEvent = \window -> \case
              River.RiverWindowV1Closed -> enqueue (WindowClosed (windowIdOf window))
              _ -> pure ()
          , handleRiverOutputCreated = enqueue . OutputCreated . outputIdOf
          , handleRiverOutputEvent = \output -> \case
              River.RiverOutputV1Removed -> enqueue (OutputRemoved (outputIdOf output))
              _ -> pure ()
          , handleRiverSeatCreated = \_ -> pure ()
          , handleRiverSeatEvent = \_ -> \case
              River.RiverSeatV1WindowInteraction window -> enqueue (WindowInteraction (WindowId (objectIdValue window)))
              _ -> pure ()
          }
    , riverKeyBindings = map fst bindings
    , handleRiverKeyBindingEvent = \definition _ event -> do
        case (event, lookup definition bindings) of
          (Xkb.RiverXkbBindingV1Pressed, Just action) -> enqueue (RunAction action)
          _ -> pure ()
        pure noRiverManageAction
    , prepareRiverManagePlan = prepareManagePlan policy
    , prepareRiverRenderPlan = \_ -> pure (pure ())
    }
 where
  bindings = policyBindings (policyConfig policy)
  enqueue event = atomicModifyIORef' (policyQueue policy) (\queue -> (event : queue, ()))

prepareManagePlan :: (Eq i) => Policy i l -> RiverRuntimeSnapshot -> IO (ManageM ())
prepareManagePlan policy snapshot = do
  events <- reverse <$> atomicModifyIORef' (policyQueue policy) ([],)
  (st, intents) <- drainCore events <$> readIORef (policyState policy)
  writeIORef (policyState policy) st
  mapM_ (policyLaunch (policyConfig policy)) [cmd | Spawn cmd <- intents]
  pure $ do
    mapM_ closeWindow (mapMaybe liveWindow [w | CloseWindow w <- intents])
    mapM_ setDefaultLayerShellOutput (currentLayerShellOutput st)
 where
  inventory = riverRuntimeInventory snapshot
  liveWindow (WindowId w) =
    riverWindowObject <$> (mkObjectId w >>= (`lookupRiverWindow` inventory))
  currentLayerShellOutput = \case
    HeadlessState _ -> Nothing
    AttachedState ss _ ->
      let OutputId o = output (current ss)
       in mkObjectId o >>= (`lookupRiverOutput` inventory) >>= riverOutputLayerShell

readPolicyState :: Policy i l -> IO (CoreState i l)
readPolicyState = readIORef . policyState

{- | Run a command through @/bin/sh -c@ without waiting for it. A forked thread
reaps the shell when it exits and reports a launch failure on stderr.
-}
launchDetached :: String -> IO ()
launchDetached cmd = void . forkIO $ do
  started <- try (createProcess (proc "/bin/sh" ["-c", cmd]))
  case started of
    Left e -> hPutStrLn stderr ("seki: launch failed: " <> show (e :: IOException))
    Right (_, _, _, handle) -> reap (getProcessExitCode handle) 1000
 where
  -- waitForProcess would block every Haskell thread under the non-threaded
  -- RTS, so poll instead, backing off to once a second.
  reap :: IO (Maybe ExitCode) -> Int -> IO ()
  reap check delay =
    check >>= \case
      Just _ -> pure ()
      Nothing -> threadDelay delay >> reap check (min 1000000 (delay * 2))

windowIdOf :: Object River.RiverWindowV1 -> WindowId
windowIdOf = WindowId . rawId

outputIdOf :: Object River.RiverOutputV1 -> OutputId
outputIdOf = OutputId . rawId

rawId :: Object interface -> Word32
rawId = objectIdValue . objectId
