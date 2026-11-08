{-# LANGUAGE LambdaCase #-}

{- | Validate public configuration before opening the Wayland connection, and
run sessions until one ends without a restart.
-}
module Seki.Run.Config (
  ConfigRunError (..),
  SekiArgs (..),
  parseSekiArgs,
  policyConfigOf,
  prepareConfig,
  runConfig,
  runSessions,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, finally, try)
import Control.Monad (void)
import System.Directory (removeFile)

import Seki.Internal.Config (BorderConfig (..), Button (..), Config (..), KeyBinding (..), Keysym (..), Mods (..), PointerAction (..), PointerBinding (..))
import Seki.Internal.Core.Layout (LayoutClass)
import Seki.Internal.Core.PointerOp (HEdge (..), OpKind (..), ResizeEdges (..), VEdge (..))
import Seki.Internal.Core.Restore (RestoreState)
import Seki.Internal.Core.Scene (SceneStyle (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.ConfigBuild (installedContext, validatedProfile)
import Seki.Run.IPC (withIPC)
import Seki.Run.Pointer (RiverPointerBinding (..))
import Seki.Run.Policy (Policy, PolicyConfig (..), PolicyError, closePolicy, configureBuild, configureBuildFailure, launchDetached, newPolicy, policyRuntimeHandlers, policyValidatedTarget, readRestoreState, recordPolicyRecoveryFailure)
import Seki.Run.Restart (Launch (..), RestartRefusal (..), checkRestartTarget, describeLoadError, discoverLaunch, execRestart, execValidatedRestart, loadRestoreFile, reapInheritedChildren, writeRestoreFile)
import Seki.Run.Runtime (RiverExit (..), RiverRuntimeError (..), closeRiverSession, openRiverSessionAt, runRiverSession)
import Seki.Run.Startup (WaylandStartupError (..))
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import System.IO (hPutStrLn, stderr)

data ConfigRunError
  = ConfigPolicyError (PolicyError String)
  | ConfigRuntimeError RiverRuntimeError
  | -- | The command line did not select exactly one supported mode.
    ConfigUsageError String
  deriving (Eq, Show)

data SekiArgs = Run | Restore FilePath | Check
  deriving (Eq, Show)

-- | Exactly one of fresh startup, restore, or candidate validation.
parseSekiArgs :: [String] -> Either String SekiArgs
parseSekiArgs = \case
  [] -> Right Run
  ["--restore", file] | file /= "--check" -> Right (Restore file)
  ["--check"] -> Right Check
  args -> Left ("usage: seki [--restore FILE | --check], got " <> show args)

prepareConfig :: Config l -> IO (Either (PolicyError String) (Policy String l))
prepareConfig = newPolicy . policyConfigOf

policyConfigOf :: Config l -> PolicyConfig String l
policyConfigOf config =
  PolicyConfig
    { policyScratchpads = scratchpads config
    , policyInputRules = inputRules config
    , policyLayout = layoutHook config
    , policyTags = workspaces config
    , policyBindings = [(lower binding, action) | (binding, action) <- keys config config]
    , policySceneStyle = SceneStyle (borderWidth style) (focusedBorderColor style) (unfocusedBorderColor style)
    , policyLaunch = launchDetached
    , policyManageHook = manageHook config
    , policyStartupHook = startupHook config
    , policyPointerBindings = [(lowerPointer binding, opKind action) | (binding, action) <- pointerBindings config config]
    , policyRestore = Nothing
    , policyCheckRestart = const (pure (Left TargetUnrecorded))
    , policyLog = \message -> void (try @IOException (hPutStrLn stderr ("seki: " <> message)))
    }
 where
  style = borders config
  lowerPointer (PointerBinding (Mods modifiers) (Button button)) = RiverPointerBinding button (River.RiverSeatV1ModifiersFlags modifiers)
  lower (KeyBinding (Mods modifiers) (Keysym symbol)) =
    RiverKeyBinding
      { riverKeyBindingKeysym = symbol
      , riverKeyBindingModifiers = River.RiverSeatV1ModifiersFlags modifiers
      , riverKeyBindingLayoutOverride = if bindingsUseFixedLayout config then Just 0 else Nothing
      }

opKind :: PointerAction -> OpKind
opKind = \case
  MoveWindow -> Move
  ResizeWindow -> Resize (Corner RightEdge BottomEdge)

{- | Run seki from its command line. Children an earlier image left behind are
reaped first in runtime modes. A state file named by @--restore@ is read and
deleted; one that cannot be used is logged, and seki starts fresh. @--check@ instead uses
'prepareConfig' and returns before any runtime effects.
-}
runConfig :: (LayoutClass l) => [String] -> Config l -> IO (Either ConfigRunError ())
runConfig args config = case parseSekiArgs args of
  Left usage -> pure (Left (ConfigUsageError usage))
  Right Check -> prepareConfig config >>= pure . either (Left . ConfigPolicyError) (const (Right ()))
  Right parsed -> do
    reapInheritedChildren
    launch <- discoverLaunch
    let base = (policyConfigOf config){policyCheckRestart = checkRestartTarget launch}
    restore <- case parsed of
      Restore file ->
        loadRestoreFile file >>= \case
          Left e -> policyLog base ("restore: " <> describeLoadError e) >> pure Nothing
          Right saved -> pure (Just saved)
      _ -> pure Nothing
    runSessions launch (const base) restore

{- | Run sessions until one ends without a restart. Only a 'RestoreState'
crosses passes: each pass builds a fresh policy and session, so no object
handle from an old connection can reach the new one. A stopped session is
saved after @finished@ and execed; if the exec fails, the next pass restores
the same state in this process on the socket path. River answers a manager
that binds while another still holds window management with
@unavailable@, which a crash restart can race, so that is retried briefly.
-}
runSessions :: (LayoutClass l) => Launch -> (Maybe (RestoreState String) -> PolicyConfig String l) -> Maybe (RestoreState String) -> IO (Either ConfigRunError ())
runSessions launch configFor = pass (launchFirstTarget launch) unavailableRetries Nothing
 where
  logLine message = void (try @IOException (policyLog (configFor Nothing) message))
  pass target retries previousFailure restore = case target of
    Left e -> pure (Left (ConfigRuntimeError (RiverRuntimeStartupError (WaylandConnectionDiscoveryError e))))
    Right connection ->
      newPolicy (configFor restore){policyRestore = restore} >>= \case
        Left e -> pure (Left (ConfigPolicyError e))
        Right policy -> do
          context <- try @IOException installedContext
          case context of
            Right (Just buildContext) -> configureBuild policy buildContext
            Left err -> configureBuildFailure policy ("build context unavailable: " <> show err)
            _ -> pure ()
          mapM_ (recordPolicyRecoveryFailure policy) previousFailure
          openRiverSessionAt connection defaultObjectLimit (policyRuntimeHandlers policy) >>= \case
            Left e -> closePolicy policy >> pure (Left (ConfigRuntimeError e))
            Right session ->
              (withIPC logLine policy (runRiverSession session) `finally` (closeRiverSession session >> closePolicy policy)) >>= \case
                Left RiverRuntimeWindowManagerUnavailable
                  | retries > 0 -> do
                      logLine "window management unavailable, retrying"
                      threadDelay 100000
                      pass (launchReconnectTarget launch) (retries - 1) previousFailure restore
                Left e -> pure (Left (ConfigRuntimeError e))
                Right RiverEnded -> pure (Right ())
                Right RiverStopped -> do
                  saved <- readRestoreState policy
                  candidate <- policyValidatedTarget policy
                  failure <- handOver candidate saved
                  pass (launchReconnectTarget launch) unavailableRetries failure (Just saved)
  handOver candidate saved = case launchStateDir launch of
    Left refusal -> reportFailure ("restart: no state directory (" <> show refusal <> "); restoring in this process")
    Right dir ->
      writeRestoreFile dir saved >>= \case
        Left e -> reportFailure ("restart: cannot write the state (" <> show e <> "); restoring in this process")
        Right file -> do
          failure <- maybe (execRestart launch file) (\target -> execValidatedRestart launch target file) candidate
          _ <- try @IOException (removeFile file)
          reportFailure ("restart: exec of " <> maybe (either show id (launchTarget launch)) validatedProfile candidate <> " failed (" <> show failure <> "); restoring in this process")
  reportFailure message = logLine message >> pure (Just message)

unavailableRetries :: Int
unavailableRetries = 10
