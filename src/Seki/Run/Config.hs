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
import Control.Exception (IOException, try)
import System.Directory (removeFile)

import Seki.Internal.Config (BorderConfig (..), Button (..), Config (..), KeyBinding (..), Keysym (..), Mods (..), PointerAction (..), PointerBinding (..))
import Seki.Internal.Core.Layout (LayoutClass)
import Seki.Internal.Core.PointerOp (HEdge (..), OpKind (..), ResizeEdges (..), VEdge (..))
import Seki.Internal.Core.Restore (RestoreState)
import Seki.Internal.Core.Scene (SceneStyle (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Pointer (RiverPointerBinding (..))
import Seki.Run.Policy (Policy, PolicyConfig (..), PolicyError, launchDetached, newPolicy, policyRuntimeHandlers, readRestoreState)
import Seki.Run.Restart (Launch (..), RestartRefusal (..), checkRestart, describeLoadError, discoverLaunch, execRestart, loadRestoreFile, reapInheritedChildren, writeRestoreFile)
import Seki.Run.Runtime (RiverExit (..), RiverRuntimeError (..), openRiverSessionAt, readRiverInventory, runRiverSession)
import Seki.Run.Startup (WaylandStartupError (..))
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import System.IO (hPutStrLn, stderr)

data ConfigRunError
  = ConfigPolicyError (PolicyError String)
  | ConfigRuntimeError RiverRuntimeError
  | -- | The command line was not empty or @--restore FILE@.
    ConfigUsageError String
  deriving (Eq, Show)

newtype SekiArgs = SekiArgs {argsRestore :: Maybe FilePath}
  deriving (Eq, Show)

-- | @seki@ or @seki --restore FILE@; anything else is a usage error.
parseSekiArgs :: [String] -> Either String SekiArgs
parseSekiArgs = \case
  [] -> Right (SekiArgs Nothing)
  ["--restore", file] -> Right (SekiArgs (Just file))
  args -> Left ("usage: seki [--restore FILE], got " <> show args)

prepareConfig :: Config l -> IO (Either (PolicyError String) (Policy String l))
prepareConfig = newPolicy . policyConfigOf

policyConfigOf :: Config l -> PolicyConfig String l
policyConfigOf config =
  PolicyConfig
    { policyLayout = layoutHook config
    , policyTags = workspaces config
    , policyBindings = [(lower binding, action) | (binding, action) <- keys config config]
    , policySceneStyle = SceneStyle (borderWidth style) (focusedBorderColor style) (unfocusedBorderColor style)
    , policyLaunch = launchDetached
    , policyManageHook = manageHook config
    , policyStartupHook = startupHook config
    , policyPointerBindings = [(lowerPointer binding, opKind action) | (binding, action) <- pointerBindings config config]
    , policyRestore = Nothing
    , policyCheckRestart = pure (Left TargetUnrecorded)
    , policyLog = hPutStrLn stderr . ("seki: " <>)
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
reaped first, and a state file named by @--restore@ is read and deleted; one
that cannot be used is logged, and seki starts fresh.
-}
runConfig :: (LayoutClass l) => [String] -> Config l -> IO (Either ConfigRunError ())
runConfig args config = case parseSekiArgs args of
  Left usage -> pure (Left (ConfigUsageError usage))
  Right parsed -> do
    reapInheritedChildren
    launch <- discoverLaunch
    let base = (policyConfigOf config){policyCheckRestart = checkRestart launch}
    restore <- case argsRestore parsed of
      Nothing -> pure Nothing
      Just file ->
        loadRestoreFile file >>= \case
          Left e -> policyLog base ("restore: " <> describeLoadError e) >> pure Nothing
          Right saved -> pure (Just saved)
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
runSessions launch configFor = pass (launchFirstTarget launch) unavailableRetries
 where
  logLine = policyLog (configFor Nothing)
  pass target retries restore = case target of
    Left e -> pure (Left (ConfigRuntimeError (RiverRuntimeStartupError (WaylandConnectionDiscoveryError e))))
    Right connection ->
      newPolicy (configFor restore){policyRestore = restore} >>= \case
        Left e -> pure (Left (ConfigPolicyError e))
        Right policy ->
          openRiverSessionAt connection defaultObjectLimit (policyRuntimeHandlers policy) >>= \case
            Left e -> pure (Left (ConfigRuntimeError e))
            Right session ->
              runRiverSession session >>= \case
                Left RiverRuntimeWindowManagerUnavailable
                  | retries > 0 -> do
                      logLine "window management unavailable, retrying"
                      threadDelay 100000
                      pass (launchReconnectTarget launch) (retries - 1) restore
                Left e -> pure (Left (ConfigRuntimeError e))
                Right RiverEnded -> pure (Right ())
                Right RiverStopped -> do
                  saved <- readRestoreState policy =<< readRiverInventory session
                  handOver saved
                  pass (launchReconnectTarget launch) unavailableRetries (Just saved)
  handOver saved = case launchStateDir launch of
    Left refusal -> logLine ("restart: no state directory (" <> show refusal <> "); restoring in this process")
    Right dir ->
      writeRestoreFile dir saved >>= \case
        Left e -> logLine ("restart: cannot write the state (" <> show e <> "); restoring in this process")
        Right file -> do
          failure <- execRestart launch file
          _ <- try @IOException (removeFile file)
          logLine ("restart: exec of " <> either show id (launchTarget launch) <> " failed (" <> show failure <> "); restoring in this process")

unavailableRetries :: Int
unavailableRetries = 10
