{-# LANGUAGE LambdaCase #-}

-- | Validate public configuration before opening the Wayland connection.
module Seki.Run.Config (ConfigRunError (..), policyConfigOf, prepareConfig, runConfig) where

import Seki.Internal.Config (BorderConfig (..), Button (..), Config (..), KeyBinding (..), Keysym (..), Mods (..), PointerAction (..), PointerBinding (..))
import Seki.Internal.Core.Layout (LayoutClass)
import Seki.Internal.Core.PointerOp (HEdge (..), OpKind (..), ResizeEdges (..), VEdge (..))
import Seki.Internal.Core.Scene (SceneStyle (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Pointer (RiverPointerBinding (..))
import Seki.Run.Policy (Policy, PolicyConfig (..), PolicyError, launchDetached, newPolicy, policyRuntimeHandlers)
import Seki.Run.Runtime (RiverRuntimeError, openRiverSession, runRiverSession)
import Seki.Run.Xkb (RiverKeyBinding (..))

data ConfigRunError = ConfigPolicyError (PolicyError String) | ConfigRuntimeError RiverRuntimeError
  deriving (Eq, Show)

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

runConfig :: (LayoutClass l) => Config l -> IO (Either ConfigRunError ())
runConfig config =
  prepareConfig config >>= \case
    Left err -> pure (Left (ConfigPolicyError err))
    Right policy ->
      openRiverSession (policyRuntimeHandlers policy) >>= \case
        Left err -> pure (Left (ConfigRuntimeError err))
        Right session -> either (Left . ConfigRuntimeError) (const (Right ())) <$> runRiverSession session
