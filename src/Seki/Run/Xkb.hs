-- | Selection and binding of river's XKB bindings global.
module Seki.Run.Xkb (
  riverXkbBindingsRequiredVersion,
  RiverKeyBinding (..),
  RiverXkbBootstrapError (..),
  selectRiverXkbBindingsGlobal,
  bindRiverXkbBindings,
  createRiverKeyBinding,
)
where

import Data.Bifunctor (first)
import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as Xkb
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Binding (interfaceMaximumVersion)
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  eventRoute,
  submitNewObjectRequestWithRoute,
 )
import Seki.Wire.Connection.ObjectTable (Object, objectId, objectVersion)
import Seki.Wire.Connection.Registry (
  Global,
  GlobalCatalog,
  GlobalName,
  RegistryError,
  bindGlobal,
  globalName,
  globalVersion,
  selectGlobals,
 )
import Seki.Wire.Transport (OwnedFd)

{- | Lowest version supported by the M0 target release.

Version 3 provides modifier watches needed by the configured binding model,
while the M0 terminal and close bindings use the version 1 request surface.
-}
riverXkbBindingsRequiredVersion :: Word32
riverXkbBindingsRequiredVersion = 3

-- | Declarative identity and initial configuration of one key binding.
data RiverKeyBinding = RiverKeyBinding
  { riverKeyBindingKeysym :: Word32
  , riverKeyBindingModifiers :: River.RiverSeatV1ModifiersFlags
  , riverKeyBindingLayoutOverride :: Maybe Word32
  }
  deriving (Eq, Show)

data RiverXkbBootstrapError
  = RiverXkbGlobalCatalogError RegistryError
  | RiverXkbBindingsGlobalMissing
  | RiverXkbBindingsGlobalsAmbiguous [GlobalName]
  | RiverXkbBindingsVersionTooLow Word32 Word32
  | RiverXkbBindingsBindError ConnectionError
  deriving (Eq, Show)

-- | Select the unique XKB bindings global and enforce seki's required version.
selectRiverXkbBindingsGlobal ::
  GlobalCatalog ->
  Either RiverXkbBootstrapError (Global Xkb.RiverXkbBindingsV1)
selectRiverXkbBindingsGlobal catalog = do
  globals <-
    first RiverXkbGlobalCatalogError $
      selectGlobals
        Xkb.riverXkbBindingsV1Interface
        (interfaceMaximumVersion Xkb.riverXkbBindingsV1Interface)
        catalog
  global <- case globals of
    [] -> Left RiverXkbBindingsGlobalMissing
    [selected] -> Right selected
    multiple -> Left (RiverXkbBindingsGlobalsAmbiguous (map globalName multiple))
  if globalVersion global < riverXkbBindingsRequiredVersion
    then
      Left
        ( RiverXkbBindingsVersionTooLow
            riverXkbBindingsRequiredVersion
            (globalVersion global)
        )
    else Right global

-- | Select and bind the unique XKB bindings global.
bindRiverXkbBindings ::
  Connection ->
  Object Wayland.WlRegistry ->
  GlobalCatalog ->
  IO (Either RiverXkbBootstrapError (Object Xkb.RiverXkbBindingsV1))
bindRiverXkbBindings connection registry catalog =
  case selectRiverXkbBindingsGlobal catalog of
    Left errorValue -> pure (Left errorValue)
    Right global ->
      first RiverXkbBindingsBindError
        <$> bindGlobal connection registry global

-- | Create one initially disabled binding and install its event route atomically.
createRiverKeyBinding ::
  Connection ->
  Object Xkb.RiverXkbBindingsV1 ->
  Object River.RiverSeatV1 ->
  RiverKeyBinding ->
  (Object Xkb.RiverXkbBindingV1 -> Xkb.RiverXkbBindingV1Event OwnedFd -> IO ()) ->
  IO (Either ConnectionError (Object Xkb.RiverXkbBindingV1))
createRiverKeyBinding connection bindings seat definition handler =
  submitNewObjectRequestWithRoute
    connection
    Xkb.riverXkbBindingsV1Interface
    bindings
    Xkb.riverXkbBindingV1Interface
    (objectVersion bindings)
    ( \binding ->
        Xkb.encodeRiverXkbBindingsV1GetXkbBinding
          (objectId seat)
          (objectId binding)
          (riverKeyBindingKeysym definition)
          (riverKeyBindingModifiers definition)
    )
    (eventRoute Xkb.decodeRiverXkbBindingV1Event handler)
