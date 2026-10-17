-- | Selection and binding of river's layer-shell global.
module Seki.Run.LayerShell (
  riverLayerShellRequiredVersion,
  RiverLayerShellBootstrapError (..),
  selectRiverLayerShellGlobal,
  bindRiverLayerShell,
)
where

import Data.Bifunctor (first)
import Data.Word (Word32)
import Seki.Protocol.RiverLayerShellV1 qualified as LayerShell
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Connection (Connection, ConnectionError)
import Seki.Wire.Connection.ObjectTable (Object)
import Seki.Wire.Connection.Registry (
  Global,
  GlobalCatalog,
  GlobalName,
  RegistryError,
  bindGlobal,
  globalName,
  selectGlobals,
 )

{- | The version seki binds. The registry rejects version 0 announcements, so
every announced layer-shell global satisfies it and there is no too-old error.
-}
riverLayerShellRequiredVersion :: Word32
riverLayerShellRequiredVersion = 1

data RiverLayerShellBootstrapError
  = RiverLayerShellGlobalCatalogError RegistryError
  | RiverLayerShellGlobalMissing
  | RiverLayerShellGlobalsAmbiguous [GlobalName]
  | RiverLayerShellBindError ConnectionError
  deriving (Eq, Show)

-- | Select the unique layer-shell global.
selectRiverLayerShellGlobal ::
  GlobalCatalog ->
  Either RiverLayerShellBootstrapError (Global LayerShell.RiverLayerShellV1)
selectRiverLayerShellGlobal catalog = do
  globals <-
    first RiverLayerShellGlobalCatalogError $
      selectGlobals
        LayerShell.riverLayerShellV1Interface
        riverLayerShellRequiredVersion
        catalog
  case globals of
    [] -> Left RiverLayerShellGlobalMissing
    [selected] -> Right selected
    multiple -> Left (RiverLayerShellGlobalsAmbiguous (map globalName multiple))

{- | Select and bind the unique layer-shell global. Binding it tells river that
the window manager supports layer surfaces.
-}
bindRiverLayerShell ::
  Connection ->
  Object Wayland.WlRegistry ->
  GlobalCatalog ->
  IO (Either RiverLayerShellBootstrapError (Object LayerShell.RiverLayerShellV1))
bindRiverLayerShell connection registry catalog =
  case selectRiverLayerShellGlobal catalog of
    Left errorValue -> pure (Left errorValue)
    Right global ->
      first RiverLayerShellBindError
        <$> bindGlobal connection registry global
