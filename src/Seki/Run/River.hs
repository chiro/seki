{-# LANGUAGE LambdaCase #-}

-- | Selection, binding, and child-route assembly for river's window manager.
module Seki.Run.River (
  riverWindowManagerRequiredVersion,
  RiverWindowManagerHandlers (..),
  RiverBootstrapError (..),
  selectRiverWindowManagerGlobal,
  riverWindowManagerRoute,
  bindRiverWindowManager,
)
where

import Data.Bifunctor (first)
import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Binding (interfaceMaximumVersion)
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  EventRoute,
  eventRoute,
  eventRouteWithServerObjects,
  serverObjectRoute,
 )
import Seki.Wire.Connection.ObjectTable (Object)
import Seki.Wire.Connection.Registry (
  Global,
  GlobalCatalog,
  GlobalName,
  RegistryError,
  bindGlobalWithRoute,
  globalName,
  globalVersion,
  selectGlobals,
 )
import Seki.Wire.Transport (OwnedFd)

-- | Lowest manager version providing the operations required by seki.
riverWindowManagerRequiredVersion :: Word32
riverWindowManagerRequiredVersion = 3

-- | Typed callbacks for the manager and each kind of server-created child.
data RiverWindowManagerHandlers = RiverWindowManagerHandlers
  { handleRiverWindowManagerEvent :: Object River.RiverWindowManagerV1 -> River.RiverWindowManagerV1Event OwnedFd -> IO ()
  , handleRiverWindowCreated :: Object River.RiverWindowV1 -> IO ()
  , handleRiverWindowEvent :: Object River.RiverWindowV1 -> River.RiverWindowV1Event OwnedFd -> IO ()
  , handleRiverOutputCreated :: Object River.RiverOutputV1 -> IO ()
  , handleRiverOutputEvent :: Object River.RiverOutputV1 -> River.RiverOutputV1Event OwnedFd -> IO ()
  , handleRiverSeatCreated :: Object River.RiverSeatV1 -> IO ()
  , handleRiverSeatEvent :: Object River.RiverSeatV1 -> River.RiverSeatV1Event OwnedFd -> IO ()
  }

data RiverBootstrapError
  = RiverGlobalCatalogError RegistryError
  | RiverWindowManagerGlobalMissing
  | RiverWindowManagerGlobalsAmbiguous [GlobalName]
  | RiverWindowManagerVersionTooLow Word32 Word32
  | RiverWindowManagerBindError ConnectionError
  deriving (Eq, Show)

-- | Select the unique manager global and enforce seki's required version.
selectRiverWindowManagerGlobal ::
  GlobalCatalog ->
  Either RiverBootstrapError (Global River.RiverWindowManagerV1)
selectRiverWindowManagerGlobal catalog = do
  globals <-
    first RiverGlobalCatalogError $
      selectGlobals
        River.riverWindowManagerV1Interface
        (interfaceMaximumVersion River.riverWindowManagerV1Interface)
        catalog
  global <- case globals of
    [] -> Left RiverWindowManagerGlobalMissing
    [selected] -> Right selected
    multiple -> Left (RiverWindowManagerGlobalsAmbiguous (map globalName multiple))
  if globalVersion global < riverWindowManagerRequiredVersion
    then
      Left
        ( RiverWindowManagerVersionTooLow
            riverWindowManagerRequiredVersion
            (globalVersion global)
        )
    else Right global

-- | Route manager events and atomically install all window/output/seat routes.
riverWindowManagerRoute :: RiverWindowManagerHandlers -> EventRoute River.RiverWindowManagerV1
riverWindowManagerRoute handlers =
  eventRouteWithServerObjects
    River.decodeRiverWindowManagerV1Event
    childRoutes
    (handleRiverWindowManagerEvent handlers)
 where
  childRoutes = \case
    River.RiverWindowManagerV1Window newWindow ->
      [ serverObjectRoute
          newWindow
          River.riverWindowV1Interface
          (eventRoute River.decodeRiverWindowV1Event (handleRiverWindowEvent handlers))
          (handleRiverWindowCreated handlers)
      ]
    River.RiverWindowManagerV1Output newOutput ->
      [ serverObjectRoute
          newOutput
          River.riverOutputV1Interface
          (eventRoute River.decodeRiverOutputV1Event (handleRiverOutputEvent handlers))
          (handleRiverOutputCreated handlers)
      ]
    River.RiverWindowManagerV1Seat newSeat ->
      [ serverObjectRoute
          newSeat
          River.riverSeatV1Interface
          (eventRoute River.decodeRiverSeatV1Event (handleRiverSeatEvent handlers))
          (handleRiverSeatCreated handlers)
      ]
    _ -> []

-- | Select and bind the manager global with its complete child routing tree.
bindRiverWindowManager ::
  Connection ->
  Object Wayland.WlRegistry ->
  GlobalCatalog ->
  RiverWindowManagerHandlers ->
  IO (Either RiverBootstrapError (Object River.RiverWindowManagerV1))
bindRiverWindowManager connection registry catalog handlers =
  case selectRiverWindowManagerGlobal catalog of
    Left errorValue -> pure (Left errorValue)
    Right global ->
      first RiverWindowManagerBindError
        <$> bindGlobalWithRoute
          connection
          registry
          global
          (riverWindowManagerRoute handlers)
