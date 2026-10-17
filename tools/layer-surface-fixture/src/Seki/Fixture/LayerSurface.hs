{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A wlr-layer-shell client that maps one exclusive-zone surface for tests.
module Seki.Fixture.LayerSurface (
  FixtureConfig (..),
  FixtureReady (..),
  FixtureStop (..),
  FixtureError (..),
  FixtureControl,
  newFixtureControl,
  runLayerSurfaceFixture,
)
where

import Control.Exception (bracket)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Fixture.Protocol.WlrLayerShellUnstableV1 qualified as LayerShell
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Startup (
  WaylandSession,
  WaylandStartupError,
  closeWaylandSession,
  openWaylandSessionAt,
  readWaylandGlobalCatalog,
  waylandConnection,
  waylandRegistry,
 )
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  DispatchResult (..),
  dispatchNextEvent,
  eventRoute,
  submitInheritedObjectRequestWithRoute,
  submitRequest,
 )
import Seki.Wire.Connection.ObjectTable (Object, defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry (
  Global,
  GlobalCatalog,
  RegistryError,
  bindGlobal,
  bindGlobalWithRoute,
  selectGlobals,
 )
import Seki.Wire.Transport (ConnectionTarget)

newtype FixtureConfig = FixtureConfig
  { fixtureHeight :: Word32
  }
  deriving (Eq, Show)

data FixtureReady = FixtureReady
  { readyWidth :: Word32
  , readyHeight :: Word32
  }
  deriving (Eq, Show)

data FixtureStop
  = StoppedByRequest
  | ClosedByCompositor
  deriving (Eq, Show)

data FixtureError
  = FixtureStartupError WaylandStartupError
  | FixtureRegistryError RegistryError
  | FixtureMissingGlobal Text
  | FixtureConnectionError ConnectionError
  | FixtureConnectionFinished
  deriving (Eq, Show)

data FixtureControl = FixtureControl

newFixtureControl :: IO FixtureControl
newFixtureControl = pure FixtureControl

data FixtureGlobals = FixtureGlobals
  { compositorGlobal :: Global Wayland.WlCompositor
  , shmGlobal :: Global Wayland.WlShm
  , layerShellGlobal :: Global LayerShell.ZwlrLayerShellV1
  }

data BoundGlobals = BoundGlobals
  { boundCompositor :: Object Wayland.WlCompositor
  , boundShm :: Object Wayland.WlShm
  , boundLayerShell :: Object LayerShell.ZwlrLayerShellV1
  }

{- | Every global is bound at version 1: the fixture uses no later request or
event, so a newer compositor cannot change what it sends.
-}
boundVersion :: Word32
boundVersion = 1

runLayerSurfaceFixture ::
  FixtureControl ->
  FixtureConfig ->
  ConnectionTarget ->
  (FixtureReady -> IO ()) ->
  IO (Either FixtureError FixtureStop)
runLayerSurfaceFixture _ config target _ =
  openWaylandSessionAt target defaultObjectLimit >>= \case
    Left errorValue -> pure (Left (FixtureStartupError errorValue))
    Right opened ->
      bracket (pure opened) closeWaylandSession $ \session ->
        runExceptT $ do
          catalog <- fixtureStep FixtureRegistryError (readWaylandGlobalCatalog session)
          globals <- except (selectFixtureGlobals catalog)
          bound <- bindFixtureGlobals session globals
          _ <- requestLayerSurface (waylandConnection session) config bound
          awaitConnectionEnd session

selectFixtureGlobals :: GlobalCatalog -> Either FixtureError FixtureGlobals
selectFixtureGlobals catalog =
  FixtureGlobals
    <$> requireGlobal Wayland.wlCompositorInterface catalog
    <*> requireGlobal Wayland.wlShmInterface catalog
    <*> requireGlobal LayerShell.zwlrLayerShellV1Interface catalog

requireGlobal :: Interface interface -> GlobalCatalog -> Either FixtureError (Global interface)
requireGlobal interface catalog = case selectGlobals interface boundVersion catalog of
  Left errorValue -> Left (FixtureRegistryError errorValue)
  Right [] -> Left (FixtureMissingGlobal (interfaceName interface))
  Right (global : _) -> Right global

bindFixtureGlobals :: WaylandSession -> FixtureGlobals -> ExceptIO BoundGlobals
bindFixtureGlobals session globals = do
  compositor <- connectionStep (bindGlobal connection registry (compositorGlobal globals))
  shm <-
    connectionStep
      ( bindGlobalWithRoute
          connection
          registry
          (shmGlobal globals)
          (eventRoute Wayland.decodeWlShmEvent (\_ _ -> pure ()))
      )
  layerShell <- connectionStep (bindGlobal connection registry (layerShellGlobal globals))
  pure (BoundGlobals compositor shm layerShell)
 where
  connection = waylandConnection session
  registry = waylandRegistry session

data SurfaceObjects = SurfaceObjects
  { surfaceObject :: Object Wayland.WlSurface
  , layerSurfaceObject :: Object LayerShell.ZwlrLayerSurfaceV1
  }

requestLayerSurface :: Connection -> FixtureConfig -> BoundGlobals -> ExceptIO SurfaceObjects
requestLayerSurface connection config bound = do
  surface <-
    connectionStep
      ( submitInheritedObjectRequestWithRoute
          connection
          Wayland.wlCompositorInterface
          (boundCompositor bound)
          Wayland.wlSurfaceInterface
          (Wayland.encodeWlCompositorCreateSurface . objectId)
          (eventRoute Wayland.decodeWlSurfaceEvent (\_ _ -> pure ()))
      )
  layerSurface <-
    connectionStep
      ( submitInheritedObjectRequestWithRoute
          connection
          LayerShell.zwlrLayerShellV1Interface
          (boundLayerShell bound)
          LayerShell.zwlrLayerSurfaceV1Interface
          ( \object ->
              LayerShell.encodeZwlrLayerShellV1GetLayerSurface
                (objectId object)
                (objectId surface)
                Nothing
                LayerShell.ZwlrLayerShellV1LayerEnumTop
                layerNamespace
          )
          (eventRoute LayerShell.decodeZwlrLayerSurfaceV1Event (\_ _ -> pure ()))
      )
  let height = fixtureHeight config
      sendLayerSurface = connectionStep . submitRequest connection LayerShell.zwlrLayerSurfaceV1Interface layerSurface
  sendLayerSurface (LayerShell.encodeZwlrLayerSurfaceV1SetSize 0 height)
  sendLayerSurface (LayerShell.encodeZwlrLayerSurfaceV1SetAnchor topEdgeAnchor)
  sendLayerSurface (LayerShell.encodeZwlrLayerSurfaceV1SetExclusiveZone (fromIntegral height))
  connectionStep (submitRequest connection Wayland.wlSurfaceInterface surface Wayland.encodeWlSurfaceCommit)
  pure (SurfaceObjects surface layerSurface)

layerNamespace :: Text
layerNamespace = "seki-layer-surface-fixture"

topEdgeAnchor :: LayerShell.ZwlrLayerSurfaceV1AnchorFlags
topEdgeAnchor =
  LayerShell.ZwlrLayerSurfaceV1AnchorFlags
    ( LayerShell.zwlrLayerSurfaceV1AnchorFlagsValue LayerShell.ZwlrLayerSurfaceV1AnchorFlagsTop
        .|. LayerShell.zwlrLayerSurfaceV1AnchorFlagsValue LayerShell.ZwlrLayerSurfaceV1AnchorFlagsLeft
        .|. LayerShell.zwlrLayerSurfaceV1AnchorFlagsValue LayerShell.ZwlrLayerSurfaceV1AnchorFlagsRight
    )

awaitConnectionEnd :: WaylandSession -> ExceptIO FixtureStop
awaitConnectionEnd session =
  connectionStep (dispatchNextEvent (waylandConnection session)) >>= \case
    DispatchFinished -> throwE FixtureConnectionFinished
    EventDispatched _ -> awaitConnectionEnd session

type ExceptIO = ExceptT FixtureError IO

fixtureStep :: (errorValue -> FixtureError) -> IO (Either errorValue value) -> ExceptIO value
fixtureStep wrap = withExceptT wrap . ExceptT

connectionStep :: IO (Either ConnectionError value) -> ExceptIO value
connectionStep = fixtureStep FixtureConnectionError
