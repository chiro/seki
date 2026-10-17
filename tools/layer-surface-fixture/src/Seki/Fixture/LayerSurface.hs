{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | A wlr-layer-shell client that maps one exclusive-zone surface for tests.
module Seki.Fixture.LayerSurface (
  FixtureConfig (..),
  FixtureReady (..),
  FixtureStop (..),
  FixtureError (..),
  FixtureControl,
  newFixtureControl,
  parseFixtureArguments,
  readyLine,
  requestStop,
  runLayerSurfaceFixture,
  stopOnEndOfInput,
)
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, bracket, try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (for_)
import Data.Int (Int32)
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
  waylandDisplay,
  waylandRegistry,
 )
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  DispatchResult (..),
  dispatchNextEvent,
  eventRoute,
  submitInheritedObjectRequest,
  submitInheritedObjectRequestWithRoute,
  submitNewObjectRequestWithRoute,
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
import Seki.Wire.Transport (ConnectionTarget, OwnedFd, adoptFd, withOwnedFd)
import System.IO (Handle)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.IO.ByteString (fdWrite)
import System.Posix.Process (getProcessID)
import System.Posix.SharedMem (ShmOpenFlags (..), shmOpen, shmUnlink)
import System.Posix.Types (Fd)
import Text.Read (readMaybe)

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
  | FixtureUnusableBufferSize Word32 Word32
  | FixtureShmError String
  deriving (Eq, Show)

-- | Everything the fixture reacts to after its initial commit, in arrival order.
data FixtureInput
  = InputConfigure Word32 Word32
  | InputClosed
  | InputSyncDone SyncPurpose
  | InputStop
  | InputConnectionEnded (Either ConnectionError ())

-- | Which round trip a sync callback closes; a stop can overtake the readiness sync.
data SyncPurpose
  = ReadinessSync
  | TeardownSync

newtype FixtureControl = FixtureControl (Chan FixtureInput)

newFixtureControl :: IO FixtureControl
newFixtureControl = FixtureControl <$> newChan

{- | Read and discard input until end of file, then request a stop. A read
error counts as end of file, since the harness can no longer reach the fixture.
-}
stopOnEndOfInput :: Handle -> FixtureControl -> IO ()
stopOnEndOfInput input control = do
  chunk <- try @IOException (ByteString.hGetSome input 4096)
  case chunk of
    Right bytes | not (ByteString.null bytes) -> stopOnEndOfInput input control
    _ -> requestStop control

-- | The one stdout line the harness waits for.
readyLine :: FixtureReady -> String
readyLine ready = "ready " <> show (readyWidth ready) <> " " <> show (readyHeight ready)

-- | Parse the optional bar height in pixels, which is also the exclusive zone.
parseFixtureArguments :: [String] -> Either String FixtureConfig
parseFixtureArguments = \case
  [] -> Right (FixtureConfig defaultHeight)
  [value]
    | Just height <- readMaybe value
    , height >= 1
    , height <= toInteger (maxBound :: Int32) ->
        Right (FixtureConfig (fromInteger height))
  _ -> Left "usage: seki-layer-surface-fixture [HEIGHT], where HEIGHT is a positive pixel count"

defaultHeight :: Word32
defaultHeight = 32

-- | Ask a running fixture to tear down and exit cleanly. Safe from any thread.
requestStop :: FixtureControl -> IO ()
requestStop (FixtureControl inputs) = writeChan inputs InputStop

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
runLayerSurfaceFixture control@(FixtureControl inputs) config target reportReady =
  openWaylandSessionAt target defaultObjectLimit >>= \case
    Left errorValue -> pure (Left (FixtureStartupError errorValue))
    Right opened ->
      bracket (pure opened) closeWaylandSession $ \session ->
        runExceptT $ do
          catalog <- fixtureStep FixtureRegistryError (readWaylandGlobalCatalog session)
          globals <- except (selectFixtureGlobals catalog)
          bound <- bindFixtureGlobals session globals
          surfaces <- requestLayerSurface (waylandConnection session) control config bound
          let fixture = Fixture session config bound surfaces control reportReady
          ExceptT $
            bracket
              (forkIO (dispatchInputs (waylandConnection session) inputs))
              killThread
              (\_ -> runExceptT (runPhase fixture AwaitingConfigure))

data Fixture = Fixture
  { fixtureSession :: WaylandSession
  , fixtureConfig :: FixtureConfig
  , fixtureGlobals :: BoundGlobals
  , fixtureSurfaces :: SurfaceObjects
  , fixtureControl :: FixtureControl
  , fixtureReportReady :: FixtureReady -> IO ()
  }

data Phase
  = AwaitingConfigure
  | AwaitingReadySync MappedBuffer FixtureReady
  | Running MappedBuffer
  | AwaitingTeardownSync FixtureStop

data MappedBuffer = MappedBuffer
  { mappedPool :: Object Wayland.WlShmPool
  , mappedBuffer :: Object Wayland.WlBuffer
  }

data Transition
  = MapBuffer Word32 Word32
  | ReportReady MappedBuffer FixtureReady
  | TearDown (Maybe MappedBuffer) FixtureStop
  | Finish FixtureStop
  | Fail FixtureError
  | Ignore

transition :: Phase -> FixtureInput -> Transition
transition _ (InputConnectionEnded (Left errorValue)) = Fail (FixtureConnectionError errorValue)
transition _ (InputConnectionEnded (Right ())) = Fail FixtureConnectionFinished
transition (AwaitingTeardownSync stop) (InputSyncDone TeardownSync) = Finish stop
transition (AwaitingTeardownSync _) _ = Ignore
transition phase InputStop = TearDown (phaseBuffer phase) StoppedByRequest
transition phase InputClosed = TearDown (phaseBuffer phase) ClosedByCompositor
transition AwaitingConfigure (InputConfigure serial width) = MapBuffer serial width
transition (AwaitingReadySync mapped ready) (InputSyncDone ReadinessSync) = ReportReady mapped ready
transition _ _ = Ignore

phaseBuffer :: Phase -> Maybe MappedBuffer
phaseBuffer = \case
  AwaitingConfigure -> Nothing
  AwaitingReadySync mapped _ -> Just mapped
  Running mapped -> Just mapped
  AwaitingTeardownSync _ -> Nothing

runPhase :: Fixture -> Phase -> ExceptIO FixtureStop
runPhase fixture phase = do
  input <- lift (readChan inputs)
  case transition phase input of
    MapBuffer serial width -> mapBuffer fixture serial width >>= runPhase fixture
    ReportReady mapped ready -> do
      lift (fixtureReportReady fixture ready)
      runPhase fixture (Running mapped)
    TearDown mapped stop -> tearDown fixture mapped stop >>= runPhase fixture
    Finish stop -> pure stop
    Fail errorValue -> throwE errorValue
    Ignore -> runPhase fixture phase
 where
  FixtureControl inputs = fixtureControl fixture

-- | Destroy every object the fixture created, role object before its surface.
tearDown :: Fixture -> Maybe MappedBuffer -> FixtureStop -> ExceptIO Phase
tearDown fixture mapped stop = do
  for_ mapped $ \buffers -> do
    send Wayland.wlBufferInterface (mappedBuffer buffers) Wayland.encodeWlBufferDestroy
    send Wayland.wlShmPoolInterface (mappedPool buffers) Wayland.encodeWlShmPoolDestroy
  send LayerShell.zwlrLayerSurfaceV1Interface (layerSurfaceObject surfaces) LayerShell.encodeZwlrLayerSurfaceV1Destroy
  send Wayland.wlSurfaceInterface (surfaceObject surfaces) Wayland.encodeWlSurfaceDestroy
  requestSync fixture TeardownSync
  pure (AwaitingTeardownSync stop)
 where
  surfaces = fixtureSurfaces fixture
  send interface object = connectionStep . submitRequest (waylandConnection (fixtureSession fixture)) interface object

mapBuffer :: Fixture -> Word32 -> Word32 -> ExceptIO Phase
mapBuffer fixture serial width = do
  let height = fixtureHeight (fixtureConfig fixture)
      connection = waylandConnection (fixtureSession fixture)
      surface = surfaceObject (fixtureSurfaces fixture)
  layout <- except (bufferLayout width height)
  connectionStep
    ( submitRequest
        connection
        LayerShell.zwlrLayerSurfaceV1Interface
        (layerSurfaceObject (fixtureSurfaces fixture))
        (LayerShell.encodeZwlrLayerSurfaceV1AckConfigure serial)
    )
  descriptor <- fixtureStep (FixtureShmError . show) (try @IOException (createPixelFile (layoutSize layout)))
  pool <-
    connectionStep
      ( submitInheritedObjectRequest
          connection
          Wayland.wlShmInterface
          (boundShm (fixtureGlobals fixture))
          Wayland.wlShmPoolInterface
          (\object -> Wayland.encodeWlShmCreatePool (objectId object) descriptor (layoutSize layout))
      )
  buffer <-
    connectionStep
      ( submitInheritedObjectRequestWithRoute
          connection
          Wayland.wlShmPoolInterface
          pool
          Wayland.wlBufferInterface
          ( \object ->
              Wayland.encodeWlShmPoolCreateBuffer
                (objectId object)
                0
                (layoutWidth layout)
                (layoutHeight layout)
                (layoutStride layout)
                Wayland.WlShmFormatEnumXrgb8888
          )
          (eventRoute Wayland.decodeWlBufferEvent (\_ _ -> pure ()))
      )
  let sendSurface = connectionStep . submitRequest connection Wayland.wlSurfaceInterface surface
  sendSurface (Wayland.encodeWlSurfaceAttach (Just (objectId buffer)) 0 0)
  sendSurface (Wayland.encodeWlSurfaceDamage 0 0 (layoutWidth layout) (layoutHeight layout))
  sendSurface Wayland.encodeWlSurfaceCommit
  requestSync fixture ReadinessSync
  pure (AwaitingReadySync (MappedBuffer pool buffer) (FixtureReady width height))

requestSync :: Fixture -> SyncPurpose -> ExceptIO ()
requestSync fixture purpose = do
  _ <-
    connectionStep
      ( submitNewObjectRequestWithRoute
          (waylandConnection session)
          Wayland.wlDisplayInterface
          (waylandDisplay session)
          Wayland.wlCallbackInterface
          1
          (Wayland.encodeWlDisplaySync . objectId)
          (eventRoute Wayland.decodeWlCallbackEvent (\_ _ -> writeChan inputs (InputSyncDone purpose)))
      )
  pure ()
 where
  session = fixtureSession fixture
  FixtureControl inputs = fixtureControl fixture

data BufferLayout = BufferLayout
  { layoutWidth :: Int32
  , layoutHeight :: Int32
  , layoutStride :: Int32
  , layoutSize :: Int32
  }

-- | One XRGB8888 buffer, rejected when empty or when its size overflows the wire's int.
bufferLayout :: Word32 -> Word32 -> Either FixtureError BufferLayout
bufferLayout width height
  | width == 0 || height == 0 || size > toInteger (maxBound :: Int32) =
      Left (FixtureUnusableBufferSize width height)
  | otherwise =
      Right
        BufferLayout
          { layoutWidth = fromIntegral width
          , layoutHeight = fromIntegral height
          , layoutStride = fromIntegral stride
          , layoutSize = fromIntegral size
          }
 where
  stride = toInteger width * bytesPerPixel
  size = stride * toInteger height

bytesPerPixel :: Integer
bytesPerPixel = 4

{- | Create an unlinked shared-memory file filled with one opaque colour.

The name only has to survive until 'shmUnlink', so a collision with another
fixture just moves on to the next attempt.
-}
createPixelFile :: Int32 -> IO OwnedFd
createPixelFile size = do
  processId <- getProcessID
  descriptor <- openUniqueShm processId (0 :: Int)
  owned <- adoptFd descriptor
  _ <- withOwnedFd owned (\fd -> writeAll fd (ByteString.concat (replicate pixelCount pixel)))
  pure owned
 where
  pixelCount = fromIntegral size `div` fromIntegral bytesPerPixel
  pixel = ByteString.pack [0x99, 0x66, 0x33, 0xff]
  openUniqueShm processId attempt = do
    let name = "/seki-layer-surface-fixture-" <> show processId <> "-" <> show attempt
    opened <- try @IOException (shmOpen name exclusiveCreate 0o600)
    case opened of
      Right descriptor -> shmUnlink name >> pure descriptor
      Left exception
        | isAlreadyExistsError exception -> openUniqueShm processId (attempt + 1)
        | otherwise -> ioError exception
  exclusiveCreate =
    ShmOpenFlags{shmReadWrite = True, shmCreate = True, shmExclusive = True, shmTrunc = False}

writeAll :: Fd -> ByteString -> IO ()
writeAll descriptor bytes
  | ByteString.null bytes = pure ()
  | otherwise = do
      written <- fdWrite descriptor bytes
      writeAll descriptor (ByteString.drop (fromIntegral written) bytes)

dispatchInputs :: Connection -> Chan FixtureInput -> IO ()
dispatchInputs connection inputs =
  dispatchNextEvent connection >>= \case
    Left errorValue -> writeChan inputs (InputConnectionEnded (Left errorValue))
    Right DispatchFinished -> writeChan inputs (InputConnectionEnded (Right ()))
    Right (EventDispatched _) -> dispatchInputs connection inputs

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

requestLayerSurface :: Connection -> FixtureControl -> FixtureConfig -> BoundGlobals -> ExceptIO SurfaceObjects
requestLayerSurface connection (FixtureControl inputs) config bound = do
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
          (eventRoute LayerShell.decodeZwlrLayerSurfaceV1Event (\_ event -> writeChan inputs (layerSurfaceInput event)))
      )
  let height = fixtureHeight config
      sendLayerSurface = connectionStep . submitRequest connection LayerShell.zwlrLayerSurfaceV1Interface layerSurface
  sendLayerSurface (LayerShell.encodeZwlrLayerSurfaceV1SetSize 0 height)
  sendLayerSurface (LayerShell.encodeZwlrLayerSurfaceV1SetAnchor topEdgeAnchor)
  sendLayerSurface (LayerShell.encodeZwlrLayerSurfaceV1SetExclusiveZone (fromIntegral height))
  connectionStep (submitRequest connection Wayland.wlSurfaceInterface surface Wayland.encodeWlSurfaceCommit)
  pure (SurfaceObjects surface layerSurface)

layerSurfaceInput :: LayerShell.ZwlrLayerSurfaceV1Event fd -> FixtureInput
layerSurfaceInput = \case
  LayerShell.ZwlrLayerSurfaceV1Configure serial width _ -> InputConfigure serial width
  LayerShell.ZwlrLayerSurfaceV1Closed -> InputClosed

layerNamespace :: Text
layerNamespace = "seki-layer-surface-fixture"

topEdgeAnchor :: LayerShell.ZwlrLayerSurfaceV1AnchorFlags
topEdgeAnchor =
  LayerShell.ZwlrLayerSurfaceV1AnchorFlags
    ( LayerShell.zwlrLayerSurfaceV1AnchorFlagsValue LayerShell.ZwlrLayerSurfaceV1AnchorFlagsTop
        .|. LayerShell.zwlrLayerSurfaceV1AnchorFlagsValue LayerShell.ZwlrLayerSurfaceV1AnchorFlagsLeft
        .|. LayerShell.zwlrLayerSurfaceV1AnchorFlagsValue LayerShell.ZwlrLayerSurfaceV1AnchorFlagsRight
    )

type ExceptIO = ExceptT FixtureError IO

fixtureStep :: (errorValue -> FixtureError) -> IO (Either errorValue value) -> ExceptIO value
fixtureStep wrap = withExceptT wrap . ExceptT

connectionStep :: IO (Either ConnectionError value) -> ExceptIO value
connectionStep = fixtureStep FixtureConnectionError
