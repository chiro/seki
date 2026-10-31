{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seki.Fixture.Probe (
  Probe,
  ProbeWindow,
  Color (..),
  Image (..),
  KeyboardSnapshot (..),
  PointerSnapshot (..),
  AppIdRequest (..),
  CommitSize (..),
  WindowSpec (..),
  windowSpec,
  withProbe,
  openWindow,
  openWindowWith,
  openWindowPending,
  keyboardSnapshot,
  committedSizes,
  pointerSnapshot,
  setTitle,
  requestFullscreen,
  requestExitFullscreen,
  requestMove,
  captureOutput,
  captureLayout,
  outputNames,
  Button (..),
  VirtualPointer,
  withVirtualPointer,
  pointerMoveTo,
  pointerPress,
  pointerRelease,
  click,
) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (forM, join, unless, void, when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.IORef
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified
import Data.Word (Word32, Word64)
import Seki.Fixture.Protocol.ExtImageCaptureSourceV1 qualified as Source
import Seki.Fixture.Protocol.ExtImageCopyCaptureV1 qualified as Copy
import Seki.Fixture.Protocol.WlrVirtualPointerUnstableV1 qualified as Pointer
import Seki.Fixture.Protocol.XdgShell qualified as Xdg
import Seki.Protocol.Wayland qualified as W
import Seki.Run.Startup
import Seki.Wire.Arguments (Fixed, fixedRawValue)
import Seki.Wire.Binding (Interface (..), RequestEncoder)
import Seki.Wire.Connection
import Seki.Wire.Connection.ObjectTable (Object, defaultObjectLimit, objectId)
import Seki.Wire.Connection.Registry
import Seki.Wire.Transport
import System.IO (SeekMode (AbsoluteSeek))
import System.IO.Error (isAlreadyExistsError)
import System.Posix.IO (dup, fdSeek)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Process (getProcessID)
import System.Posix.SharedMem
import System.Posix.Types (Fd)
import System.Timeout (timeout)

newtype ProbeWindow = ProbeWindow Int deriving (Eq, Ord, Show)
data Color = Red | Green | Blue deriving (Eq, Ord, Show)
data Image = Image {imageWidth :: Int, imageHeight :: Int, imageRgb :: ByteString} deriving (Eq, Show)
data KeyboardSnapshot = KeyboardSnapshot
  { keyboardOwner :: Maybe ProbeWindow
  , pressedKeys :: Map ProbeWindow Word64
  , depressedModifiers :: Word32
  }
  deriving (Eq, Show)

data AppIdRequest = NumberedAppId | CustomAppId Text | NoAppId deriving (Eq, Show)
data CommitSize = CommitConfigured | CommitFixed Int32 Int32 deriving (Eq, Show)
data WindowSpec = WindowSpec
  { specColor :: Color
  , specAppId :: AppIdRequest
  , specParent :: Maybe ProbeWindow
  , specMinSize :: Maybe (Int32, Int32)
  , specMaxSize :: Maybe (Int32, Int32)
  , specCommit :: CommitSize
  }
  deriving (Eq, Show)

windowSpec :: Color -> WindowSpec
windowSpec color = WindowSpec color NumberedAppId Nothing Nothing Nothing CommitConfigured

data PointerSnapshot = PointerSnapshot
  { pointerOwner :: Maybe ProbeWindow
  , surfacePosition :: Maybe (Double, Double)
  , pointerPresses :: Map ProbeWindow Word64
  , pointerReleases :: Map ProbeWindow Word64
  }
  deriving (Eq, Show)

data Probe = Probe (Chan Input) (MVar (Either SomeException ())) (IORef KeyboardSnapshot) (IORef PointerSnapshot) (IORef (Map ProbeWindow (Int32, Int32)))
data ToplevelRequest = SetFullscreen | UnsetFullscreen | Move | SetTitle Text
data Input
  = Open WindowSpec (MVar ProbeWindow)
  | Size ProbeWindow Int32 Int32
  | Configure ProbeWindow Word32
  | Synced (MVar ProbeWindow) ProbeWindow
  | Keyboard (W.WlKeyboardEvent OwnedFd)
  | Pointer (W.WlPointerEvent OwnedFd)
  | Request ProbeWindow ToplevelRequest (MVar (Either String ()))
  | Seat (W.WlSeatEvent OwnedFd)
  | Ping Word32
  | Released (Object W.WlBuffer) OwnedFd
  | Ended String

data Window = Window
  { windowSurface :: Object W.WlSurface
  , windowXdg :: Object Xdg.XdgSurface
  , windowToplevel :: Object Xdg.XdgToplevel
  , windowColor :: Color
  , windowCommit :: CommitSize
  , windowSize :: (Int32, Int32)
  , windowPaintedSize :: Maybe (Int32, Int32)
  , windowReply :: Maybe (MVar ProbeWindow)
  }

data Actor = Actor
  { actorSession :: WaylandSession
  , actorInputs :: Chan Input
  , actorCompositor :: Object W.WlCompositor
  , actorShm :: Object W.WlShm
  , actorWm :: Object Xdg.XdgWmBase
  , actorSeat :: Object W.WlSeat
  , actorKeyboard :: IORef KeyboardSnapshot
  , actorPointer :: IORef PointerSnapshot
  , actorFiles :: IORef [OwnedFd]
  , actorCommitted :: IORef (Map ProbeWindow (Int32, Int32))
  }

data Loop = Loop
  { loopKeyboard :: Bool
  , loopPointer :: Bool
  , loopPressSerials :: Map ProbeWindow Word32
  , loopWindows :: Map ProbeWindow Window
  }

withProbe :: FilePath -> (Probe -> IO a) -> IO a
withProbe socketPath action = bracket (openSession socketPath) closeWaylandSession $ \session -> do
  inputs <- newChan
  snapshot <- newIORef (KeyboardSnapshot Nothing Map.empty 0)
  pointerState <- newIORef (PointerSnapshot Nothing Nothing Map.empty Map.empty)
  files <- newIORef []
  committed <- newIORef Map.empty
  finished <- newEmptyMVar
  dispatcherFinished <- newEmptyMVar
  let connection = waylandConnection session
  compositor <- bindOne session W.wlCompositorInterface 1 Nothing
  shm <- bindOne session W.wlShmInterface 1 (Just (eventRoute W.decodeWlShmEvent (\_ _ -> pure ())))
  wm <- bindOne session Xdg.xdgWmBaseInterface 1 (Just (eventRoute Xdg.decodeXdgWmBaseEvent (\_ (Xdg.XdgWmBasePing serial) -> writeChan inputs (Ping serial))))
  seat <- bindOne session W.wlSeatInterface 1 (Just (eventRoute W.decodeWlSeatEvent (\_ event -> writeChan inputs (Seat event))))
  let actor = Actor session inputs compositor shm wm seat snapshot pointerState files committed
      cleanupFiles = readIORef files >>= mapM_ (void . closeOwnedFd)
  bracket
    ( do
        dispatcher <-
          forkFinally
            (dispatchLoop connection inputs)
            ( \outcome -> do
                case outcome of Left exception -> writeChan inputs (Ended (displayException exception)); Right () -> pure ()
                putMVar dispatcherFinished ()
            )
        worker <- forkFinally (actorLoop actor (Loop False False Map.empty Map.empty)) (putMVar finished)
        pure (dispatcher, worker)
    )
    (\(dispatcher, worker) -> closeWaylandSession session >> killThread dispatcher >> killThread worker >> readMVar dispatcherFinished >> readMVar finished >> cleanupFiles)
    (\_ -> watchActor finished (action (Probe inputs finished snapshot pointerState committed)))

openWindow :: Probe -> Color -> IO ProbeWindow
openWindow probe = openWindowWith probe . windowSpec

openWindowWith :: Probe -> WindowSpec -> IO ProbeWindow
openWindowWith probe = join . openWindowPending probe

{- | Queue the window's requests behind every earlier probe request, and
return an action that waits up to ten seconds for its first configure.
-}
openWindowPending :: Probe -> WindowSpec -> IO (IO ProbeWindow)
openWindowPending (Probe inputs _ _ _ _) spec = do
  reply <- newEmptyMVar
  writeChan inputs (Open spec reply)
  pure (bounded "initial xdg configure" (readMVar reply))

watchActor :: MVar (Either SomeException ()) -> IO a -> IO a
watchActor finished action = do
  outcome <- newEmptyMVar
  let run task publish = do
        joined <- newEmptyMVar
        thread <- forkFinally task (\result -> void (tryPutMVar outcome (publish result)) `finally` putMVar joined ())
        pure (thread, joined)
      stop (thread, joined) = killThread thread >> readMVar joined
  bracket (run action Right) stop $ \_ ->
    bracket (run (readMVar finished >>= either throwIO pure) Left) stop $ \_ ->
      takeMVar outcome >>= \case
        Right result -> either throwIO pure result
        Left (Left exception) -> throwIO exception
        Left (Right ()) -> fail "probe actor stopped"

keyboardSnapshot :: Probe -> IO KeyboardSnapshot
keyboardSnapshot probe@(Probe _ _ snapshot _ _) = actorAlive probe >> readIORef snapshot

-- | The size each window last acked and committed.
committedSizes :: Probe -> IO (Map ProbeWindow (Int32, Int32))
committedSizes probe@(Probe _ _ _ _ committed) = actorAlive probe >> readIORef committed

pointerSnapshot :: Probe -> IO PointerSnapshot
pointerSnapshot probe@(Probe _ _ _ snapshot _) = actorAlive probe >> readIORef snapshot

actorAlive :: Probe -> IO ()
actorAlive (Probe _ finished _ _ _) =
  tryReadMVar finished >>= \case
    Just (Left exception) -> throwIO exception
    _ -> pure ()

setTitle :: Probe -> ProbeWindow -> Text -> IO ()
setTitle probe window title = request probe window (SetTitle title)

requestFullscreen :: Probe -> ProbeWindow -> IO ()
requestFullscreen probe window = request probe window SetFullscreen

requestExitFullscreen :: Probe -> ProbeWindow -> IO ()
requestExitFullscreen probe window = request probe window UnsetFullscreen

requestMove :: Probe -> ProbeWindow -> IO ()
requestMove probe window = request probe window Move

request :: Probe -> ProbeWindow -> ToplevelRequest -> IO ()
request probe@(Probe inputs _ _ _ _) window value = do
  actorAlive probe
  reply <- newEmptyMVar
  writeChan inputs (Request window value reply)
  bounded "toplevel request round trip" (readMVar reply) >>= either fail pure

actorLoop :: Actor -> Loop -> IO ()
actorLoop actor state =
  readChan (actorInputs actor) >>= \case
    Ended reason -> fail reason
    Released buffer descriptor -> do
      send W.wlBufferInterface buffer W.encodeWlBufferDestroy
      void (closeOwnedFd descriptor)
      readIORef (actorFiles actor) >>= filterMIO (fmap isRight . (`withOwnedFd` const (pure ()))) >>= writeIORef (actorFiles actor)
      continue state
    Ping serial -> send Xdg.xdgWmBaseInterface (actorWm actor) (Xdg.encodeXdgWmBasePong serial) >> continue state
    Seat (W.WlSeatCapabilities flags) -> do
      let advertised bit = W.wlSeatCapabilityFlagsValue flags .&. bit /= 0
      when (not (loopKeyboard state) && advertised 2) $
        void (checkedIO (submitInheritedObjectRequestWithRoute connection W.wlSeatInterface (actorSeat actor) W.wlKeyboardInterface (W.encodeWlSeatGetKeyboard . objectId) (eventRoute W.decodeWlKeyboardEvent (\_ event -> writeChan (actorInputs actor) (Keyboard event)))))
      -- A removed capability leaves the old wl_pointer inert, so a returning pointer needs a new one.
      when (not (loopPointer state) && advertised 1) $
        void (checkedIO (submitInheritedObjectRequestWithRoute connection W.wlSeatInterface (actorSeat actor) W.wlPointerInterface (W.encodeWlSeatGetPointer . objectId) (eventRoute W.decodeWlPointerEvent (\_ event -> writeChan (actorInputs actor) (Pointer event)))))
      continue state{loopKeyboard = loopKeyboard state || advertised 2, loopPointer = advertised 1}
    Seat _ -> continue state
    Keyboard event -> do
      case event of
        W.WlKeyboardKeymap _ descriptor _ -> void (closeOwnedFd descriptor)
        W.WlKeyboardEnter _ surface _ -> modifyIORef' (actorKeyboard actor) (\s -> s{keyboardOwner = windowOf surface})
        W.WlKeyboardLeave{} -> modifyIORef' (actorKeyboard actor) (\s -> s{keyboardOwner = Nothing})
        W.WlKeyboardModifiers _ depressed _ _ _ -> modifyIORef' (actorKeyboard actor) (\s -> s{depressedModifiers = depressed})
        W.WlKeyboardKey _ _ _ W.WlKeyboardKeyStateEnumPressed -> modifyIORef' (actorKeyboard actor) (\s -> s{pressedKeys = maybe (pressedKeys s) (\w -> Map.insertWith (+) w 1 (pressedKeys s)) (keyboardOwner s)})
        _ -> pure ()
      continue state
    Pointer event -> case event of
      W.WlPointerEnter _ surface x y -> pointer (\s -> s{pointerOwner = windowOf surface, surfacePosition = Just (surfaceLocal x, surfaceLocal y)}) >> continue state
      W.WlPointerLeave{} -> pointer (\s -> s{pointerOwner = Nothing, surfacePosition = Nothing}) >> continue state
      W.WlPointerMotion _ x y -> pointer (\s -> s{surfacePosition = Just (surfaceLocal x, surfaceLocal y)}) >> continue state
      W.WlPointerButton serial _ _ buttonState -> do
        owner <- pointerOwner <$> readIORef (actorPointer actor)
        let pressed = buttonState == W.WlPointerButtonStateEnumPressed
            counted field s = maybe (field s) (\w -> Map.insertWith (+) w 1 (field s)) owner
        pointer (\s -> if pressed then s{pointerPresses = counted pointerPresses s} else s{pointerReleases = counted pointerReleases s})
        continue state{loopPressSerials = if pressed then maybe (loopPressSerials state) (\w -> Map.insert w serial (loopPressSerials state)) owner else loopPressSerials state}
      _ -> continue state
    Request window value reply -> do
      case Map.lookup window windows of
        Nothing -> putMVar reply (Left ("request for unknown probe window " <> show window))
        Just w -> do
          let toplevel encoder = send Xdg.xdgToplevelInterface (windowToplevel w) encoder >> sync session (putMVar reply (Right ()))
          case value of
            SetTitle title -> toplevel (Xdg.encodeXdgToplevelSetTitle title)
            SetFullscreen -> toplevel (Xdg.encodeXdgToplevelSetFullscreen Nothing)
            UnsetFullscreen -> toplevel Xdg.encodeXdgToplevelUnsetFullscreen
            Move -> maybe (putMVar reply (Left ("move needs a pointer press on " <> show window))) (toplevel . Xdg.encodeXdgToplevelMove (objectId (actorSeat actor))) (Map.lookup window (loopPressSerials state))
      continue state
    Open spec reply -> do
      let window = ProbeWindow (Map.size windows + 1)
      surface <- checkedIO (submitInheritedObjectRequestWithRoute connection W.wlCompositorInterface (actorCompositor actor) W.wlSurfaceInterface (W.encodeWlCompositorCreateSurface . objectId) (eventRoute W.decodeWlSurfaceEvent (\_ _ -> pure ())))
      xdg <- checkedIO (submitInheritedObjectRequestWithRoute connection Xdg.xdgWmBaseInterface (actorWm actor) Xdg.xdgSurfaceInterface (\obj -> Xdg.encodeXdgWmBaseGetXdgSurface (objectId obj) (objectId surface)) (eventRoute Xdg.decodeXdgSurfaceEvent (\_ (Xdg.XdgSurfaceConfigure serial) -> writeChan (actorInputs actor) (Configure window serial))))
      top <-
        checkedIO
          ( submitInheritedObjectRequestWithRoute
              connection
              Xdg.xdgSurfaceInterface
              xdg
              Xdg.xdgToplevelInterface
              (Xdg.encodeXdgSurfaceGetToplevel . objectId)
              ( eventRoute
                  Xdg.decodeXdgToplevelEvent
                  ( \_ -> \case
                      Xdg.XdgToplevelConfigure width height _ -> writeChan (actorInputs actor) (Size window width height)
                      Xdg.XdgToplevelClose -> writeChan (actorInputs actor) (Ended "fixture window unexpectedly closed")
                      _ -> pure ()
                  )
              )
          )
      let toplevel = send Xdg.xdgToplevelInterface top
      case specAppId spec of
        NumberedAppId -> toplevel (Xdg.encodeXdgToplevelSetAppId (Data.Text.pack ("seki-color-probe-" <> show (Map.size windows + 1))))
        CustomAppId appId -> toplevel (Xdg.encodeXdgToplevelSetAppId appId)
        NoAppId -> pure ()
      mapM_ (\parent -> maybe (fail "unknown probe parent window") (toplevel . Xdg.encodeXdgToplevelSetParent . Just . objectId . windowToplevel) (Map.lookup parent windows)) (specParent spec)
      mapM_ (toplevel . uncurry Xdg.encodeXdgToplevelSetMinSize) (specMinSize spec)
      mapM_ (toplevel . uncurry Xdg.encodeXdgToplevelSetMaxSize) (specMaxSize spec)
      send W.wlSurfaceInterface surface W.encodeWlSurfaceCommit
      continue state{loopWindows = Map.insert window (Window surface xdg top (specColor spec) (specCommit spec) (100, 100) Nothing (Just reply)) windows}
    Size window width height -> continue state{loopWindows = Map.adjust (\w -> w{windowSize = if width > 0 && height > 0 then (width, height) else windowSize w}) window windows}
    Configure window serial -> case Map.lookup window windows of
      Nothing -> fail "configure for unknown probe window"
      Just w -> do
        let (width, height) = case windowCommit w of CommitConfigured -> windowSize w; CommitFixed fixedWidth fixedHeight -> (fixedWidth, fixedHeight)
        send Xdg.xdgSurfaceInterface (windowXdg w) (Xdg.encodeXdgSurfaceAckConfigure serial)
        when (windowPaintedSize w /= Just (width, height)) $ do
          (_, buffer, _) <- makeBuffer connection (actorShm actor) (actorFiles actor) width height W.WlShmFormatEnumXrgb8888 (colorBytes (windowColor w)) (\buffer descriptor -> writeChan (actorInputs actor) (Released buffer descriptor))
          send W.wlSurfaceInterface (windowSurface w) (W.encodeWlSurfaceAttach (Just (objectId buffer)) 0 0)
          send W.wlSurfaceInterface (windowSurface w) (W.encodeWlSurfaceDamage 0 0 width height)
        send W.wlSurfaceInterface (windowSurface w) W.encodeWlSurfaceCommit
        modifyIORef' (actorCommitted actor) (Map.insert window (width, height))
        mapM_ (\reply -> sync session (writeChan (actorInputs actor) (Synced reply window))) (windowReply w)
        continue state{loopWindows = Map.insert window w{windowReply = Nothing, windowPaintedSize = Just (width, height)} windows}
    Synced reply window -> putMVar reply window >> continue state
 where
  session = actorSession actor
  connection = waylandConnection session
  windows = loopWindows state
  windowOf surface = fst <$> Map.lookupMin (Map.filter ((== surface) . objectId . windowSurface) windows)
  pointer = modifyIORef' (actorPointer actor)
  send interface object = checkedIO . submitRequest connection interface object
  continue = actorLoop actor

surfaceLocal :: Fixed -> Double
surfaceLocal = (/ 256) . fromIntegral . fixedRawValue

colorBytes :: Color -> ByteString
colorBytes Red = BS.pack [0, 0, 255, 255]
colorBytes Green = BS.pack [0, 255, 0, 255]
colorBytes Blue = BS.pack [255, 0, 0, 255]

dispatchLoop :: Connection -> Chan Input -> IO ()
dispatchLoop connection inputs =
  dispatchNextEvent connection >>= \case
    Left errorValue -> writeChan inputs (Ended (show errorValue))
    Right DispatchFinished -> writeChan inputs (Ended "probe connection closed")
    Right (EventDispatched _) -> dispatchLoop connection inputs

outputNames :: FilePath -> IO [(Word32, Text)]
outputNames socketPath = bounded "discover named outputs" $
  bracket (openSession socketPath) closeWaylandSession $ \session -> do
    globals <- catalogGlobals session W.wlOutputInterface 4
    names <- forM globals $ \global -> do
      name <- newIORef Nothing
      _ <-
        checkedIO
          ( bindGlobalWithRoute
              (waylandConnection session)
              (waylandRegistry session)
              global
              ( eventRoute
                  W.decodeWlOutputEvent
                  ( \_ -> \case
                      W.WlOutputName value -> writeIORef name (Just value)
                      _ -> pure ()
                  )
              )
          )
      pure (globalNameToWord (globalName global), name)
    roundtrip session
    forM names $ \(global, name) -> do
      value <- readIORef name >>= maybe (fail "wl_output omitted name") pure
      pure (global, value)

data Button = LeftButton | RightButton deriving (Eq, Show)

data VirtualPointer = VirtualPointer WaylandSession (Object Pointer.ZwlrVirtualPointerV1) (Word32, Word32)

withVirtualPointer :: FilePath -> (Word32, Word32) -> (VirtualPointer -> IO a) -> IO a
withVirtualPointer socketPath extent action = bracket (openSession socketPath) closeWaylandSession $ \session -> do
  let connection = waylandConnection session
  manager <- bindOne session Pointer.zwlrVirtualPointerManagerV1Interface 1 Nothing
  pointer <- checkedIO (submitInheritedObjectRequest connection Pointer.zwlrVirtualPointerManagerV1Interface manager Pointer.zwlrVirtualPointerV1Interface (Pointer.encodeZwlrVirtualPointerManagerV1CreateVirtualPointer Nothing . objectId))
  result <- action (VirtualPointer session pointer extent)
  checkedIO (submitRequest connection Pointer.zwlrVirtualPointerV1Interface pointer Pointer.encodeZwlrVirtualPointerV1Destroy)
  bounded "destroy virtual pointer" (roundtrip session)
  pure result

pointerMoveTo :: VirtualPointer -> (Word32, Word32) -> IO ()
pointerMoveTo pointer@(VirtualPointer _ _ (width, height)) (x, y) = pointerFrame pointer ("pointer motion to " <> show (x, y)) (Pointer.encodeZwlrVirtualPointerV1MotionAbsolute 0 x y width height)

pointerPress :: VirtualPointer -> Button -> IO ()
pointerPress pointer button = pointerFrame pointer ("press " <> show button) (Pointer.encodeZwlrVirtualPointerV1Button 0 (buttonCode button) W.WlPointerButtonStateEnumPressed)

pointerRelease :: VirtualPointer -> Button -> IO ()
pointerRelease pointer button = pointerFrame pointer ("release " <> show button) (Pointer.encodeZwlrVirtualPointerV1Button 0 (buttonCode button) W.WlPointerButtonStateEnumReleased)

pointerFrame :: VirtualPointer -> String -> RequestEncoder OwnedFd -> IO ()
pointerFrame (VirtualPointer session pointer _) phase event = bounded phase $ do
  let send = checkedIO . submitRequest (waylandConnection session) Pointer.zwlrVirtualPointerV1Interface pointer
  send event
  send Pointer.encodeZwlrVirtualPointerV1Frame
  roundtrip session

buttonCode :: Button -> Word32
buttonCode LeftButton = 0x110
buttonCode RightButton = 0x111

{- | Press and release the left button of a fresh virtual pointer at a point
of the output layout, given with the layout's width and height.
-}
click :: FilePath -> (Word32, Word32) -> (Word32, Word32) -> IO ()
click socketPath extent point = withVirtualPointer socketPath extent $ \pointer -> do
  pointerMoveTo pointer point
  pointerPress pointer LeftButton
  pointerRelease pointer LeftButton

captureOutput :: FilePath -> Text -> IO Image
captureOutput socketPath outputName = bounded ("capture " <> show outputName) $
  bracket (openSession socketPath) closeWaylandSession $ \session -> do
    files <- newIORef []
    capture session files `finally` (readIORef files >>= mapM_ (void . closeOwnedFd))
 where
  capture session files = do
    let connection = waylandConnection session
    outputs <- catalogGlobals session W.wlOutputInterface 4
    named <- forM outputs $ \global -> do
      name <- newIORef Nothing
      normal <- newIORef True
      output <-
        checkedIO
          ( bindGlobalWithRoute
              connection
              (waylandRegistry session)
              global
              ( eventRoute
                  W.decodeWlOutputEvent
                  ( \_ -> \case
                      W.WlOutputName value -> writeIORef name (Just value)
                      W.WlOutputScale scale -> when (scale /= 1) (writeIORef normal False)
                      W.WlOutputGeometry _ _ _ _ _ _ _ transform -> when (transform /= W.WlOutputTransformEnumNormal) (writeIORef normal False)
                      _ -> pure ()
                  )
              )
          )
      pure (output, name, normal)
    roundtrip session
    matches <- filterMIO (\(_, name, _) -> (== Just outputName) <$> readIORef name) named
    output <- case matches of
      [(object, _, normal)] -> readIORef normal >>= \ok -> if ok then pure object else fail "capture requires unit scale and normal transform"
      _ -> fail ("named capture output unavailable: " <> show outputName)
    shm <- bindOne session W.wlShmInterface 1 (Just (eventRoute W.decodeWlShmEvent (\_ _ -> pure ())))
    sourceManager <- bindOne session Source.extOutputImageCaptureSourceManagerV1Interface 1 Nothing
    copyManager <- bindOne session Copy.extImageCopyCaptureManagerV1Interface 1 Nothing
    source <- checkedIO (submitInheritedObjectRequest connection Source.extOutputImageCaptureSourceManagerV1Interface sourceManager Source.extImageCaptureSourceV1Interface (\obj -> Source.encodeExtOutputImageCaptureSourceManagerV1CreateSource (objectId obj) (objectId output)))
    constraints <- newIORef (Nothing, [] :: [W.WlShmFormatEnum], False)
    stopped <- newIORef False
    copySession <-
      checkedIO
        ( submitInheritedObjectRequestWithRoute
            connection
            Copy.extImageCopyCaptureManagerV1Interface
            copyManager
            Copy.extImageCopyCaptureSessionV1Interface
            (\obj -> Copy.encodeExtImageCopyCaptureManagerV1CreateSession (objectId obj) (objectId source) (Copy.ExtImageCopyCaptureManagerV1OptionsFlags 0))
            ( eventRoute
                Copy.decodeExtImageCopyCaptureSessionV1Event
                ( \_ -> \case
                    Copy.ExtImageCopyCaptureSessionV1BufferSize width height -> modifyIORef' constraints (\(_, formats, done) -> (Just (width, height), formats, done))
                    Copy.ExtImageCopyCaptureSessionV1ShmFormat format -> modifyIORef' constraints (\(size, formats, done) -> (size, format : formats, done))
                    Copy.ExtImageCopyCaptureSessionV1Done -> modifyIORef' constraints (\(size, formats, _) -> (size, formats, True))
                    Copy.ExtImageCopyCaptureSessionV1Stopped -> writeIORef stopped True
                    _ -> pure ()
                )
            )
        )
    dispatchUntil connection $ do
      (_, _, done) <- readIORef constraints
      readIORef stopped >>= \s -> when s (fail "capture session stopped")
      pure done
    (size, formats, _) <- readIORef constraints
    (width, height) <- maybe (fail "capture omitted buffer size") pure size
    (format, toRgb) <- maybe (fail ("capture has no supported SHM format: " <> show formats)) pure (captureLayout formats)
    (_, buffer, descriptor) <- makeBuffer connection shm files (fromIntegral width) (fromIntegral height) format (BS.pack [0, 0, 0, 0]) (\_ _ -> pure ())
    result <- newIORef Nothing
    transform <- newIORef Nothing
    frame <-
      checkedIO
        ( submitInheritedObjectRequestWithRoute
            connection
            Copy.extImageCopyCaptureSessionV1Interface
            copySession
            Copy.extImageCopyCaptureFrameV1Interface
            (Copy.encodeExtImageCopyCaptureSessionV1CreateFrame . objectId)
            ( eventRoute
                Copy.decodeExtImageCopyCaptureFrameV1Event
                ( \_ -> \case
                    Copy.ExtImageCopyCaptureFrameV1Ready -> writeIORef result (Just (Right ()))
                    Copy.ExtImageCopyCaptureFrameV1Failed reason -> writeIORef result (Just (Left (show reason)))
                    Copy.ExtImageCopyCaptureFrameV1Transform value -> writeIORef transform (Just value)
                    _ -> pure ()
                )
            )
        )
    let send = checkedIO . submitRequest connection Copy.extImageCopyCaptureFrameV1Interface frame
    send (Copy.encodeExtImageCopyCaptureFrameV1AttachBuffer (objectId buffer))
    send (Copy.encodeExtImageCopyCaptureFrameV1DamageBuffer 0 0 (fromIntegral width) (fromIntegral height))
    send Copy.encodeExtImageCopyCaptureFrameV1Capture
    dispatchUntil connection $ do
      readIORef stopped >>= \s -> when s (fail "capture session stopped")
      readIORef result >>= \case
        Nothing -> pure False
        Just (Left reason) -> fail ("capture frame failed: " <> reason)
        Just (Right ()) -> pure True
    readIORef transform >>= \t -> unless (t == Just W.WlOutputTransformEnumNormal) (fail ("unexpected capture transform: " <> show t))
    pixels <-
      checked
        =<< withOwnedFd
          descriptor
          ( \fd -> do
              _ <- fdSeek fd AbsoluteSeek 0
              readAll fd (fromIntegral width * fromIntegral height * 4)
          )
    pure (Image (fromIntegral width) (fromIntegral height) (toRgb pixels))

{- | The first offered SHM format the capture can read, in preference order,
with the conversion of its little-endian pixels to packed RGB.
-}
captureLayout :: [W.WlShmFormatEnum] -> Maybe (W.WlShmFormatEnum, ByteString -> ByteString)
captureLayout offered =
  case [(format, channels) | (format, channels) <- readable, format `elem` offered] of
    (format, channels) : _ -> Just (format, \pixels -> BS.pack [BS.index pixels (offset + channel) | offset <- [0, 4 .. BS.length pixels - 4], channel <- channels])
    [] -> Nothing
 where
  readable =
    [ (W.WlShmFormatEnumXrgb8888, [2, 1, 0])
    , (W.WlShmFormatEnumArgb8888, [2, 1, 0])
    , (W.WlShmFormatEnumXbgr8888, [0, 1, 2])
    , (W.WlShmFormatEnumAbgr8888, [0, 1, 2])
    ]

makeBuffer :: Connection -> Object W.WlShm -> IORef [OwnedFd] -> Int32 -> Int32 -> W.WlShmFormatEnum -> ByteString -> (Object W.WlBuffer -> OwnedFd -> IO ()) -> IO (Object W.WlShmPool, Object W.WlBuffer, OwnedFd)
makeBuffer connection shm files width height format pixel released = do
  live <- length <$> readIORef files
  when (live >= 128) (fail "fixture connection exceeded 128 live buffers")
  let size = toInteger width * toInteger height * 4
  unless (width > 0 && height > 0 && size <= 16 * 1024 * 1024) (fail ("invalid fixture buffer size: " <> show (width, height)))
  descriptor <- createPixelFile files (BS.concat (replicate (fromIntegral size `div` 4) pixel))
  wireDescriptor <- checked =<< withOwnedFd descriptor dup
  ownedWire <- adoptFd wireDescriptor
  pool <- checkedIO (submitInheritedObjectRequest connection W.wlShmInterface shm W.wlShmPoolInterface (\obj -> W.encodeWlShmCreatePool (objectId obj) ownedWire (fromIntegral size))) `finally` void (closeOwnedFd ownedWire)
  buffer <- checkedIO (submitInheritedObjectRequestWithRoute connection W.wlShmPoolInterface pool W.wlBufferInterface (\obj -> W.encodeWlShmPoolCreateBuffer (objectId obj) 0 width height (width * 4) format) (eventRoute W.decodeWlBufferEvent (\buffer _ -> released buffer descriptor)))
  checkedIO (submitRequest connection W.wlShmPoolInterface pool W.encodeWlShmPoolDestroy)
  pure (pool, buffer, descriptor)

createPixelFile :: IORef [OwnedFd] -> ByteString -> IO OwnedFd
createPixelFile files pixels = do
  processId <- getProcessID
  let open attempt = do
        let name = "/seki-probe-" <> show processId <> "-" <> show attempt
        try (shmOpen name (ShmOpenFlags{shmReadWrite = True, shmCreate = True, shmExclusive = True, shmTrunc = False}) 0o600) >>= \case
          Right fd -> shmUnlink name >> pure fd
          Left (exception :: IOException)
            | isAlreadyExistsError exception -> open (attempt + 1 :: Int)
            | otherwise -> throwIO exception
  descriptor <- open 0 >>= adoptFd
  modifyIORef' files (descriptor :)
  checked =<< withOwnedFd descriptor (\fd -> writeAll fd pixels)
  pure descriptor
 where
  writeAll fd bytes = unless (BS.null bytes) $ do
    count <- fdWrite fd bytes
    when (count == 0) (fail "zero-length SHM write")
    writeAll fd (BS.drop (fromIntegral count) bytes)

readAll :: Fd -> Int -> IO ByteString
readAll _ 0 = pure BS.empty
readAll fd count = do
  bytes <- fdRead fd (fromIntegral count)
  when (BS.null bytes) (fail "short capture SHM read")
  (bytes <>) <$> readAll fd (count - BS.length bytes)

openSession :: FilePath -> IO WaylandSession
openSession socketPath = bounded "open fixture connection" (openWaylandSessionAt (UnixSocketPath socketPath) defaultObjectLimit >>= checked)

catalogGlobals :: WaylandSession -> Interface i -> Word32 -> IO [Global i]
catalogGlobals session interface version = readWaylandGlobalCatalog session >>= checked >>= checked . selectGlobals interface version

bindOne :: WaylandSession -> Interface i -> Word32 -> Maybe (EventRoute i) -> IO (Object i)
bindOne session interface version route = do
  globals <- catalogGlobals session interface version
  global <- case globals of [] -> fail ("missing fixture global: " <> show (interfaceName interface)); first : _ -> pure first
  checkedIO $ case route of
    Nothing -> bindGlobal connection registry global
    Just events -> bindGlobalWithRoute connection registry global events
 where
  connection = waylandConnection session
  registry = waylandRegistry session

sync :: WaylandSession -> IO () -> IO ()
sync session action = void $ checkedIO (submitNewObjectRequestWithRoute (waylandConnection session) W.wlDisplayInterface (waylandDisplay session) W.wlCallbackInterface 1 (W.encodeWlDisplaySync . objectId) (eventRoute W.decodeWlCallbackEvent (\_ _ -> action)))

roundtrip :: WaylandSession -> IO ()
roundtrip session = do
  done <- newIORef False
  sync session (writeIORef done True)
  dispatchUntil (waylandConnection session) (readIORef done)

dispatchUntil :: Connection -> IO Bool -> IO ()
dispatchUntil connection done =
  done >>= \ready -> unless ready $ do
    checkedIO (dispatchNextEvent connection) >>= \case
      DispatchFinished -> fail "fixture connection closed while waiting for event"
      EventDispatched _ -> dispatchUntil connection done

checked :: (Show e) => Either e a -> IO a
checked = either (fail . show) pure

bounded :: String -> IO a -> IO a
bounded phase action = timeout (10 * 1000 * 1000) action >>= maybe (fail ("timeout: " <> phase)) pure

filterMIO :: (a -> IO Bool) -> [a] -> IO [a]
filterMIO _ [] = pure []
filterMIO predicate (x : xs) = do yes <- predicate x; rest <- filterMIO predicate xs; pure (if yes then x : rest else rest)

checkedIO :: (Show e) => IO (Either e a) -> IO a
checkedIO action = action >>= checked
