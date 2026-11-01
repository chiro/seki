{-# LANGUAGE LambdaCase #-}

module Seki.Run.Input (
  InputSessionIdentity (..),
  InputIdentity (..),
  InputGeneration (..),
  DeviceLifecycle (..),
  InputDevice (..),
  InputInventory (..),
  emptyInputInventory,
  discoverInput,
  updateInput,
  finishDiscovery,
  discoveryCohort,
  InputSession,
  openInputSession,
  readInputInventory,
  refreshInputOutputs,
  SettingOutcome (..),
  SettingResult (..),
  GenericRequest (..),
  planGeneric,
) where

import Control.Monad (void, when)
import Data.IORef
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Internal.Input
import Seki.Protocol.RiverInputManagementV1 qualified as Input
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Startup
import Seki.Wire.Arguments (fixedFromRaw)
import Seki.Wire.Connection
import Seki.Wire.Connection.ObjectTable (Object, objectId, objectVersion)
import Seki.Wire.Connection.Registry
import Seki.Wire.Types (ObjectId)
import System.Posix.Process (getProcessID)

data InputSessionIdentity = InputSessionIdentity Int Word64 Int deriving (Eq, Ord, Show)

data InputIdentity = InputIdentity InputSessionIdentity InputGeneration deriving (Eq, Ord, Show)

newtype InputGeneration = InputGeneration Integer deriving (Eq, Ord, Show)
data DeviceLifecycle = Discovering | Ready | Removed deriving (Eq, Show)
data InputDevice = InputDevice
  { inputIdentity :: InputIdentity
  , inputGeneration :: InputGeneration
  , inputObject :: Object Input.RiverInputDeviceV1
  , inputLifecycle :: DeviceLifecycle
  , inputName :: Maybe Text
  , inputType :: Maybe DeviceType
  , inputDefaults :: Map SettingKey InputSetting
  , inputCurrent :: Map SettingKey InputSetting
  , inputResults :: Map SettingKey SettingResult
  }
  deriving (Eq, Show)
data InputInventory = InputInventory
  { inputSessionIdentity :: InputSessionIdentity
  , inputNextGeneration :: Integer
  , inputDevices :: Map InputGeneration InputDevice
  }
  deriving (Eq, Show)

emptyInputInventory :: InputInventory
emptyInputInventory = InputInventory (InputSessionIdentity 0 0 0) 0 Map.empty

discoverInput :: Object Input.RiverInputDeviceV1 -> InputInventory -> InputInventory
discoverInput object inventory =
  inventory
    { inputNextGeneration = next + 1
    , inputDevices = Map.insert generation (InputDevice (InputIdentity (inputSessionIdentity inventory) generation) generation object Discovering Nothing Nothing Map.empty Map.empty Map.empty) (inputDevices inventory)
    }
 where
  next = inputNextGeneration inventory
  generation = InputGeneration next

-- | Capture identities, never reusable wire IDs, for a particular sync request.
discoveryCohort :: InputInventory -> [InputGeneration]
discoveryCohort = Map.keys . Map.filter (\d -> inputLifecycle d == Discovering && objectVersion (inputObject d) == 1) . inputDevices

finishDiscovery :: [InputGeneration] -> InputInventory -> InputInventory
finishDiscovery cohort inventory = inventory{inputDevices = foldr (Map.adjust ready) (inputDevices inventory) cohort}
 where
  ready d
    | inputLifecycle d == Discovering, Just _ <- inputName d, Just _ <- inputType d = d{inputLifecycle = Ready}
    | otherwise = d

updateInput :: InputGeneration -> Input.RiverInputDeviceV1Event fd -> InputInventory -> InputInventory
updateInput generation event inventory = inventory{inputDevices = Map.adjust update generation (inputDevices inventory)}
 where
  update d | inputLifecycle d == Removed = d
  update d = case event of
    Input.RiverInputDeviceV1Removed -> d{inputLifecycle = Removed, inputResults = fmap (\r -> r{settingOutcome = SettingRemoved}) (inputResults d)}
    Input.RiverInputDeviceV1Name name -> d{inputName = Just name}
    Input.RiverInputDeviceV1Type kind -> d{inputType = deviceType kind}
    Input.RiverInputDeviceV1Done -> case (inputName d, inputType d) of
      (Just _, Just _) -> d{inputLifecycle = Ready}
      _ -> d
  deviceType = \case
    Input.RiverInputDeviceV1TypeEnumKeyboard -> Just Keyboard
    Input.RiverInputDeviceV1TypeEnumPointer -> Just Pointer
    Input.RiverInputDeviceV1TypeEnumTouch -> Just Touch
    Input.RiverInputDeviceV1TypeEnumTablet -> Just Tablet
    _ -> Nothing

data SettingOutcome = SettingPending | SettingSent | SettingUnavailable | SettingUnsupported | SettingInvalid SettingError | SettingRemoved deriving (Eq, Show)
data SettingResult = SettingResult
  { settingDesired :: InputSetting
  , settingTarget :: Maybe ObjectId
  , settingOutcome :: SettingOutcome
  }
  deriving (Eq, Show)
data GenericRequest
  = GenericRepeat InputGeneration Int32 Int32
  | GenericScroll InputGeneration Int32
  | GenericMapOutput InputGeneration (Maybe ObjectId)
  | GenericMapRectangle InputGeneration InputRectangle
  deriving (Eq, Show)

{- | Sent means encoded and queued, never compositor application. Generic input
management has no current/default events or result objects.
-}
planGeneric :: [InputRule] -> Map Text ObjectId -> InputInventory -> (InputInventory, [GenericRequest])
planGeneric rules outputs inventory =
  let (requests, devices) = Map.mapAccumWithKey planDevice [] (inputDevices inventory)
   in (inventory{inputDevices = devices}, requests)
 where
  planDevice requests generation device
    | inputLifecycle device /= Ready = (requests, device)
    | Just name <- inputName device
    , Just kind <- inputType device =
        let desired = resolveInputRules rules name kind
            results = Map.mapWithKey preserveSent (Map.map (classify kind) desired)
            preserveSent key new = case Map.lookup key (inputResults device) of
              Just old | settingOutcome new == SettingPending, settingOutcome old == SettingSent, settingDesired old == settingDesired new, settingTarget old == settingTarget new -> old
              _ -> new
            changed = Map.differenceWith (\new old -> if new == old then Nothing else Just new) results (inputResults device)
            sends = concatMap (requestFor generation) [r | r <- Map.elems changed, settingOutcome r == SettingPending]
         in (requests <> sends, device{inputResults = results})
    | otherwise = (requests, device)
  requestFor generation r = case settingDesired r of
    RepeatInfo rate delay -> [GenericRepeat generation rate delay]
    ScrollFactor factor -> [GenericScroll generation (round (factor * 256))]
    MapOutput _ -> [GenericMapOutput generation (settingTarget r)]
    MapRectangle rectangle -> [GenericMapRectangle generation (maybe (InputRectangle 0 0 0 0) id rectangle)]
    _ -> []
  classify kind setting = case validateInputSetting setting of
    Left errorValue -> SettingResult setting Nothing (SettingInvalid errorValue)
    Right () -> case setting of
      RepeatInfo _ _ -> result (kind == Keyboard)
      ScrollFactor _ -> result (kind == Pointer)
      MapRectangle _ -> result (kind /= Keyboard)
      MapOutput target
        | kind == Keyboard -> result False
        | Just name <- target -> case Map.lookup name outputs of
            Nothing -> SettingResult setting Nothing SettingUnavailable
            Just output -> SettingResult setting (Just output) SettingPending
        | otherwise -> result True
      _ -> SettingResult setting Nothing SettingUnavailable
     where
      result supported = SettingResult setting Nothing (if supported then SettingPending else SettingUnsupported)

data InputSession = InputSession
  { inputState :: IORef InputInventory
  , inputOutputs :: IORef (Map Text ObjectId)
  , inputApply :: IO ()
  }

refreshInputOutputs :: InputSession -> Map Text ObjectId -> IO ()
refreshInputOutputs session outputs = writeIORef (inputOutputs session) outputs >> inputApply session
readInputInventory :: InputSession -> IO InputInventory
readInputInventory = readIORef . inputState

-- | Optional discovery; absence is distinct from an empty device inventory.
openInputSession :: [InputRule] -> WaylandSession -> (ConnectionError -> IO ()) -> IO (Either ConnectionError (Maybe InputSession))
openInputSession rules wayland failed = do
  catalog <- readWaylandGlobalCatalog wayland
  case catalog of
    Left _ -> pure (Right Nothing)
    Right globals -> case selectGlobals Input.riverInputManagerV1Interface 2 globals of
      Left _ -> pure (Right Nothing)
      Right [] -> pure (Right Nothing)
      Right (global : _) -> do
        identity <- InputSessionIdentity <$> (fromIntegral <$> getProcessID) <*> getMonotonicTimeNSec <*> (hashUnique <$> newUnique)
        state <- newIORef emptyInputInventory{inputSessionIdentity = identity}
        outputs <- newIORef Map.empty
        let session = InputSession state outputs apply
            apply = do
              inventory <- readIORef state
              targets <- readIORef outputs
              let (next, requests) = planGeneric rules targets inventory
              writeIORef state next
              mapM_ send requests
            send command = do
              let (generation, key, request) = case command of
                    GenericRepeat gen rate delay -> (gen, RepeatInfoKey, Input.encodeRiverInputDeviceV1SetRepeatInfo rate delay)
                    GenericScroll gen raw -> (gen, ScrollFactorKey, Input.encodeRiverInputDeviceV1SetScrollFactor (fixedFromRaw raw))
                    GenericMapOutput gen target -> (gen, MapOutputKey, Input.encodeRiverInputDeviceV1MapToOutput target)
                    GenericMapRectangle gen (InputRectangle x y width height) -> (gen, MapRectangleKey, Input.encodeRiverInputDeviceV1MapToRectangle x y width height)
              inventory <- readIORef state
              case Map.lookup generation (inputDevices inventory) of
                Just device
                  | inputLifecycle device == Ready ->
                      submitRequest connection Input.riverInputDeviceV1Interface (inputObject device) request >>= \case
                        Left e -> failed e
                        Right () -> modifyIORef' state (\inventoryValue -> inventoryValue{inputDevices = Map.adjust (\d -> d{inputResults = Map.adjust (\r -> r{settingOutcome = SettingSent}) key (inputResults d)}) generation (inputDevices inventoryValue)})
                _ -> pure ()
            connection = waylandConnection wayland
            result = either failed (const (pure ()))
            created object = do
              modifyIORef' state (discoverInput object)
              when (objectVersion object == 1) $ do
                cohort <- discoveryCohort <$> readIORef state
                void
                  ( submitNewObjectRequestWithRoute
                      connection
                      Wayland.wlDisplayInterface
                      (waylandDisplay wayland)
                      Wayland.wlCallbackInterface
                      1
                      (Wayland.encodeWlDisplaySync . objectId)
                      (eventRoute Wayland.decodeWlCallbackEvent (\_ _ -> modifyIORef' state (finishDiscovery cohort) >> apply))
                      >>= either failed (const (pure ()))
                  )
            handle object event = do
              inventory <- readIORef state
              let identities = Map.keys (Map.filter (\d -> inputObject d == object && inputLifecycle d /= Removed) (inputDevices inventory))
              mapM_ (\generation -> modifyIORef' state (updateInput generation event)) identities
              apply
              case event of
                Input.RiverInputDeviceV1Removed -> submitRequest connection Input.riverInputDeviceV1Interface object Input.encodeRiverInputDeviceV1Destroy >>= result
                _ -> pure ()
            route =
              eventRouteWithServerObjects
                Input.decodeRiverInputManagerV1Event
                ( \case
                    Input.RiverInputManagerV1InputDevice new -> [serverObjectRoute new Input.riverInputDeviceV1Interface (eventRoute Input.decodeRiverInputDeviceV1Event handle) (void . created)]
                    _ -> []
                )
                ( \manager -> \case
                    Input.RiverInputManagerV1Finished -> do
                      inventory <- readIORef state
                      mapM_
                        ( \d -> when (inputLifecycle d /= Removed) $ do
                            modifyIORef' state (updateInput (inputGeneration d) Input.RiverInputDeviceV1Removed)
                            submitRequest connection Input.riverInputDeviceV1Interface (inputObject d) Input.encodeRiverInputDeviceV1Destroy >>= result
                        )
                        (Map.elems (inputDevices inventory))
                      submitRequest connection Input.riverInputManagerV1Interface manager Input.encodeRiverInputManagerV1Destroy >>= result
                    _ -> pure ()
                )
        fmap (const (Just session)) <$> bindGlobalWithRoute connection (waylandRegistry wayland) global route
