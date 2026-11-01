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
  planInput,
  acknowledgeLibinput,
  finishFacetDiscovery,
) where

import Control.Exception (onException)
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
import Seki.Protocol.RiverLibinputConfigV1 qualified as L
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.Libinput
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
  , inputLibinput :: LibinputFacet
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
    , inputDevices = Map.insert generation (InputDevice (InputIdentity (inputSessionIdentity inventory) generation) generation object Discovering Nothing Nothing Map.empty Map.empty (emptyFacet FacetUnavailable) Map.empty) (inputDevices inventory)
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
    Input.RiverInputDeviceV1Removed -> d{inputLifecycle = Removed, inputLibinput = emptyFacet FacetRemoved, inputResults = fmap (\r -> r{settingOutcome = SettingRemoved}) (inputResults d)}
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

data SettingOutcome = SettingPending | SettingAwaiting | SettingApplied | SettingRejectedInvalid | SettingSent | SettingUnavailable | SettingUnsupported | SettingInvalid SettingError | SettingRemoved deriving (Eq, Show)
data SettingResult = SettingResult
  { settingDesired :: InputSetting
  , settingRevision :: Integer
  , settingFacet :: Maybe FacetGeneration
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
planInput :: [InputRule] -> Map Text ObjectId -> InputInventory -> (InputInventory, [GenericRequest])
planInput rules outputs inventory =
  let (requests, devices) = Map.mapAccumWithKey planDevice [] (inputDevices inventory)
   in (inventory{inputDevices = devices}, requests)
 where
  planDevice requests generation device
    | inputLifecycle device /= Ready = (requests, device)
    | Just name <- inputName device
    , Just kind <- inputType device =
        let desired = resolveInputRules rules name kind
            results = Map.mapWithKey preserveSent (Map.map (classifyDevice device kind) desired)
            preserveSent key new = case Map.lookup key (inputResults device) of
              Just old | settingDesired old == settingDesired new, settingTarget old == settingTarget new, settingFacet old == settingFacet new, settingOutcome new == SettingPending, settingOutcome old `notElem` [SettingUnavailable, SettingRemoved] -> old
              Just old | settingDesired old == settingDesired new, settingTarget old == settingTarget new, settingFacet old == settingFacet new, settingOutcome old == settingOutcome new -> old
              Just old -> new{settingRevision = settingRevision old + 1}
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
  classifyDevice device kind setting
    | libinputKey (settingKey setting) = SettingResult setting 0 (facetGeneration (inputLibinput device)) (objectId <$> facetObject (inputLibinput device)) $ case validateInputSetting setting of
        Left e -> SettingInvalid e
        Right () -> case facetLifecycle (inputLibinput device) of
          FacetUnavailable -> SettingUnavailable
          FacetRemoved -> SettingUnavailable
          FacetDiscovering -> SettingPending
          FacetReady -> case supportsSetting (inputLibinput device) setting of
            Nothing -> SettingPending
            Just False -> SettingUnsupported
            Just True -> SettingPending
    | otherwise = classify kind setting
  classify kind setting = case validateInputSetting setting of
    Left errorValue -> SettingResult setting 0 Nothing Nothing (SettingInvalid errorValue)
    Right () -> case setting of
      RepeatInfo _ _ -> result (kind == Keyboard)
      ScrollFactor _ -> result (kind == Pointer)
      MapRectangle _ -> result (kind /= Keyboard)
      MapOutput target
        | kind == Keyboard -> result False
        | Just name <- target -> case Map.lookup name outputs of
            Nothing -> SettingResult setting 0 Nothing Nothing SettingUnavailable
            Just output -> SettingResult setting 0 Nothing (Just output) SettingPending
        | otherwise -> result True
      _ -> SettingResult setting 0 Nothing Nothing SettingUnavailable
     where
      result supported = SettingResult setting 0 Nothing Nothing (if supported then SettingPending else SettingUnsupported)

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
        libManager <- newIORef Nothing
        libEnabled <- newIORef False
        libRevision <- newIORef (0 :: Integer)
        let session = InputSession state outputs apply
            apply = do
              inventory <- readIORef state
              targets <- readIORef outputs
              let (next, requests) = planInput rules targets inventory
              writeIORef state next
              mapM_ send requests
              mapM_ (sendLibinput libManager libRevision state connection apply failed) (Map.elems (inputDevices next))
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
              enabled <- readIORef libEnabled
              modifyIORef' state (\inventory -> let new = discoverInput object inventory in new{inputDevices = Map.adjust (\d -> d{inputLibinput = emptyFacet (if enabled then FacetDiscovering else FacetUnavailable)}) (InputGeneration (inputNextGeneration inventory)) (inputDevices new)})
              when enabled $ do
                generation <- InputGeneration . subtract 1 . inputNextGeneration <$> readIORef state
                syncInput wayland failed $ do
                  modifyIORef' state (\i -> i{inputDevices = Map.adjust (\d -> if facetObject (inputLibinput d) == Nothing then d{inputLibinput = emptyFacet FacetUnavailable} else d) generation (inputDevices i)})
                  apply
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
                Input.RiverInputDeviceV1Removed -> do
                  mapM_ (\gen -> retireFacet connection failed (inputLibinput <$> Map.lookup gen (inputDevices inventory))) identities
                  submitRequest connection Input.riverInputDeviceV1Interface object Input.encodeRiverInputDeviceV1Destroy >>= result
                  modifyIORef' state (\i -> i{inputDevices = foldr Map.delete (inputDevices i) identities})
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
                            retireFacet connection failed (Just (inputLibinput d))
                            submitRequest connection Input.riverInputDeviceV1Interface (inputObject d) Input.encodeRiverInputDeviceV1Destroy >>= result
                        )
                        (Map.elems (inputDevices inventory))
                      submitRequest connection Input.riverInputManagerV1Interface manager Input.encodeRiverInputManagerV1Destroy >>= result
                    _ -> pure ()
                )
        bound <- bindGlobalWithRoute connection (waylandRegistry wayland) global route
        case bound of
          Left e -> pure (Left e)
          Right _ -> do
            bindLibinput wayland globals state libManager libEnabled apply failed
            pure (Right (Just session))

retireFacet :: Connection -> (ConnectionError -> IO ()) -> Maybe LibinputFacet -> IO ()
retireFacet connection failed facet = case facet >>= facetObject of
  Nothing -> pure ()
  Just object -> submitRequest connection L.riverLibinputDeviceV1Interface object L.encodeRiverLibinputDeviceV1Destroy >>= either failed pure

bindLibinput :: WaylandSession -> GlobalCatalog -> IORef InputInventory -> IORef (Maybe (Object L.RiverLibinputConfigV1)) -> IORef Bool -> IO () -> (ConnectionError -> IO ()) -> IO ()
bindLibinput wayland globals state managerRef enabled apply failed =
  case selectGlobals L.riverLibinputConfigV1Interface 2 globals of
    Right (global : _) -> do
      writeIORef enabled True
      serial <- newIORef (0 :: Integer)
      unassociated <- newIORef Map.empty
      let connection = waylandConnection wayland
          result = either failed (const (pure ()))
          findGeneration object inventory = Map.lookupMin (Map.filter (\d -> objectId (inputObject d) == object && inputLifecycle d /= Removed) (inputDevices inventory))
          route =
            eventRouteWithServerObjects
              L.decodeRiverLibinputConfigV1Event
              ( \case
                  L.RiverLibinputConfigV1LibinputDevice new ->
                    [serverObjectRoute new L.riverLibinputDeviceV1Interface (eventRoute L.decodeRiverLibinputDeviceV1Event handle) created]
                  _ -> []
              )
              ( \manager -> \case
                  L.RiverLibinputConfigV1Finished -> do
                    writeIORef managerRef Nothing
                    writeIORef enabled False
                    inventory <- readIORef state
                    mapM_ (retireFacet connection failed . Just . inputLibinput) (Map.elems (inputDevices inventory))
                    modifyIORef' state (\i -> i{inputDevices = fmap (\d -> d{inputLibinput = emptyFacet FacetUnavailable}) (inputDevices i)})
                    submitRequest connection L.riverLibinputConfigV1Interface manager L.encodeRiverLibinputConfigV1Destroy >>= result
                    apply
                  _ -> pure ()
              )
          created object = do
            token <- atomicModifyIORef' serial (\n -> (n + 1, FacetGeneration n))
            modifyIORef' unassociated (Map.insert (objectId object) token)
            when (objectVersion object == 1) $ syncInput wayland failed $ do
              modifyIORef' state (finishFacetDiscovery object token)
              apply
          handle object event = do
            inventory <- readIORef state
            case event of
              L.RiverLibinputDeviceV1InputDevice generic -> do
                token <- atomicModifyIORef' unassociated (\pending -> (Map.delete (objectId object) pending, Map.lookup (objectId object) pending))
                case findGeneration generic inventory of
                  Nothing -> submitRequest connection L.riverLibinputDeviceV1Interface object L.encodeRiverLibinputDeviceV1Destroy >>= result
                  Just (generation, _) -> modifyIORef' state (\i -> i{inputDevices = Map.adjust (\d -> d{inputLibinput = (emptyFacet FacetDiscovering){facetObject = Just object, facetGeneration = token}}) generation (inputDevices i)})
              _ -> do
                let matching = Map.filter ((== Just object) . facetObject . inputLibinput) (inputDevices inventory)
                    update d =
                      let facet = updateFacet event (inputLibinput d)
                       in case event of
                            L.RiverLibinputDeviceV1Done -> publishFacet d facet
                            L.RiverLibinputDeviceV1Removed -> (publishFacet d (emptyFacet FacetRemoved)){inputResults = Map.mapWithKey (\key r -> if libinputKey key then r{settingOutcome = SettingUnavailable} else r) (inputResults d)}
                            _
                              | objectVersion object == 1 && facetLifecycle facet == FacetReady -> publishFacet d facet
                              | otherwise -> d{inputLibinput = facet}
                modifyIORef' state (\i -> i{inputDevices = foldr (Map.adjust update) (inputDevices i) (Map.keys matching)})
                case event of
                  L.RiverLibinputDeviceV1Removed | not (Map.null matching) -> submitRequest connection L.riverLibinputDeviceV1Interface object L.encodeRiverLibinputDeviceV1Destroy >>= result
                  _ -> pure ()
            apply
      bindGlobalWithRoute connection (waylandRegistry wayland) global route >>= \case
        Left e -> failed e
        Right manager -> writeIORef managerRef (Just manager)
    _ -> pure ()

publishFacet :: InputDevice -> LibinputFacet -> InputDevice
publishFacet device facet = device{inputLibinput = facet, inputDefaults = facetDefaults facet, inputCurrent = facetCurrent facet}

syncInput :: WaylandSession -> (ConnectionError -> IO ()) -> IO () -> IO ()
syncInput wayland failed action =
  void $
    submitNewObjectRequestWithRoute
      (waylandConnection wayland)
      Wayland.wlDisplayInterface
      (waylandDisplay wayland)
      Wayland.wlCallbackInterface
      1
      (Wayland.encodeWlDisplaySync . objectId)
      (eventRoute Wayland.decodeWlCallbackEvent (\_ _ -> action))
      >>= either failed (const (pure ()))

sendLibinput :: IORef (Maybe (Object L.RiverLibinputConfigV1)) -> IORef Integer -> IORef InputInventory -> Connection -> IO () -> (ConnectionError -> IO ()) -> InputDevice -> IO ()
sendLibinput managerRef outstanding state connection apply failed device =
  case (facetLifecycle facet, facetObject facet) of
    (FacetReady, Just object) | inputLifecycle device == Ready -> mapM_ (send object) (Map.toList (inputResults device))
    _ -> pure ()
 where
  facet = inputLibinput device
  generation = inputGeneration device
  send object (key, desired)
    | libinputKey key
    , settingOutcome desired == SettingPending
    , supportsSetting facet (settingDesired desired) == Just True = do
        count <- readIORef outstanding
        when (count < 256) $ do
          modifyIORef' state (\i -> i{inputDevices = Map.adjust (\d -> d{inputResults = Map.adjust (\r -> r{settingOutcome = SettingAwaiting}) key (inputResults d)}) generation (inputDevices i)})
          let complete outcome = modifyIORef' state (acknowledgeLibinput generation object desired outcome)
              acknowledged = \case
                L.RiverLibinputResultV1Success -> SettingApplied
                L.RiverLibinputResultV1Unsupported -> SettingUnsupported
                L.RiverLibinputResultV1Invalid -> SettingRejectedInvalid
              request parentInterface parent makeRequest callback = do
                modifyIORef' outstanding (+ 1)
                response <-
                  submitNewObjectRequestWithRoute
                    connection
                    parentInterface
                    parent
                    L.riverLibinputResultV1Interface
                    1
                    (makeRequest . objectId)
                    (eventRoute L.decodeRiverLibinputResultV1Event (\_ event -> modifyIORef' outstanding (subtract 1) >> callback (acknowledged event) >> apply))
                case response of
                  Left e -> modifyIORef' outstanding (subtract 1) >> callback SettingUnavailable >> failed e
                  Right _ -> pure ()
          case settingDesired desired of
            Acceleration (AccelerationCurves curves) ->
              readIORef managerRef >>= \case
                Nothing -> complete SettingUnavailable
                Just manager -> do
                  allocated <-
                    submitNewObjectRequest
                      connection
                      L.riverLibinputConfigV1Interface
                      manager
                      L.riverLibinputAccelConfigV1Interface
                      1
                      (\config -> L.encodeRiverLibinputConfigV1CreateAccelConfig (objectId config) L.RiverLibinputDeviceV1AccelProfileEnumCustom)
                  case allocated of
                    Left e -> failed e
                    Right config -> do
                      retired <- newIORef False
                      let destroy = do
                            previous <- atomicModifyIORef' retired (\old -> (True, old))
                            when (not previous) $ submitRequest connection L.riverLibinputAccelConfigV1Interface config L.encodeRiverLibinputAccelConfigV1Destroy >>= either failed pure
                          stillCurrent = do
                            inventory <- readIORef state
                            pure $ case Map.lookup generation (inputDevices inventory) of
                              Just current -> facetObject (inputLibinput current) == Just object && Map.lookup key (inputResults current) == Just desired{settingOutcome = SettingAwaiting}
                              Nothing -> False
                          points [] = do
                            live <- stillCurrent
                            if live then request L.riverLibinputDeviceV1Interface object (\result -> L.encodeRiverLibinputDeviceV1ApplyAccelConfig result (objectId config)) complete >> destroy else destroy
                          points (AccelCurve kind step values : rest) = do
                            live <- stillCurrent
                            if not live
                              then destroy
                              else
                                request
                                  L.riverLibinputAccelConfigV1Interface
                                  config
                                  (\result -> L.encodeRiverLibinputAccelConfigV1SetPoints result (case kind of CurveFallback -> L.RiverLibinputAccelConfigV1AccelTypeEnumFallback; CurveMotion -> L.RiverLibinputAccelConfigV1AccelTypeEnumMotion; CurveScroll -> L.RiverLibinputAccelConfigV1AccelTypeEnumScroll) (encodeDoubles [step]) (encodeDoubles values))
                                  (\outcome -> if outcome == SettingApplied then points rest else complete outcome >> destroy)
                      points curves `onException` destroy
            setting -> case settingRequest setting of
              Nothing -> complete SettingUnavailable
              Just makeRequest -> request L.riverLibinputDeviceV1Interface object makeRequest complete
    | otherwise = pure ()

acknowledgeLibinput :: InputGeneration -> Object L.RiverLibinputDeviceV1 -> SettingResult -> SettingOutcome -> InputInventory -> InputInventory
acknowledgeLibinput generation object desired outcome inventory = inventory{inputDevices = Map.adjust update generation (inputDevices inventory)}
 where
  update device
    | inputLifecycle device == Removed || facetGeneration (inputLibinput device) /= settingFacet desired || facetObject (inputLibinput device) /= Just object = device
    | otherwise = device{inputResults = Map.adjust record (settingKey (settingDesired desired)) (inputResults device)}
  record current
    | settingFacet current == settingFacet desired && settingRevision current == settingRevision desired && settingDesired current == settingDesired desired && settingOutcome current == SettingAwaiting = current{settingOutcome = outcome}
    | otherwise = current

finishFacetDiscovery :: Object L.RiverLibinputDeviceV1 -> FacetGeneration -> InputInventory -> InputInventory
finishFacetDiscovery object token inventory = inventory{inputDevices = fmap ready (inputDevices inventory)}
 where
  ready device
    | facetObject facet == Just object && facetGeneration facet == Just token && facetLifecycle facet == FacetDiscovering = publishFacet device facet{facetLifecycle = FacetReady}
    | otherwise = device
   where
    facet = inputLibinput device
