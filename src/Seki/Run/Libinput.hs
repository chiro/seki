{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.Libinput (FacetGeneration (..), FacetLifecycle (..), LibinputFacet (..), emptyFacet, encodeFloats, encodeDoubles, decodeFloats, decodeDouble, libinputKey, libinputKeys, updateFacet, supportsSetting, settingRequest) where

import Data.Binary.Get qualified as Get
import Data.Bits (complement, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as Lazy
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import Seki.Internal.Input
import Seki.Protocol.RiverLibinputConfigV1 qualified as L
import Seki.Wire.Binding (RequestEncoder)
import Seki.Wire.Connection.ObjectTable (Object)
import Seki.Wire.Types (ObjectId)

newtype FacetGeneration = FacetGeneration Integer deriving (Eq, Ord, Show)

data FacetLifecycle = FacetDiscovering | FacetReady | FacetUnavailable | FacetRemoved deriving (Eq, Show)
data LibinputFacet = LibinputFacet
  { facetObject :: Maybe (Object L.RiverLibinputDeviceV1)
  , facetGeneration :: Maybe FacetGeneration
  , facetLifecycle :: FacetLifecycle
  , facetSupport :: Map SettingKey (Either Text Word32)
  , facetDefaults :: Map SettingKey InputSetting
  , facetCurrent :: Map SettingKey InputSetting
  , facetDiagnostics :: [Text]
  }
  deriving (Eq, Show)

emptyFacet :: FacetLifecycle -> LibinputFacet
emptyFacet lifecycle = LibinputFacet Nothing Nothing lifecycle Map.empty Map.empty Map.empty []

encodeFloats :: [Float] -> ByteString
encodeFloats = Lazy.toStrict . B.toLazyByteString . foldMap (case targetByteOrder of LittleEndian -> B.floatLE; BigEndian -> B.floatBE)
encodeDoubles :: [Double] -> ByteString
encodeDoubles = Lazy.toStrict . B.toLazyByteString . foldMap (case targetByteOrder of LittleEndian -> B.doubleLE; BigEndian -> B.doubleBE)

decodeFloats :: ByteString -> Either Text [Float]
decodeFloats bytes
  | BS.length bytes /= 24 = Left "calibration matrix must contain six native floats"
  | otherwise = finiteValues (Get.runGet (sequence (replicate 6 Get.getFloathost)) (Lazy.fromStrict bytes))
decodeDouble :: ByteString -> Either Text Double
decodeDouble bytes
  | BS.length bytes /= 8 = Left "speed must contain one native double"
  | otherwise = do
      values <- finiteValues [Get.runGet Get.getDoublehost (Lazy.fromStrict bytes)]
      case values of
        [value] -> Right value
        _ -> Left "invalid speed"
finiteValues :: (RealFloat a) => [a] -> Either Text [a]
finiteValues values
  | any (\v -> isNaN v || isInfinite v) values = Left "non-finite floating point payload"
  | otherwise = Right values

libinputKey :: SettingKey -> Bool
libinputKey key = key `elem` libinputKeys
libinputKeys :: [SettingKey]
libinputKeys = [SendEventsKey, TapKey, TapButtonMapKey, DragKey, DragLockKey, ThreeFingerDragKey, CalibrationMatrixKey, AccelerationKey, AccelSpeedKey, NaturalScrollKey, LeftHandedKey, ClickMethodKey, ClickfingerButtonMapKey, MiddleEmulationKey, ScrollMethodKey, ScrollButtonKey, ScrollButtonLockKey, DisableWhileTypingKey, DisableWhileTrackpointKey, RotationKey]

-- Support is absent until its own event; unknown masks remain diagnostic data.
updateFacet :: L.RiverLibinputDeviceV1Event fd -> LibinputFacet -> LibinputFacet
updateFacet event facet = case event of
  L.RiverLibinputDeviceV1SendEventsSupport (L.RiverLibinputDeviceV1SendEventsModesFlags value) -> facet{facetSupport = foldr (\key -> Map.insert key (if value .&. complement 3 /= 0 then Left (Text.pack (show value)) else Right value)) (facetSupport facet) [SendEventsKey]}
  L.RiverLibinputDeviceV1TapSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (if value < 0 then Left (Text.pack (show value)) else Right (fromIntegral value))) (facetSupport facet) [TapKey, TapButtonMapKey, DragKey, DragLockKey]}
  L.RiverLibinputDeviceV1ThreeFingerDragSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (if value < 0 then Left (Text.pack (show value)) else Right (fromIntegral value))) (facetSupport facet) [ThreeFingerDragKey]}
  L.RiverLibinputDeviceV1CalibrationMatrixSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (Right (if value == 0 then 0 else 1))) (facetSupport facet) [CalibrationMatrixKey]}
  L.RiverLibinputDeviceV1AccelProfilesSupport (L.RiverLibinputDeviceV1AccelProfilesFlags value) -> facet{facetSupport = foldr (\key -> Map.insert key (if value .&. complement 7 /= 0 then Left (Text.pack (show value)) else Right value)) (facetSupport facet) [AccelerationKey, AccelSpeedKey]}
  L.RiverLibinputDeviceV1NaturalScrollSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (Right (if value == 0 then 0 else 1))) (facetSupport facet) [NaturalScrollKey]}
  L.RiverLibinputDeviceV1LeftHandedSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (Right (if value == 0 then 0 else 1))) (facetSupport facet) [LeftHandedKey]}
  L.RiverLibinputDeviceV1ClickMethodSupport (L.RiverLibinputDeviceV1ClickMethodsFlags value) -> facet{facetSupport = foldr (\key -> Map.insert key (if value .&. complement 3 /= 0 then Left (Text.pack (show value)) else Right value)) (facetSupport facet) [ClickMethodKey, ClickfingerButtonMapKey]}
  L.RiverLibinputDeviceV1MiddleEmulationSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (Right (if value == 0 then 0 else 1))) (facetSupport facet) [MiddleEmulationKey]}
  L.RiverLibinputDeviceV1ScrollMethodSupport (L.RiverLibinputDeviceV1ScrollMethodsFlags value) -> facet{facetSupport = foldr (\key -> Map.insert key (if value .&. complement 7 /= 0 then Left (Text.pack (show value)) else Right value)) (facetSupport facet) [ScrollMethodKey, ScrollButtonKey, ScrollButtonLockKey]}
  L.RiverLibinputDeviceV1DwtSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (Right (if value == 0 then 0 else 1))) (facetSupport facet) [DisableWhileTypingKey]}
  L.RiverLibinputDeviceV1DwtpSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (Right (if value == 0 then 0 else 1))) (facetSupport facet) [DisableWhileTrackpointKey]}
  L.RiverLibinputDeviceV1RotationSupport value -> facet{facetSupport = foldr (\key -> Map.insert key (Right (if value == 0 then 0 else 1))) (facetSupport facet) [RotationKey]}
  L.RiverLibinputDeviceV1SendEventsDefault (L.RiverLibinputDeviceV1SendEventsModesFlags value) -> recordValue SendEventsKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (SendEvents EventsEnabled); 1 -> Right (SendEvents EventsDisabled); 2 -> Right (SendEvents EventsDisabledOnExternalMouse); _ -> Left ("unknown send_events value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1SendEventsCurrent (L.RiverLibinputDeviceV1SendEventsModesFlags value) -> recordValue SendEventsKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (SendEvents EventsEnabled); 1 -> Right (SendEvents EventsDisabled); 2 -> Right (SendEvents EventsDisabledOnExternalMouse); _ -> Left ("unknown send_events value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1TapDefault (L.RiverLibinputDeviceV1TapStateEnum value) -> recordValue TapKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (Tap False); 1 -> Right (Tap True); _ -> Left ("unknown tap value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1TapCurrent (L.RiverLibinputDeviceV1TapStateEnum value) -> recordValue TapKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (Tap False); 1 -> Right (Tap True); _ -> Left ("unknown tap value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1TapButtonMapDefault (L.RiverLibinputDeviceV1TapButtonMapEnum value) -> recordValue TapButtonMapKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (TapButtonMap LeftRightMiddle); 1 -> Right (TapButtonMap LeftMiddleRight); _ -> Left ("unknown tap_button_map value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1TapButtonMapCurrent (L.RiverLibinputDeviceV1TapButtonMapEnum value) -> recordValue TapButtonMapKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (TapButtonMap LeftRightMiddle); 1 -> Right (TapButtonMap LeftMiddleRight); _ -> Left ("unknown tap_button_map value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DragDefault (L.RiverLibinputDeviceV1DragStateEnum value) -> recordValue DragKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (Drag False); 1 -> Right (Drag True); _ -> Left ("unknown drag value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DragCurrent (L.RiverLibinputDeviceV1DragStateEnum value) -> recordValue DragKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (Drag False); 1 -> Right (Drag True); _ -> Left ("unknown drag value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DragLockDefault (L.RiverLibinputDeviceV1DragLockStateEnum value) -> recordValue DragLockKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (DragLock DragLockDisabled); 1 -> Right (DragLock DragLockTimeout); 2 -> Right (DragLock DragLockSticky); _ -> Left ("unknown drag_lock value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DragLockCurrent (L.RiverLibinputDeviceV1DragLockStateEnum value) -> recordValue DragLockKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (DragLock DragLockDisabled); 1 -> Right (DragLock DragLockTimeout); 2 -> Right (DragLock DragLockSticky); _ -> Left ("unknown drag_lock value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ThreeFingerDragDefault (L.RiverLibinputDeviceV1ThreeFingerDragStateEnum value) -> recordValue ThreeFingerDragKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (ThreeFingerDrag ThreeFingerDragDisabled); 1 -> Right (ThreeFingerDrag ThreeFingerDragEnabled); 2 -> Right (ThreeFingerDrag FourFingerDragEnabled); _ -> Left ("unknown three_finger_drag value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ThreeFingerDragCurrent (L.RiverLibinputDeviceV1ThreeFingerDragStateEnum value) -> recordValue ThreeFingerDragKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (ThreeFingerDrag ThreeFingerDragDisabled); 1 -> Right (ThreeFingerDrag ThreeFingerDragEnabled); 2 -> Right (ThreeFingerDrag FourFingerDragEnabled); _ -> Left ("unknown three_finger_drag value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1CalibrationMatrixDefault value -> recordValue CalibrationMatrixKey facetDefaults (\v f -> f{facetDefaults = v}) (CalibrationMatrix <$> decodeFloats value) facet
  L.RiverLibinputDeviceV1CalibrationMatrixCurrent value -> recordValue CalibrationMatrixKey facetCurrent (\v f -> f{facetCurrent = v}) (CalibrationMatrix <$> decodeFloats value) facet
  L.RiverLibinputDeviceV1AccelProfileDefault (L.RiverLibinputDeviceV1AccelProfileEnum value) -> recordValue AccelerationKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (Acceleration (AccelerationProfile AccelNone)); 1 -> Right (Acceleration (AccelerationProfile AccelFlat)); 2 -> Right (Acceleration (AccelerationProfile AccelAdaptive)); 4 -> Right (Acceleration (AccelerationProfile AccelCustom)); _ -> Left ("unknown accel_profile value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1AccelProfileCurrent (L.RiverLibinputDeviceV1AccelProfileEnum value) -> recordValue AccelerationKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (Acceleration (AccelerationProfile AccelNone)); 1 -> Right (Acceleration (AccelerationProfile AccelFlat)); 2 -> Right (Acceleration (AccelerationProfile AccelAdaptive)); 4 -> Right (Acceleration (AccelerationProfile AccelCustom)); _ -> Left ("unknown accel_profile value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1AccelSpeedDefault value -> recordValue AccelSpeedKey facetDefaults (\v f -> f{facetDefaults = v}) (AccelSpeed <$> decodeDouble value) facet
  L.RiverLibinputDeviceV1AccelSpeedCurrent value -> recordValue AccelSpeedKey facetCurrent (\v f -> f{facetCurrent = v}) (AccelSpeed <$> decodeDouble value) facet
  L.RiverLibinputDeviceV1NaturalScrollDefault (L.RiverLibinputDeviceV1NaturalScrollStateEnum value) -> recordValue NaturalScrollKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (NaturalScroll False); 1 -> Right (NaturalScroll True); _ -> Left ("unknown natural_scroll value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1NaturalScrollCurrent (L.RiverLibinputDeviceV1NaturalScrollStateEnum value) -> recordValue NaturalScrollKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (NaturalScroll False); 1 -> Right (NaturalScroll True); _ -> Left ("unknown natural_scroll value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1LeftHandedDefault (L.RiverLibinputDeviceV1LeftHandedStateEnum value) -> recordValue LeftHandedKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (LeftHanded False); 1 -> Right (LeftHanded True); _ -> Left ("unknown left_handed value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1LeftHandedCurrent (L.RiverLibinputDeviceV1LeftHandedStateEnum value) -> recordValue LeftHandedKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (LeftHanded False); 1 -> Right (LeftHanded True); _ -> Left ("unknown left_handed value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ClickMethodDefault (L.RiverLibinputDeviceV1ClickMethodEnum value) -> recordValue ClickMethodKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (ClickMethod ClickNone); 1 -> Right (ClickMethod ClickButtonAreas); 2 -> Right (ClickMethod ClickFinger); _ -> Left ("unknown click_method value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ClickMethodCurrent (L.RiverLibinputDeviceV1ClickMethodEnum value) -> recordValue ClickMethodKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (ClickMethod ClickNone); 1 -> Right (ClickMethod ClickButtonAreas); 2 -> Right (ClickMethod ClickFinger); _ -> Left ("unknown click_method value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ClickfingerButtonMapDefault (L.RiverLibinputDeviceV1ClickfingerButtonMapEnum value) -> recordValue ClickfingerButtonMapKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (ClickfingerButtonMap LeftRightMiddle); 1 -> Right (ClickfingerButtonMap LeftMiddleRight); _ -> Left ("unknown clickfinger_button_map value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ClickfingerButtonMapCurrent (L.RiverLibinputDeviceV1ClickfingerButtonMapEnum value) -> recordValue ClickfingerButtonMapKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (ClickfingerButtonMap LeftRightMiddle); 1 -> Right (ClickfingerButtonMap LeftMiddleRight); _ -> Left ("unknown clickfinger_button_map value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1MiddleEmulationDefault (L.RiverLibinputDeviceV1MiddleEmulationStateEnum value) -> recordValue MiddleEmulationKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (MiddleEmulation False); 1 -> Right (MiddleEmulation True); _ -> Left ("unknown middle_emulation value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1MiddleEmulationCurrent (L.RiverLibinputDeviceV1MiddleEmulationStateEnum value) -> recordValue MiddleEmulationKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (MiddleEmulation False); 1 -> Right (MiddleEmulation True); _ -> Left ("unknown middle_emulation value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ScrollMethodDefault (L.RiverLibinputDeviceV1ScrollMethodEnum value) -> recordValue ScrollMethodKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (ScrollMethod ScrollNone); 1 -> Right (ScrollMethod ScrollTwoFinger); 2 -> Right (ScrollMethod ScrollEdge); 4 -> Right (ScrollMethod ScrollOnButton); _ -> Left ("unknown scroll_method value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ScrollMethodCurrent (L.RiverLibinputDeviceV1ScrollMethodEnum value) -> recordValue ScrollMethodKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (ScrollMethod ScrollNone); 1 -> Right (ScrollMethod ScrollTwoFinger); 2 -> Right (ScrollMethod ScrollEdge); 4 -> Right (ScrollMethod ScrollOnButton); _ -> Left ("unknown scroll_method value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ScrollButtonDefault value -> recordValue ScrollButtonKey facetDefaults (\v f -> f{facetDefaults = v}) (Right (ScrollButton value)) facet
  L.RiverLibinputDeviceV1ScrollButtonCurrent value -> recordValue ScrollButtonKey facetCurrent (\v f -> f{facetCurrent = v}) (Right (ScrollButton value)) facet
  L.RiverLibinputDeviceV1ScrollButtonLockDefault (L.RiverLibinputDeviceV1ScrollButtonLockStateEnum value) -> recordValue ScrollButtonLockKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (ScrollButtonLock False); 1 -> Right (ScrollButtonLock True); _ -> Left ("unknown scroll_button_lock value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1ScrollButtonLockCurrent (L.RiverLibinputDeviceV1ScrollButtonLockStateEnum value) -> recordValue ScrollButtonLockKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (ScrollButtonLock False); 1 -> Right (ScrollButtonLock True); _ -> Left ("unknown scroll_button_lock value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DwtDefault (L.RiverLibinputDeviceV1DwtStateEnum value) -> recordValue DisableWhileTypingKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (DisableWhileTyping False); 1 -> Right (DisableWhileTyping True); _ -> Left ("unknown dwt value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DwtCurrent (L.RiverLibinputDeviceV1DwtStateEnum value) -> recordValue DisableWhileTypingKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (DisableWhileTyping False); 1 -> Right (DisableWhileTyping True); _ -> Left ("unknown dwt value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DwtpDefault (L.RiverLibinputDeviceV1DwtpStateEnum value) -> recordValue DisableWhileTrackpointKey facetDefaults (\v f -> f{facetDefaults = v}) (case value of 0 -> Right (DisableWhileTrackpoint False); 1 -> Right (DisableWhileTrackpoint True); _ -> Left ("unknown dwtp value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1DwtpCurrent (L.RiverLibinputDeviceV1DwtpStateEnum value) -> recordValue DisableWhileTrackpointKey facetCurrent (\v f -> f{facetCurrent = v}) (case value of 0 -> Right (DisableWhileTrackpoint False); 1 -> Right (DisableWhileTrackpoint True); _ -> Left ("unknown dwtp value " <> Text.pack (show value))) facet
  L.RiverLibinputDeviceV1RotationDefault value -> recordValue RotationKey facetDefaults (\v f -> f{facetDefaults = v}) (Right (Rotation value)) facet
  L.RiverLibinputDeviceV1RotationCurrent value -> recordValue RotationKey facetCurrent (\v f -> f{facetCurrent = v}) (Right (Rotation value)) facet
  L.RiverLibinputDeviceV1Done -> facet{facetLifecycle = FacetReady}
  L.RiverLibinputDeviceV1Removed -> facet{facetLifecycle = FacetRemoved}
  _ -> facet

recordValue :: SettingKey -> (LibinputFacet -> Map SettingKey InputSetting) -> (Map SettingKey InputSetting -> LibinputFacet -> LibinputFacet) -> Either Text InputSetting -> LibinputFacet -> LibinputFacet
recordValue key get set decoded facet = case decoded >>= (\value -> case validateInputSetting value of Left e -> Left (Text.pack (show e)); Right () -> Right value) of
  Left diagnostic -> set (Map.delete key (get facet)) facet{facetDiagnostics = take 16 ((Text.pack (show key) <> ": " <> diagnostic) : facetDiagnostics facet)}
  Right value -> set (Map.insert (settingKey value) value (get facet)) facet

-- Nothing is still discovering; False is an explicit unsupported capability.
supportsSetting :: LibinputFacet -> InputSetting -> Maybe Bool
supportsSetting facet setting = case Map.lookup (settingKey setting) (facetSupport facet) of
  Nothing -> Nothing
  Just (Left _) -> Nothing
  Just (Right mask) -> Just $ case setting of
    SendEvents EventsEnabled -> True
    SendEvents EventsDisabled -> mask .&. 1 /= 0
    SendEvents EventsDisabledOnExternalMouse -> mask .&. 2 /= 0
    ThreeFingerDrag FourFingerDragEnabled -> mask >= 4
    ThreeFingerDrag _ -> mask >= 3
    Acceleration (AccelerationProfile AccelNone) -> mask /= 0
    Acceleration (AccelerationProfile AccelFlat) -> mask .&. 1 /= 0
    Acceleration (AccelerationProfile AccelAdaptive) -> mask .&. 2 /= 0
    Acceleration _ -> mask .&. 4 /= 0
    ClickMethod ClickNone -> mask /= 0
    ClickMethod ClickButtonAreas -> mask .&. 1 /= 0
    ClickMethod ClickFinger -> mask .&. 2 /= 0
    ClickfingerButtonMap _ -> mask .&. 2 /= 0
    ScrollMethod ScrollNone -> mask /= 0
    ScrollMethod ScrollTwoFinger -> mask .&. 1 /= 0
    ScrollMethod ScrollEdge -> mask .&. 2 /= 0
    ScrollMethod ScrollOnButton -> mask .&. 4 /= 0
    ScrollButton _ -> mask .&. 4 /= 0
    ScrollButtonLock _ -> mask .&. 4 /= 0
    _ -> mask /= 0

settingRequest :: InputSetting -> Maybe (ObjectId -> RequestEncoder fd)
settingRequest = \case
  SendEvents value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetSendEvents result (L.RiverLibinputDeviceV1SendEventsModesFlags (case value of EventsEnabled -> 0; EventsDisabled -> 1; EventsDisabledOnExternalMouse -> 2)))
  Tap value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetTap result (L.RiverLibinputDeviceV1TapStateEnum (case value of False -> 0; True -> 1)))
  TapButtonMap value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetTapButtonMap result (L.RiverLibinputDeviceV1TapButtonMapEnum (case value of LeftRightMiddle -> 0; LeftMiddleRight -> 1)))
  Drag value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetDrag result (L.RiverLibinputDeviceV1DragStateEnum (case value of False -> 0; True -> 1)))
  DragLock value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetDragLock result (L.RiverLibinputDeviceV1DragLockStateEnum (case value of DragLockDisabled -> 0; DragLockTimeout -> 1; DragLockSticky -> 2)))
  ThreeFingerDrag value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetThreeFingerDrag result (L.RiverLibinputDeviceV1ThreeFingerDragStateEnum (case value of ThreeFingerDragDisabled -> 0; ThreeFingerDragEnabled -> 1; FourFingerDragEnabled -> 2)))
  CalibrationMatrix value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetCalibrationMatrix result (encodeFloats value))
  Acceleration (AccelerationProfile value) -> Just (\result -> L.encodeRiverLibinputDeviceV1SetAccelProfile result (L.RiverLibinputDeviceV1AccelProfileEnum (case value of AccelNone -> 0; AccelFlat -> 1; AccelAdaptive -> 2; AccelCustom -> 4)))
  AccelSpeed value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetAccelSpeed result (encodeDoubles [value]))
  NaturalScroll value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetNaturalScroll result (L.RiverLibinputDeviceV1NaturalScrollStateEnum (case value of False -> 0; True -> 1)))
  LeftHanded value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetLeftHanded result (L.RiverLibinputDeviceV1LeftHandedStateEnum (case value of False -> 0; True -> 1)))
  ClickMethod value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetClickMethod result (L.RiverLibinputDeviceV1ClickMethodEnum (case value of ClickNone -> 0; ClickButtonAreas -> 1; ClickFinger -> 2)))
  ClickfingerButtonMap value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetClickfingerButtonMap result (L.RiverLibinputDeviceV1ClickfingerButtonMapEnum (case value of LeftRightMiddle -> 0; LeftMiddleRight -> 1)))
  MiddleEmulation value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetMiddleEmulation result (L.RiverLibinputDeviceV1MiddleEmulationStateEnum (case value of False -> 0; True -> 1)))
  ScrollMethod value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetScrollMethod result (L.RiverLibinputDeviceV1ScrollMethodEnum (case value of ScrollNone -> 0; ScrollTwoFinger -> 1; ScrollEdge -> 2; ScrollOnButton -> 4)))
  ScrollButton value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetScrollButton result value)
  ScrollButtonLock value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetScrollButtonLock result (L.RiverLibinputDeviceV1ScrollButtonLockStateEnum (case value of False -> 0; True -> 1)))
  DisableWhileTyping value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetDwt result (L.RiverLibinputDeviceV1DwtStateEnum (case value of False -> 0; True -> 1)))
  DisableWhileTrackpoint value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetDwtp result (L.RiverLibinputDeviceV1DwtpStateEnum (case value of False -> 0; True -> 1)))
  Rotation value -> Just (\result -> L.encodeRiverLibinputDeviceV1SetRotation result value)
  _ -> Nothing
