{-# LANGUAGE OverloadedStrings #-}

module Seki.Internal.Input (
  DeviceType (..),
  DevicePattern,
  PatternError (..),
  compileDevicePattern,
  devicePatternText,
  matchesDevicePattern,
  InputRule (..),
  InputSetting (..),
  SettingKey (..),
  settingKey,
  resolveInputRules,
  InputRectangle (..),
  SendEventsMode (..),
  ButtonMap (..),
  DragLockMode (..),
  ThreeFingerDragMode (..),
  ClickMethod (..),
  ScrollMethod (..),
  AccelProfile (..),
  AccelerationConfig (..),
  AccelCurve (..),
  CurveType (..),
  LayoutSelection (..),
  SettingError (..),
  validateInputSetting,
) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word32)

data DeviceType = Keyboard | Pointer | Touch | Tablet deriving (Eq, Ord, Show)

data SendEventsMode = EventsEnabled | EventsDisabled | EventsDisabledOnExternalMouse deriving (Eq, Ord, Show)

data ButtonMap = LeftRightMiddle | LeftMiddleRight deriving (Eq, Ord, Show)

data DragLockMode = DragLockDisabled | DragLockTimeout | DragLockSticky deriving (Eq, Ord, Show)

data ThreeFingerDragMode = ThreeFingerDragDisabled | ThreeFingerDragEnabled | FourFingerDragEnabled deriving (Eq, Ord, Show)

data ClickMethod = ClickNone | ClickButtonAreas | ClickFinger deriving (Eq, Ord, Show)

data ScrollMethod = ScrollNone | ScrollTwoFinger | ScrollEdge | ScrollOnButton deriving (Eq, Ord, Show)

data AccelProfile = AccelNone | AccelFlat | AccelAdaptive | AccelCustom deriving (Eq, Ord, Show)

data CurveType = CurveFallback | CurveMotion | CurveScroll deriving (Eq, Ord, Show)

data LayoutSelection = LayoutByIndex Int32 | LayoutByName Text deriving (Eq, Show)

data InputRectangle = InputRectangle Int32 Int32 Int32 Int32 deriving (Eq, Show)
data AccelCurve = AccelCurve CurveType Double [Double] deriving (Eq, Show)
data AccelerationConfig = AccelerationProfile AccelProfile | AccelerationCurves [AccelCurve] deriving (Eq, Show)

data InputSetting
  = RepeatInfo Int32 Int32
  | ScrollFactor Double
  | MapOutput (Maybe Text)
  | MapRectangle (Maybe InputRectangle)
  | SendEvents SendEventsMode
  | Tap Bool
  | TapButtonMap ButtonMap
  | Drag Bool
  | DragLock DragLockMode
  | ThreeFingerDrag ThreeFingerDragMode
  | CalibrationMatrix [Float]
  | Acceleration AccelerationConfig
  | AccelSpeed Double
  | NaturalScroll Bool
  | LeftHanded Bool
  | ClickMethod ClickMethod
  | ClickfingerButtonMap ButtonMap
  | MiddleEmulation Bool
  | ScrollMethod ScrollMethod
  | ScrollButton Word32
  | ScrollButtonLock Bool
  | DisableWhileTyping Bool
  | DisableWhileTrackpoint Bool
  | Rotation Word32
  | Keymap FilePath
  | ActiveLayout LayoutSelection
  | CapsLock Bool
  | NumLock Bool
  | ScrollLock Bool
  deriving (Eq, Show)

data SettingKey = RepeatInfoKey | ScrollFactorKey | MapOutputKey | MapRectangleKey | SendEventsKey | TapKey | TapButtonMapKey | DragKey | DragLockKey | ThreeFingerDragKey | CalibrationMatrixKey | AccelerationKey | AccelSpeedKey | NaturalScrollKey | LeftHandedKey | ClickMethodKey | ClickfingerButtonMapKey | MiddleEmulationKey | ScrollMethodKey | ScrollButtonKey | ScrollButtonLockKey | DisableWhileTypingKey | DisableWhileTrackpointKey | RotationKey | KeymapKey | ActiveLayoutKey | CapsLockKey | NumLockKey | ScrollLockKey
  deriving (Eq, Ord, Show)

settingKey :: InputSetting -> SettingKey
settingKey setting = case setting of
  RepeatInfo _ _ -> RepeatInfoKey
  ScrollFactor _ -> ScrollFactorKey
  MapOutput _ -> MapOutputKey
  MapRectangle _ -> MapRectangleKey
  SendEvents _ -> SendEventsKey
  Tap _ -> TapKey
  TapButtonMap _ -> TapButtonMapKey
  Drag _ -> DragKey
  DragLock _ -> DragLockKey
  ThreeFingerDrag _ -> ThreeFingerDragKey
  CalibrationMatrix _ -> CalibrationMatrixKey
  Acceleration _ -> AccelerationKey
  AccelSpeed _ -> AccelSpeedKey
  NaturalScroll _ -> NaturalScrollKey
  LeftHanded _ -> LeftHandedKey
  ClickMethod _ -> ClickMethodKey
  ClickfingerButtonMap _ -> ClickfingerButtonMapKey
  MiddleEmulation _ -> MiddleEmulationKey
  ScrollMethod _ -> ScrollMethodKey
  ScrollButton _ -> ScrollButtonKey
  ScrollButtonLock _ -> ScrollButtonLockKey
  DisableWhileTyping _ -> DisableWhileTypingKey
  DisableWhileTrackpoint _ -> DisableWhileTrackpointKey
  Rotation _ -> RotationKey
  Keymap _ -> KeymapKey
  ActiveLayout _ -> ActiveLayoutKey
  CapsLock _ -> CapsLockKey
  NumLock _ -> NumLockKey
  ScrollLock _ -> ScrollLockKey

data InputRule = InputRule
  { ruleDeviceName :: DevicePattern
  , ruleDeviceType :: Maybe DeviceType
  , ruleSettings :: [InputSetting]
  }
  deriving (Eq, Show)

data PatternToken = Literal Char | AnyCharacter | AnySequence deriving (Eq, Show)
newtype DevicePattern = DevicePattern [PatternToken] deriving (Eq, Show)
data PatternError = PatternTooLong | InvalidPatternEscape (Maybe Char) deriving (Eq, Show)

-- | Anchored, case-sensitive glob. Only '*', '?' and backslash may be escaped.
compileDevicePattern :: Text -> Either PatternError DevicePattern
compileDevicePattern source
  | Text.length source > 256 = Left PatternTooLong
  | otherwise = DevicePattern <$> parse (Text.unpack source)
 where
  parse [] = Right []
  parse ('\\' : []) = Left (InvalidPatternEscape Nothing)
  parse ('\\' : c : rest)
    | c `elem` ['*', '?', '\\'] = (Literal c :) <$> parse rest
    | otherwise = Left (InvalidPatternEscape (Just c))
  parse ('*' : rest) = (AnySequence :) <$> parse rest
  parse ('?' : rest) = (AnyCharacter :) <$> parse rest
  parse (c : rest) = (Literal c :) <$> parse rest

-- | Recover the escaped pattern for diagnostics and configuration displays.
devicePatternText :: DevicePattern -> Text
devicePatternText (DevicePattern tokens) = Text.concat (map render tokens)
 where
  render AnySequence = "*"
  render AnyCharacter = "?"
  render (Literal c)
    | c `elem` ['*', '?', '\\'] = Text.pack ['\\', c]
    | otherwise = Text.singleton c

matchesDevicePattern :: DevicePattern -> Text -> Bool
matchesDevicePattern (DevicePattern tokens) name = case foldr step initial tokens of
  matched : _ -> matched
  [] -> False
 where
  characters = Text.unpack name
  initial = replicate (length characters) False <> [True]
  step AnySequence suffix = take (length suffix) (scanr (||) False suffix)
  step AnyCharacter suffix = drop 1 suffix <> [False]
  step (Literal expected) suffix = zipWith (\c matched -> c == expected && matched) characters (drop 1 suffix) <> [False]

-- | Matching rules compose in declaration order, replacing only specified fields.
resolveInputRules :: [InputRule] -> Text -> DeviceType -> Map SettingKey InputSetting
resolveInputRules rules name deviceType = foldl' apply Map.empty rules
 where
  apply settings rule
    | matchesDevicePattern (ruleDeviceName rule) name
        && maybe True (== deviceType) (ruleDeviceType rule) =
        foldl' (\current setting -> Map.insert (settingKey setting) setting current) settings (ruleSettings rule)
    | otherwise = settings

data SettingError
  = NegativeRepeat
  | InvalidScrollFactor
  | InvalidRectangle
  | EmptyOutputName
  | InvalidMatrix
  | InvalidAccelSpeed
  | InvalidCurve
  | InvalidRotation
  | InvalidLayout
  | EmptyKeymapPath
  deriving (Eq, Show)

validateInputSetting :: InputSetting -> Either SettingError ()
validateInputSetting setting = case setting of
  RepeatInfo rate delay | rate < 0 || delay < 0 -> Left NegativeRepeat
  ScrollFactor factor | not (finite factor) || factor < 0 || factor > 8388607.99609375 || (factor > 0 && factor * 256 <= 0.5) -> Left InvalidScrollFactor
  MapOutput (Just name) | Text.null name || Text.any (== '\0') name -> Left EmptyOutputName
  MapRectangle (Just (InputRectangle _ _ width height)) | width < 0 || height < 0 -> Left InvalidRectangle
  CalibrationMatrix values | length values /= 6 || any (not . finite) values -> Left InvalidMatrix
  AccelSpeed speed | not (finite speed) || speed < -1 || speed > 1 -> Left InvalidAccelSpeed
  Acceleration (AccelerationCurves curves) | null curves || length curves > 3 || any invalidCurve curves || length (map (\(AccelCurve kind _ _) -> kind) curves) /= length (unique (map (\(AccelCurve kind _ _) -> kind) curves)) -> Left InvalidCurve
  Rotation angle | angle >= 360 -> Left InvalidRotation
  ActiveLayout (LayoutByIndex index) | index < 0 -> Left InvalidLayout
  ActiveLayout (LayoutByName name) | Text.null name || Text.any (== '\0') name || BS.length (TextEncoding.encodeUtf8 name) > 4083 -> Left InvalidLayout
  Keymap path | null path || '\0' `elem` path -> Left EmptyKeymapPath
  _ -> Right ()
 where
  unique = foldr (\v acc -> if v `elem` acc then acc else v : acc) []
  finite value = not (isNaN value || isInfinite value)
  invalidCurve (AccelCurve _ step points) = not (finite step) || step <= 0 || length points < 2 || length points > 64 || any (\value -> not (finite value) || value < 0) points
