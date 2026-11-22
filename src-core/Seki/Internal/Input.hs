{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

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

-- | The kind of input device a rule applies to.
data DeviceType = Keyboard | Pointer | Touch | Tablet deriving (Eq, Ord, Show)

-- | Whether the device sends events: always, never, or only while no external mouse is plugged in.
data SendEventsMode = EventsEnabled | EventsDisabled | EventsDisabledOnExternalMouse deriving (Eq, Ord, Show)

-- | Which buttons a one-, two-, and three-finger tap or click press: left, right, and middle, or left, middle, and right.
data ButtonMap = LeftRightMiddle | LeftMiddleRight deriving (Eq, Ord, Show)

{- | Whether a tap-and-drag survives lifting the finger: never, until a timeout,
or until the next tap.
-}
data DragLockMode = DragLockDisabled | DragLockTimeout | DragLockSticky deriving (Eq, Ord, Show)

-- | Whether a three- or four-finger swipe drags, as if a button were held.
data ThreeFingerDragMode = ThreeFingerDragDisabled | ThreeFingerDragEnabled | FourFingerDragEnabled deriving (Eq, Ord, Show)

{- | How a touchpad press picks its button: by the area of the pad pressed, or
by the number of fingers on it.
-}
data ClickMethod = ClickNone | ClickButtonAreas | ClickFinger deriving (Eq, Ord, Show)

{- | How the device scrolls: not at all, with two fingers, along the touchpad
edge, or by moving while a button is held.
-}
data ScrollMethod = ScrollNone | ScrollTwoFinger | ScrollEdge | ScrollOnButton deriving (Eq, Ord, Show)

{- | libinput's pointer acceleration profile. 'AccelCustom' uses the curves of
'AccelerationCurves'.
-}
data AccelProfile = AccelNone | AccelFlat | AccelAdaptive | AccelCustom deriving (Eq, Ord, Show)

-- | The movement a custom acceleration curve applies to: pointer motion, scrolling, or any movement without a curve of its own.
data CurveType = CurveFallback | CurveMotion | CurveScroll deriving (Eq, Ord, Show)

-- | A keyboard layout of the device's keymap, by 0-based index or by name.
data LayoutSelection = LayoutByIndex Int32 | LayoutByName Text deriving (Eq, Show)

-- | A rectangle in global logical coordinates: x, y, width, and height.
data InputRectangle = InputRectangle Int32 Int32 Int32 Int32 deriving (Eq, Show)

{- | A custom acceleration curve: the movement it applies to, the positive
input-speed step between points, and from 2 to 64 non-negative output speeds,
one per step.
-}
data AccelCurve = AccelCurve CurveType Double [Double] deriving (Eq, Show)

-- | Pointer acceleration as a libinput profile, or as one to three custom curves of distinct types.
data AccelerationConfig = AccelerationProfile AccelProfile | AccelerationCurves [AccelCurve] deriving (Eq, Show)

{- | One setting for the matching devices. When a device or river cannot apply
a setting, seki reports that in IPC and keeps running.
-}
data InputSetting
  = -- | A keyboard's repeat rate in repeats per second and delay in milliseconds. A rate of 0 turns repeat off.
    RepeatInfo Int32 Int32
  | -- | A pointer's scroll speed multiplier, so 0.5 scrolls at half speed.
    ScrollFactor Double
  | -- | Map a pointer, touch, or tablet device to the output with this name, or clear the mapping.
    MapOutput (Maybe Text)
  | -- | Map a pointer, touch, or tablet device to this rectangle, or clear the mapping. A rectangle wins over an output.
    MapRectangle (Maybe InputRectangle)
  | SendEvents SendEventsMode
  | -- | Tap to click.
    Tap Bool
  | TapButtonMap ButtonMap
  | -- | Tap and drag.
    Drag Bool
  | DragLock DragLockMode
  | ThreeFingerDrag ThreeFingerDragMode
  | -- | libinput's six-value calibration matrix for a touch or tablet device.
    CalibrationMatrix [Float]
  | Acceleration AccelerationConfig
  | -- | Pointer acceleration from -1, the slowest, to 1, the fastest, where 0 is the device default.
    AccelSpeed Double
  | NaturalScroll Bool
  | LeftHanded Bool
  | ClickMethod ClickMethod
  | -- | The buttons a clickfinger press maps one, two, and three fingers to.
    ClickfingerButtonMap ButtonMap
  | -- | Press the left and right buttons together for a middle click.
    MiddleEmulation Bool
  | ScrollMethod ScrollMethod
  | -- | The Linux input event code of the button that 'ScrollOnButton' holds.
    ScrollButton Word32
  | -- | Toggle 'ScrollOnButton' with a click of the button instead of holding it.
    ScrollButtonLock Bool
  | -- | Turn a touchpad off while the keyboard types.
    DisableWhileTyping Bool
  | -- | Turn a touchpad off while the trackpoint moves.
    DisableWhileTrackpoint Bool
  | -- | Rotate the device's input clockwise by this many degrees, below 360.
    Rotation Word32
  | -- | Load a keyboard's keymap from this XKB keymap file.
    Keymap FilePath
  | ActiveLayout LayoutSelection
  | CapsLock Bool
  | NumLock Bool
  | ScrollLock Bool
  deriving (Eq, Show)

-- | Which field of a device's settings an 'InputSetting' fills. A later setting with the same key replaces an earlier one.
data SettingKey = RepeatInfoKey | ScrollFactorKey | MapOutputKey | MapRectangleKey | SendEventsKey | TapKey | TapButtonMapKey | DragKey | DragLockKey | ThreeFingerDragKey | CalibrationMatrixKey | AccelerationKey | AccelSpeedKey | NaturalScrollKey | LeftHandedKey | ClickMethodKey | ClickfingerButtonMapKey | MiddleEmulationKey | ScrollMethodKey | ScrollButtonKey | ScrollButtonLockKey | DisableWhileTypingKey | DisableWhileTrackpointKey | RotationKey | KeymapKey | ActiveLayoutKey | CapsLockKey | NumLockKey | ScrollLockKey
  deriving (Eq, Ord, Show)

-- | The field a setting fills.
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

-- | Settings for every device whose name and type match.
data InputRule = InputRule
  { ruleDeviceName :: DevicePattern
  -- ^ The pattern the whole device name must match.
  , ruleDeviceType :: Maybe DeviceType
  -- ^ The device type to match, or 'Nothing' for every type.
  , ruleSettings :: [InputSetting]
  -- ^ The settings to apply, in order.
  }
  deriving (Eq, Show)

data PatternToken = Literal Char | AnyCharacter | AnySequence deriving (Eq, Show)

-- | A compiled device name pattern. Build one with 'compileDevicePattern'.
newtype DevicePattern = DevicePattern [PatternToken] deriving (Eq, Show)

-- | Why 'compileDevicePattern' rejected a pattern.
data PatternError
  = -- | The pattern is longer than 256 characters.
    PatternTooLong
  | -- | A backslash before this character, which is not @*@, @?@, or backslash, or 'Nothing' for a backslash that ends the pattern.
    InvalidPatternEscape (Maybe Char)
  deriving (Eq, Show)

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

-- | Whether the pattern matches the whole device name.
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

-- | Why 'validateInputSetting' rejected a setting.
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

{- | Check a setting's values. seki checks every setting this way before it
connects, and a configuration can call this to check a value itself.
-}
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
