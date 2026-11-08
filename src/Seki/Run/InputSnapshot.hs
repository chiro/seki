{-# LANGUAGE OverloadedStrings #-}

-- | Bounded wire projection of immutable input state. No wire handles escape.
module Seki.Run.InputSnapshot (inputValue, inputStatusValue, xkbValue, settingValue, outcomeName) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.ByteString.Lazy qualified as Lazy
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Seki.Internal.Input
import Seki.Run.Input
import Seki.Run.Libinput
import Seki.Run.Runtime (RiverInputSnapshot (..))
import Seki.Run.XkbConfig
import Seki.Wire.Connection.ObjectTable (objectId)
import Seki.Wire.Types (objectIdValue)

bounded :: Int -> [Value] -> Value
bounded budget values = object ["items" .= kept, "count" .= length values, "truncated" .= (length kept /= length values)]
 where
  kept = fit 0 values
  fit _ [] = []
  fit used (v : vs)
    | used + size <= budget = v : fit (used + size) vs
    | otherwise = []
   where
    size = fromIntegral (Lazy.length (encode v)) + 1

inputValue :: RiverInputSnapshot -> Value
inputValue (InputUnavailable rules) =
  object
    [ "availability" .= ("unavailable" :: Text)
    , "requested_rule_count" .= length rules
    , "requested_rules" .= bounded 8000 (map ruleValue rules)
    , "devices" .= bounded 0 []
    ]
 where
  ruleValue rule =
    object
      [ "name_pattern" .= devicePatternText (ruleDeviceName rule)
      , "type" .= fmap deviceTypeName (ruleDeviceType rule)
      , "settings"
          .= bounded
            4096
            [ object ["setting" .= keyName (settingKey setting), "requested" .= settingValue setting, "status" .= ("unavailable" :: Text)]
            | setting <- ruleSettings rule
            ]
      ]
inputValue (InputAvailable inventory) =
  object
    [ "availability" .= ("available" :: Text)
    , "devices" .= bounded 24000 (map deviceValue (Map.elems (inputDevices inventory)))
    ]

inputStatusValue :: RiverInputSnapshot -> Value
inputStatusValue unavailable@(InputUnavailable _) = inputValue unavailable
inputStatusValue (InputAvailable inventory) =
  object
    [ "availability" .= ("available" :: Text)
    , "devices" .= bounded 8000 (map statusValue (Map.elems (inputDevices inventory)))
    ]
 where
  statusValue device =
    object
      [ "identity" .= identityValue device
      , "name" .= fmap (Text.take 128) (inputName device)
      , "name_truncated" .= maybe False ((> 128) . Text.length) (inputName device)
      , "libinput" .= facetName (facetLifecycle (inputLibinput device))
      , "xkb" .= facetName (xkbLifecycle (inputXkb device))
      , "results" .= resultsValue device
      ]

resultsValue :: InputDevice -> [Value]
resultsValue device =
  [ object
    [ "setting" .= keyName key
    , "requested" .= settingValue (settingDesired result)
    , "revision" .= settingRevision result
    , "status" .= outcomeName (settingOutcome result)
    ]
  | (key, result) <- Map.toList (inputResults device)
  ]

identityValue :: InputDevice -> Value
identityValue d = let InputGeneration generation = inputGeneration d in object ["generation" .= generation]

deviceValue :: InputDevice -> Value
deviceValue d =
  object
    [ "identity" .= identityValue d
    , "wire_id" .= objectIdValue (objectId (inputObject d))
    , "name" .= fmap (Text.take 128) (inputName d)
    , "name_truncated" .= maybe False ((> 128) . Text.length) (inputName d)
    , "type" .= fmap deviceTypeName (inputType d)
    , "lifecycle" .= lifecycleName (inputLifecycle d)
    , "libinput" .= facetName (facetLifecycle (inputLibinput d))
    , "xkb" .= keyboardValue d
    , "defaults" .= values (inputDefaults d)
    , "current" .= values (inputCurrent d)
    , "results" .= resultsValue d
    , "diagnostics" .= bounded 1024 [object ["message" .= Text.take 128 message, "truncated" .= (Text.length message > 128)] | message <- xkbDiagnostics (inputXkb d) <> facetDiagnostics (inputLibinput d)]
    ]
 where
  values m = [object ["setting" .= keyName k, "value" .= settingValue v] | (k, v) <- Map.toList m]

xkbValue :: RiverInputSnapshot -> Value
xkbValue (InputUnavailable _) = Null
xkbValue (InputAvailable inventory) = bounded 8000 [object ["identity" .= identityValue d, "name" .= fmap (Text.take 128) (inputName d), "name_truncated" .= maybe False ((> 128) . Text.length) (inputName d), "state" .= keyboardValue d] | d <- Map.elems (inputDevices inventory), inputType d == Just Keyboard]

keyboardValue :: InputDevice -> Value
keyboardValue d =
  object
    [ "availability" .= facetName (xkbLifecycle facet)
    , "layout_index" .= fmap fst (xkbLayout current)
    , "layout_name" .= fmap (Text.take 128) (xkbLayout current >>= snd)
    , "layout_name_truncated" .= maybe False ((> 128) . Text.length) (xkbLayout current >>= snd)
    , "caps_lock" .= xkbCapsLock current
    , "num_lock" .= xkbNumLock current
    , "scroll_lock" .= xkbScrollLock current
    ]
 where
  facet = inputXkb d
  current = xkbCurrent facet

deviceTypeName :: DeviceType -> Text
deviceTypeName v = case v of
  Keyboard -> "keyboard"
  Pointer -> "pointer"
  Touch -> "touch"
  Tablet -> "tablet"

lifecycleName :: DeviceLifecycle -> Text
lifecycleName v = case v of
  Discovering -> "discovering"
  Ready -> "ready"
  Removed -> "removed"

facetName :: FacetLifecycle -> Text
facetName v = case v of
  FacetDiscovering -> "discovering"
  FacetReady -> "ready"
  FacetUnavailable -> "unavailable"
  FacetRemoved -> "removed"

outcomeName :: SettingOutcome -> Text
outcomeName v = case v of
  SettingPending -> "pending"
  SettingInFlight -> "pending"
  SettingApplied -> "applied"
  SettingRejectedInvalid -> "rejected_invalid"
  SettingSent -> "sent"
  SettingObserved -> "observed"
  SettingUnavailable -> "unavailable"
  SettingUnsupported -> "unsupported"
  SettingRemoved -> "removed"

keyName :: SettingKey -> Text
keyName v = case v of
  RepeatInfoKey -> "repeat_info"
  ScrollFactorKey -> "scroll_factor"
  MapOutputKey -> "map_output"
  MapRectangleKey -> "map_rectangle"
  SendEventsKey -> "send_events"
  TapKey -> "tap"
  TapButtonMapKey -> "tap_button_map"
  DragKey -> "drag"
  DragLockKey -> "drag_lock"
  ThreeFingerDragKey -> "three_finger_drag"
  CalibrationMatrixKey -> "calibration_matrix"
  AccelerationKey -> "acceleration"
  AccelSpeedKey -> "accel_speed"
  NaturalScrollKey -> "natural_scroll"
  LeftHandedKey -> "left_handed"
  ClickMethodKey -> "click_method"
  ClickfingerButtonMapKey -> "clickfinger_button_map"
  MiddleEmulationKey -> "middle_emulation"
  ScrollMethodKey -> "scroll_method"
  ScrollButtonKey -> "scroll_button"
  ScrollButtonLockKey -> "scroll_button_lock"
  DisableWhileTypingKey -> "disable_while_typing"
  DisableWhileTrackpointKey -> "disable_while_trackpoint"
  RotationKey -> "rotation"
  KeymapKey -> "keymap"
  ActiveLayoutKey -> "active_layout"
  CapsLockKey -> "caps_lock"
  NumLockKey -> "num_lock"
  ScrollLockKey -> "scroll_lock"

settingValue :: InputSetting -> Value
settingValue v = case v of
  RepeatInfo rate delay -> object ["rate" .= rate, "delay" .= delay]
  ScrollFactor value -> toJSON (finite value)
  MapOutput value -> maybe Null (\name -> object ["name" .= Text.take 128 name, "truncated" .= (Text.length name > 128)]) value
  MapRectangle rectangle -> maybe Null (\(InputRectangle x y w h) -> toJSON [x, y, w, h]) rectangle
  SendEvents value -> toJSON (sendEventsModeName value)
  Tap value -> toJSON value
  TapButtonMap value -> toJSON (buttonMapName value)
  Drag value -> toJSON value
  DragLock value -> toJSON (dragLockModeName value)
  ThreeFingerDrag value -> toJSON (threeFingerDragModeName value)
  CalibrationMatrix values -> toJSON (map finite values)
  Acceleration (AccelerationCurves curves) -> toJSON [object ["type" .= curveType kind, "step" .= finite step, "point_count" .= length points, "points" .= map finite (take 8 points), "truncated" .= (length points > 8)] | AccelCurve kind step points <- take 3 curves]
  Acceleration (AccelerationProfile profile) -> toJSON (profileName profile)
  AccelSpeed value -> toJSON (finite value)
  NaturalScroll value -> toJSON value
  LeftHanded value -> toJSON value
  ClickMethod value -> toJSON (clickMethodName value)
  ClickfingerButtonMap value -> toJSON (buttonMapName value)
  MiddleEmulation value -> toJSON value
  ScrollMethod value -> toJSON (scrollMethodName value)
  ScrollButton value -> toJSON value
  ScrollButtonLock value -> toJSON value
  DisableWhileTyping value -> toJSON value
  DisableWhileTrackpoint value -> toJSON value
  Rotation value -> toJSON value
  Keymap path -> object ["path" .= take 128 path, "truncated" .= (length path > 128)]
  ActiveLayout (LayoutByIndex index) -> object ["index" .= index]
  ActiveLayout (LayoutByName name) -> object ["name" .= Text.take 128 name, "truncated" .= (Text.length name > 128)]
  CapsLock value -> toJSON value
  NumLock value -> toJSON value
  ScrollLock value -> toJSON value

finite :: (RealFloat a) => a -> Maybe a
finite v
  | isNaN v || isInfinite v = Nothing
  | otherwise = Just v

dragLockModeName :: DragLockMode -> Text
dragLockModeName v = case v of
  DragLockDisabled -> "drag_lock_disabled"
  DragLockTimeout -> "drag_lock_timeout"
  DragLockSticky -> "drag_lock_sticky"

sendEventsModeName :: SendEventsMode -> Text
sendEventsModeName v = case v of
  EventsEnabled -> "events_enabled"
  EventsDisabled -> "events_disabled"
  EventsDisabledOnExternalMouse -> "events_disabled_on_external_mouse"

threeFingerDragModeName :: ThreeFingerDragMode -> Text
threeFingerDragModeName v = case v of
  ThreeFingerDragDisabled -> "three_finger_drag_disabled"
  ThreeFingerDragEnabled -> "three_finger_drag_enabled"
  FourFingerDragEnabled -> "four_finger_drag_enabled"

buttonMapName :: ButtonMap -> Text
buttonMapName v = case v of
  LeftRightMiddle -> "left_right_middle"
  LeftMiddleRight -> "left_middle_right"

clickMethodName :: ClickMethod -> Text
clickMethodName v = case v of
  ClickNone -> "click_none"
  ClickButtonAreas -> "click_button_areas"
  ClickFinger -> "click_finger"

scrollMethodName :: ScrollMethod -> Text
scrollMethodName v = case v of
  ScrollNone -> "scroll_none"
  ScrollTwoFinger -> "scroll_two_finger"
  ScrollEdge -> "scroll_edge"
  ScrollOnButton -> "scroll_on_button"

profileName :: AccelProfile -> Text
profileName v = case v of
  AccelNone -> "accel_none"
  AccelFlat -> "accel_flat"
  AccelAdaptive -> "accel_adaptive"
  AccelCustom -> "accel_custom"

curveType :: CurveType -> Text
curveType v = case v of
  CurveFallback -> "curve_fallback"
  CurveMotion -> "curve_motion"
  CurveScroll -> "curve_scroll"
