{-# LANGUAGE OverloadedStrings #-}

module Seki.Internal.InputTest (tests) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Seki.Internal.Input
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Input rule model"
    [ testCase "patterns are anchored and case sensitive" $ do
        check "*Touch?ad*" "USB Touchpad 1" True
        check "Touchpad" "USB Touchpad" False
        check "Touchpad" "touchpad" False
        check "" "" True
        check "" "keyboard" False
        check "*" "" True
        check "?" "" False
        check "a**?b" "axyb" True
        check "a**?b" "ab" False
    , testCase "escapes match literal wildcard characters" $ do
        check "USB\\*\\?\\\\" "USB*?\\" True
        assertEqual "unknown escape" (Left (InvalidPatternEscape (Just 'x'))) (compileDevicePattern "\\x")
        assertEqual "trailing escape" (Left (InvalidPatternEscape Nothing)) (compileDevicePattern "device\\")
        assertEqual "pattern bound" (Left PatternTooLong) (compileDevicePattern (Text.replicate 257 "a"))
    , testCase "matching fields merge in order with last specified field winning" $ do
        allDevices <- patternOf "*"
        touchpads <- patternOf "*Touchpad*"
        let rules =
              [ InputRule allDevices Nothing [Tap False, NaturalScroll True, ScrollFactor 1]
              , InputRule touchpads (Just Pointer) [Tap True, ScrollFactor 2]
              , InputRule touchpads (Just Keyboard) [Tap False]
              ]
        assertEqual
          "merged fields"
          (Map.fromList [(TapKey, Tap True), (NaturalScrollKey, NaturalScroll True), (ScrollFactorKey, ScrollFactor 2)])
          (resolveInputRules rules "USB Touchpad" Pointer)
        assertEqual "no defaults" Map.empty (resolveInputRules [] "USB Touchpad" Pointer)
    , testCase "clearing a mapping and selecting layouts replaces the previous desired field" $ do
        devices <- patternOf "*"
        let rules = [InputRule devices Nothing [MapOutput (Just "DP-1"), ActiveLayout (LayoutByIndex 0)], InputRule devices Nothing [MapOutput Nothing, ActiveLayout (LayoutByName "Japanese")]]
        assertEqual
          "explicit clears survive"
          (Map.fromList [(MapOutputKey, MapOutput Nothing), (ActiveLayoutKey, ActiveLayout (LayoutByName "Japanese"))])
          (resolveInputRules rules "keyboard" Keyboard)
    , testCase "invalid settings are rejected before protocol use" $ do
        mapM_
          (\(setting, expected) -> assertEqual (show setting) (Left expected) (validateInputSetting setting))
          [ (RepeatInfo (-1) 200, NegativeRepeat)
          , (ScrollFactor (0 / 0), InvalidScrollFactor)
          , (ScrollFactor (1 / 0), InvalidScrollFactor)
          , (ScrollFactor 8388608, InvalidScrollFactor)
          , (MapRectangle (Just (InputRectangle 0 0 (-1) 2)), InvalidRectangle)
          , (MapOutput (Just ""), EmptyOutputName)
          , (CalibrationMatrix [1, 0], InvalidMatrix)
          , (CalibrationMatrix [1, 0, 0, 0, 1, 1 / 0], InvalidMatrix)
          , (AccelSpeed 1.01, InvalidAccelSpeed)
          , (Acceleration (AccelerationCurves [AccelCurve CurveMotion 0 [1, 2]]), InvalidCurve)
          , (Rotation 360, InvalidRotation)
          , (ActiveLayout (LayoutByIndex (-1)), InvalidLayout)
          , (ActiveLayout (LayoutByName (Text.replicate 1362 "界")), InvalidLayout)
          , (Keymap "", EmptyKeymapPath)
          ]
        mapM_
          (\setting -> assertEqual (show setting) (Right ()) (validateInputSetting setting))
          [RepeatInfo 0 0, ScrollFactor 0, AccelSpeed (-1), AccelSpeed 1, CalibrationMatrix [1, 0, 0, 0, 1, 0], MapRectangle (Just (InputRectangle (-100) 0 0 0)), Acceleration (AccelerationCurves [AccelCurve CurveMotion 1 [0, 1]])]
    ]
 where
  patternOf source = either (assertFailure . show) pure (compileDevicePattern source)
  check source name expected = do
    patternValue <- patternOf source
    assertEqual (show (source, name)) expected (matchesDevicePattern patternValue name)
