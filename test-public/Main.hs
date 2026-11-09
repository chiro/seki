{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ConfigMain (customConfig, fullConfig, mediaConfig, raiseVolume, tolerantConfig)
import Control.Monad (unless)
import PortedConfig (dailyConfig, portedConfig)
import Seki
import Seki.Core.StackSet (differentiate, integrate, integrate')
import System.Exit (exitFailure)

main :: IO ()
main = do
  check "default input rules are empty" (null (inputRules def))
  case compileDevicePattern "*keyboard*" of
    Left err -> failWith (show err)
    Right patternValue -> do
      let config = def{inputRules = [InputRule patternValue (Just Keyboard) [RepeatInfo 37 410]]}
      check "public input rule construction" (length (inputRules config) == 1)
      check "public invalid setting is explicit" (validateInputSetting (AccelSpeed (0 / 0)) == Left InvalidAccelSpeed)
  check "public invalid matcher is explicit" (compileDevicePattern "bad\\q" == Left (InvalidPatternEscape (Just 'q')))
  check "negative size is rejected" (either Just (const Nothing) (mkSize (-1) 0) == Just (NegativeSize (-1) 0))
  case mkSize 1001 10 >>= mkRect (Point (-1920) 0) of
    Left err -> failWith (show err)
    Right rect -> do
      let (left, right) = splitHorizontallyBy (1 / 2) rect
      check "ratio split widths" ((rectWidth left, rectWidth right) == (500, 501))
  check "custom layout configuration" (description (layoutHook customConfig) == "Custom")
  check "changed layout type" (description (layoutHook fullConfig) == "Full")
  check "differentiate focuses the first element" (differentiate [1, 2, 3 :: Int] == Just (Stack 1 [] [2, 3]))
  check "integrate lists a stack from the top" (integrate (Stack 2 [1] [3 :: Int]) == [1, 2, 3])
  check "integrate' of an empty list is empty" (null (integrate' (differentiate ([] :: [Int]))))
  check "the first layout is selected" (description layouts == "Tall")
  check "NextLayout selects the next layout" (fmap description (pureMessage layouts (SomeMessage NextLayout)) == Just "Mirror Tall")
  check "a modifier-less media key is bound" (raiseVolume `elem` bound mediaConfig)
  check "the media key joins the default bindings" (length (bound mediaConfig) == length (bound def) + 1)
  check "removeKeys removes the media key" (raiseVolume `notElem` bound (removeKeys mediaConfig [raiseVolume]))
  check "the default layouts crop" (sizePolicy (layoutHook def) == Crop)
  check "tolerant layouts tolerate" (sizePolicy (layoutHook tolerantConfig) == Tolerate)
  check "tolerant keeps the layout description" (description (layoutHook tolerantConfig) == "Tall")
  check "def moves with mod and button 1 and resizes with mod and button 3" (pointerBindings def def == [(PointerBinding Mod4 Button1, MoveWindow), (PointerBinding Mod4 Button3, ResizeWindow)])
  check "a pointer binding joins the defaults" (map fst (pointerBindings customConfig customConfig) == [PointerBinding (Mod4 .|. Shift) Button1, PointerBinding Mod4 Button1, PointerBinding Mod4 Button3])
  check "the port uses Mod4 and alacritty" (modMask portedConfig == Mod4 && terminal portedConfig == "alacritty")
  check "the port names its workspaces" (workspaces portedConfig == ["term", "emacs", "Web", "4", "5", "6", "7", "8", "9"])
  check "the port draws a 1-pixel red focused and #0000dd unfocused border" (borders portedConfig == BorderConfig 1 (BorderColor 0xffffffff 0 0 0xffffffff) (BorderColor 0 0 0xdddddddd 0xffffffff))
  check "the port cycles Tall, Mirror Tall, and Full" (layoutHook portedConfig == layouts)
  check "the port adds its lock and media keys" (all (`elem` bound portedConfig) portedKeys)
  check "the port's added keys replace no default" (length (bound portedConfig) == length (bound def) + length portedKeys)
  case dailyConfig "/keymaps/daily.xkb" of
    Left err -> failWith err
    Right daily -> do
      check "the daily configuration keeps the port's workspaces and layouts" (workspaces daily == workspaces portedConfig && layoutHook daily == layouts)
      check "the daily configuration keeps the port's borders" (borders daily == borders portedConfig)
      check "the daily configuration keeps every port key" (all (`elem` bound daily) (bound portedConfig))
      check "Mod-grave is the one added key, and Mod-Shift-l is rebound in place" (KeyBinding Mod4 (Keysym 0x60) `elem` bound daily && length (bound daily) == length (bound portedConfig) + 1)
      check "the daily configuration has one alacritty scratchpad" (map (\p -> (scratchpadName p, scratchpadCommand p)) (scratchpads daily) == [("term", "alacritty --class seki-scratchpad")])
      check "the scratchpad is 1280 by 800, centered on a 1920 by 1200 output" (map scratchpadGeometry (scratchpads daily) == [OutputRect padRect | Right padRect <- [mkSize 1280 800 >>= mkRect (Point 320 200)]])
      check "the keyboard rule loads the keymap and repeats at 25 Hz after 660 ms" (map ruleSettings (filter ((== Just Keyboard) . ruleDeviceType) (inputRules daily)) == [[Keymap "/keymaps/daily.xkb", RepeatInfo 25 660]])
      check "the touchpad rule taps, clicks by finger count, and scrolls naturally" (map ruleSettings (filter ((== Just Pointer) . ruleDeviceType) (inputRules daily)) == [[Tap True, ClickMethod ClickFinger, NaturalScroll True]])
      check "the touchpad rule names the oracle touchpad" (map ruleDeviceName (filter ((== Just Pointer) . ruleDeviceType) (inputRules daily)) == [touchpad | Right touchpad <- [compileDevicePattern "ELAN06C7:00 04F3:3193 Touchpad"]])
 where
  check label ok = unless ok (failWith label)
  failWith message = putStrLn ("public API check failed: " <> message) >> exitFailure
  bound config = map fst (keys config config)
  layouts = Tall 1 (3 / 100) (1 / 2) ||| Mirror (Tall 1 (3 / 100) (1 / 2)) ||| Full
  portedKeys = KeyBinding (Mod4 .|. Shift) (Keysym 0x6c) : [KeyBinding (Mods 0) (Keysym symbol) | symbol <- [0x1008FF11, 0x1008FF12, 0x1008FF13, 0x1008FF02, 0x1008FF03]]
