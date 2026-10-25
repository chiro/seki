{- | A port of the maintainer's xmonad.hs, the M2 acceptance oracle, against
the public API alone. It differs from the original in these ways:

* @className =?@ becomes 'appId' '=?'. Wayland clients report an app_id, and
  Xwayland clients report their class through the same field.
* @focusFollowsMouse = False@ and @clickJustFocuses = False@ need no setting.
  seki has no focus-follows-mouse, and a click on a window both focuses it and
  reaches the client, since river delivers an unbound press to the window and
  only tells seki that it was interacted with.
* @avoidStruts@, @docks@, and @manageDocks@ are dropped. Layouts tile river's
  non-exclusive area, so a bar's exclusive zone is already avoided, and bars
  are layer surfaces that seki never manages.
* @ewmh@ and @disableEwmhManageDesktopViewport@ are dropped. Workspace output
  for bars is M3's IPC work.
* @ewmhFullscreen@ needs nothing: seki honors client fullscreen requests
  natively.
* 'defaultManageHook' stays last to float dialogs, as xmonad floats transient
  windows outside its manageHook.
* A held media or brightness key runs its command once and does not repeat,
  unlike xmonad on X11.
* 'spawnOnce' history survives a restart through seki's restore file.
-}
module PortedConfig (portedConfig) where

import Seki

portedConfig :: Config StdLayouts
portedConfig =
  additionalKeys
    def
      { modMask = Mod4
      , terminal = "alacritty"
      , workspaces = ["term", "emacs", "Web", "4", "5", "6", "7", "8", "9"]
      , borders = BorderConfig{borderWidth = 1, focusedBorderColor = opaque 0xff 0 0, unfocusedBorderColor = opaque 0 0 0xdd}
      , layoutHook = tiled ||| Mirror tiled ||| Full
      , manageHook =
          composeAll
            [ appId =? "Emacs" --> doShift "emacs"
            , appId =? "Discord" --> doShift "4"
            , appId =? "Steam" --> doShift "5"
            , defaultManageHook
            ]
      , startupHook = do
          spawnOnce "~/.config/polybar/launch.sh"
          spawnOnce "nitrogen --restore"
          spawnOnce "fcitx5"
          spawnOnce "xscreensaver --no-splash"
      }
    [ (KeyBinding (Mod4 .|. Shift) (Keysym 0x6c), spawn "xscreensaver-command --lock")
    , (KeyBinding (Mods 0) (Keysym 0x1008FF11), spawn "amixer -D pulse sset Master 2%-")
    , (KeyBinding (Mods 0) (Keysym 0x1008FF12), spawn "amixer -D pulse set Master toggle")
    , (KeyBinding (Mods 0) (Keysym 0x1008FF13), spawn "amixer -D pulse sset Master 2%+")
    , (KeyBinding (Mods 0) (Keysym 0x1008FF02), spawn "brightnessctl set 5%+")
    , (KeyBinding (Mods 0) (Keysym 0x1008FF03), spawn "brightnessctl set 5%-")
    ]
 where
  tiled = Tall 1 (3 / 100) (1 / 2)
  opaque r g b = BorderColor (channel r) (channel g) (channel b) maxBound
  channel byte = byte * 0x01010101
