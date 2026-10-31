{-# LANGUAGE OverloadedStrings #-}

-- | The ported reference configuration on real river (WP9).
module Seki.Integration.Reference (referenceScenarios) where

import Control.Exception (bracket, onException)
import Control.Monad (forM_, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (isInfixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Seki.Fixture.Probe
import Seki.Integration.Fixture (BorderStyle (..), Box (..), NestedRiver (..), Paint (..), outputPicture, pictureErrorWith, withKeyboard, withNestedRiver)
import Seki.Integration.Harness
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Process (proc)

referenceScenarios :: [(String, IO ())]
referenceScenarios = [("m2-reference-config", referenceConfig)]

{- | The @seki-reference-config@ executable runs with a private HOME and a PATH
of stubs that log each launch. Its startup commands run once; an @Emacs@
window goes to workspace @emacs@ while @term@ keeps its own two tiles; a
click focuses a tile and reaches its client; the terminal, volume, and lock
bindings launch their commands; and after Mod-q the Emacs window is still on
@emacs@, the bindings work, and no startup command ran again.
-}
referenceConfig :: IO ()
referenceConfig = withNestedRiver 1 $ \nested -> withSystemTempDirectory "seki-reference-home" $ \home -> do
  executable <- lookupEnv "SEKI_REFERENCE_CONFIG_EXECUTABLE" >>= maybe (failIntegration "SEKI_REFERENCE_CONFIG_EXECUTABLE is unset; run through ci/run-integration.sh") pure
  let bin = home </> "bin"
      launches = home </> "launches"
      base = riverEnvironment nested
      sekiEnvironment = [("HOME", home), ("PATH", bin <> ":" <> fromMaybe "/usr/bin:/bin" (lookup "PATH" base))] <> filter ((`notElem` ["HOME", "PATH"]) . fst) base
      socket = riverSocket nested
      input = runInputDriver base
      key keys = input (["-M", "logo"] <> concatMap (\k -> ["-k", k]) keys <> ["-m", "logo"])
      recorded = doesFileExist launches >>= \exists -> if exists then lines . BS8.unpack <$> BS8.readFile launches else pure []
      launched phase expected = void (waitUntil phase assertionTimeoutMicroseconds recorded ((== expected) . drop (length startupCommands)))
  forM_ (map (bin </>) ["nitrogen", "fcitx5", "xscreensaver", "xscreensaver-command", "alacritty", "amixer", "brightnessctl"] <> [home </> ".config/polybar/launch.sh"]) $ \stub -> do
    createDirectoryIfMissing True (takeDirectory stub)
    writeFile stub ("#!/bin/sh\nline=${0##*/}\nfor argument in \"$@\"; do line=\"$line $argument\"; done\nprintf '%s\\n' \"$line\" >> '" <> launches <> "'\n")
    setFileMode stub 0o755
  bracket (startManagedProcess "seki-reference-config" sekiEnvironment (proc executable [])) stopManagedProcess $ \seki ->
    ( do
        pid <- managedProcessPid seki >>= maybe (failIntegration "seki-reference-config has no pid") pure
        [(_, output)] <- waitUntil "seki shows the output" assertionTimeoutMicroseconds (outputNames socket) ((== 1) . length)
        void (runProcessCapture base "wlr-randr" ["--output", Text.unpack output, "--custom-mode", "800x600", "--pos", "0,0", "--scale", "1", "--transform", "normal"])
        let onOutput phase = outputPicture "m2-reference-config" socket output phase . pictureErrorWith portedBorders
            startedOnce = (== sort startupCommands) . sort . take (length startupCommands)
        _ <- waitUntil "each startup command runs once after the first output" assertionTimeoutMicroseconds recorded startedOnce
        withKeyboard base . withProbe socket $ \probe -> do
          first <- openWindow probe Blue
          emacsShown <- openWindowPending probe (windowSpec Green){specAppId = CustomAppId "Emacs"}
          second <- openWindow probe Green
          onOutput "term tiles its two windows and not the Emacs window" [Paint Green leftTile True, Paint Blue rightTile False]
          _ <- waitUntil "the newest term window has keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just second) . keyboardOwner)
          withVirtualPointer socket (800, 600) $ \pointer -> do
            pointerMoveTo pointer (600, 300)
            _ <- waitUntil "the pointer is over the right tile" assertionTimeoutMicroseconds (pointerSnapshot probe) ((== Just first) . pointerOwner)
            pointerPress pointer LeftButton
            pointerRelease pointer LeftButton
          _ <- waitUntil "a click focuses the right tile" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just first) . keyboardOwner)
          _ <- waitUntil "the click also reaches the right tile's client" assertionTimeoutMicroseconds (pointerSnapshot probe) ((== 1) . Map.findWithDefault 0 first . pointerPresses)
          onOutput "the clicked tile takes the focused border" [Paint Green leftTile False, Paint Blue rightTile True]
          key ["2"]
          emacs <- emacsShown
          _ <- waitUntil "the Emacs window has keyboard focus on emacs" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just emacs) . keyboardOwner)
          onOutput "emacs shows the Emacs window alone" [Paint Green fullTile True]
          key ["1"]
          onOutput "term still holds only its two windows" [Paint Green leftTile False, Paint Blue rightTile True]
          input ["-M", "logo", "-M", "shift", "-k", "Return", "-m", "shift", "-m", "logo"]
          launched "Mod-Shift-Return launches the terminal" ["alacritty"]
          input ["-k", "XF86AudioRaiseVolume"]
          launched "XF86AudioRaiseVolume raises the volume" ["alacritty", "amixer -D pulse sset Master 2%+"]
          input ["-M", "logo", "-M", "shift", "-k", "l", "-m", "shift", "-m", "logo"]
          launched "Mod-Shift-l locks the screen" ["alacritty", "amixer -D pulse sset Master 2%+", "xscreensaver-command --lock"]
          key ["q"]
          _ <- waitUntil "seki execs itself with --restore" assertionTimeoutMicroseconds (BS8.unpack <$> BS.readFile ("/proc/" <> show pid <> "/cmdline")) ("--restore" `isInfixOf`)
          _ <-
            waitUntil
              "restarted workspace binding selects Emacs"
              assertionTimeoutMicroseconds
              ( do
                  key ["2"]
                  frame <- captureOutput socket output
                  pure (pictureErrorWith portedBorders [Paint Green fullTile True] frame)
              )
              (== Nothing)
          onOutput "emacs still shows the Emacs window after the restart" [Paint Green fullTile True]
          _ <- waitUntil "the Emacs window has keyboard focus after the restart" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just emacs) . keyboardOwner)
          input ["-k", "XF86AudioLowerVolume"]
          launched "XF86AudioLowerVolume lowers the volume after the restart" ["alacritty", "amixer -D pulse sset Master 2%+", "xscreensaver-command --lock", "amixer -D pulse sset Master 2%-"]
          after <- recorded
          assertEqual "each startup command ran once across the restart" (sort startupCommands) (sort (filter (`elem` startupCommands) after))
    )
      `onException` (renderManagedProcessLogs seki >>= announce)
 where
  portedBorders = BorderStyle 1 (BS.pack [255, 0, 0]) (BS.pack [0, 0, 221])
  leftTile = Box 1 1 398 598
  rightTile = Box 401 1 398 598
  fullTile = Box 1 1 798 598

-- | What the stubs log for the four startup commands, in any order.
startupCommands :: [String]
startupCommands = ["launch.sh", "nitrogen --restore", "fcitx5", "xscreensaver --no-splash"]
