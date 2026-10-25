{-# LANGUAGE OverloadedStrings #-}

-- | Real-river restart scenarios (WP8).
module Seki.Integration.Restart (restartScenarios) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Exception (bracket, onException)
import Control.Monad (forM_, unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Fixture.Probe
import Seki.Integration.Fixture (Box (..), NestedRiver (..), Paint (..), outputPicture, pictureError, withHeldLogoIn, withKeyboard, withNestedRiver)
import Seki.Integration.Harness
import Seki.Internal.Config (Config (..), StdLayouts, def, defaultManageHook)
import Seki.Internal.Core.Action (restart, windows)
import Seki.Internal.Core.Scene (BorderColor (..), SceneStyle (..))
import Seki.Internal.Core.StackSet qualified as SS
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Config (policyConfigOf, runSessions)
import Seki.Run.Policy (PolicyConfig (..))
import Seki.Run.Restart (Launch (..), checkRestart)
import Seki.Run.Xkb (RiverKeyBinding (..))
import Seki.Wire.Transport (ConnectionTarget (UnixSocketPath))
import System.Directory (createDirectoryIfMissing, getSymbolicLinkTarget, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, setFileMode)
import System.Posix.Process (executeFile)
import System.Process (proc)
import System.Timeout (timeout)

restartScenarios :: [(String, IO ())]
restartScenarios =
  [ ("m2-restart", restartProcess)
  , ("m2-restart-fallback", restartFallback)
  ]

{- | The real @seki@ executable, started by a bare name found on PATH through a
symlink, restarts with Mod-q. Workspace 1 holds two tiles under Mirror Tall
and a dialog float moved by a logo drag; workspace 2 holds a fullscreen
window. Each workspace looks the same before and after, the process keeps
its pid, and the bindings still work.
-}
restartProcess :: IO ()
restartProcess = withNestedRiver 1 $ \nested -> withSystemTempDirectory "seki-restart-bin" $ \bin -> do
  executable <- lookupEnv "SEKI_EXECUTABLE" >>= maybe (failIntegration "SEKI_EXECUTABLE is unset; run through ci/run-integration.sh") pure
  createSymbolicLink executable (bin </> "seki")
  let base = riverEnvironment nested
      environment = [("PATH", bin <> ":" <> fromMaybe "/usr/bin:/bin" (lookup "PATH" base))] <> filter ((/= "PATH") . fst) base
      socket = riverSocket nested
      key keys = runInputDriver environment (["-M", "logo"] <> concatMap (\k -> ["-k", k]) keys <> ["-m", "logo"])
  bracket (startManagedProcess "seki" environment (proc "/bin/sh" ["-c", "exec seki"])) stopManagedProcess $ \seki ->
    ( do
        pid <- managedProcessPid seki >>= maybe (failIntegration "seki has no pid") pure
        [(_, output)] <- waitUntil "seki shows the output" assertionTimeoutMicroseconds (outputNames socket) ((== 1) . length)
        void (runProcessCapture environment "wlr-randr" ["--output", Text.unpack output, "--custom-mode", "800x600", "--pos", "0,0", "--scale", "1", "--transform", "normal"])
        withKeyboard environment . withProbe socket $ \probe -> do
          key ["2"]
          covering <- openWindow probe Blue
          requestFullscreen probe covering
          workspace2 <- settled socket output
          key ["1"]
          red <- openWindow probe Red
          _ <- openWindow probe Green
          key ["space"]
          dialog <- openWindowWith probe (windowSpec Blue){specParent = Just red, specCommit = CommitFixed 300 200}
          _ <- waitUntil "the dialog has keyboard focus" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just dialog) . keyboardOwner)
          withHeldLogoIn environment probe . withVirtualPointer socket (800, 600) $ \pointer -> do
            pointerMoveTo pointer (400, 300)
            _ <- waitUntil "the pointer is over the dialog" assertionTimeoutMicroseconds (pointerSnapshot probe) ((== Just dialog) . pointerOwner)
            pointerPress pointer LeftButton
            pointerMoveTo pointer (300, 240)
            pointerRelease pointer LeftButton
          workspace1 <- settled socket output
          ownerBefore <- keyboardOwner <$> keyboardSnapshot probe
          key ["q"]
          _ <- waitUntil "seki execs itself with --restore" assertionTimeoutMicroseconds (BS8.unpack <$> BS.readFile ("/proc/" <> show pid <> "/cmdline")) ("--restore" `isInfixOf`)
          cmdline <- BS8.split '\NUL' <$> BS.readFile ("/proc/" <> show pid <> "/cmdline")
          announce ("m2-restart: restarted as " <> show cmdline)
          assertEqual "argv[0] keeps the symlink found on PATH" (BS8.pack (bin </> "seki")) (head cmdline)
          sameImage "workspace 1 after the restart" socket output workspace1
          descriptors <- listDirectory ("/proc/" <> show pid <> "/fd")
          targets <- mapM (\fd -> getSymbolicLinkTarget ("/proc/" <> show pid <> "/fd/" <> fd)) descriptors
          announce ("m2-restart: descriptors after exec " <> show (zip descriptors targets))
          _ <- waitUntil "keyboard focus returns to the same window" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== ownerBefore) . keyboardOwner)
          key ["2"]
          sameImage "the fullscreen window covers workspace 2 again" socket output workspace2
          key ["1"]
          sameImage "workspace 1 shows again" socket output workspace1
    )
      `onException` (renderManagedProcessLogs seki >>= announce)

-- | Capture until two captures 300 ms apart agree.
settled :: FilePath -> Text -> IO Image
settled socket output = go Nothing (40 :: Int)
 where
  go previous tries = do
    image <- captureOutput socket output
    if Just image == previous
      then pure image
      else
        if tries == 0
          then failIntegration "the output never settled"
          else threadDelay 300000 >> go (Just image) (tries - 1)

-- | Wait up to ten seconds for the output to show exactly this image.
sameImage :: String -> FilePath -> Text -> Image -> IO ()
sameImage phase socket output expected = do
  start <- getMonotonicTimeNSec
  let loop = do
        image <- captureOutput socket output
        unless (image == expected) $ do
          now <- getMonotonicTimeNSec
          if now - start < 10 * 1000 * 1000 * 1000
            then threadDelay pollIntervalMicroseconds >> loop
            else do
              artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
              forM_ artifacts $ \directory -> do
                createDirectoryIfMissing True directory
                forM_ [("expected", expected), ("actual", image)] $ \(label, frame) ->
                  BS.writeFile (directory </> ("m2-restart-" <> label <> ".ppm")) (BS8.pack "P6\n800 600\n255\n" <> imageRgb frame)
              failIntegration (phase <> ": the output differs from before the restart")
  loop
  announce ("m2-restart: " <> phase <> " pixels verified")

{- | The restart supervisor in this process, with a target that passes the
check but whose exec fails: the session stops, the state is written, the
exec fails, and the next session reconnects on the socket path and restores.
Workspace 1 keeps its tile and its floating dialog, although river does not
send the dialog's parent again (M2-D08); workspace 2 keeps its window.
-}
restartFallback :: IO ()
restartFallback = withNestedRiver 1 $ \nested -> withSystemTempDirectory "seki-restart-target" $ \dir -> do
  let target = dir </> "seki"
      socket = riverSocket nested
      environment = riverEnvironment nested
      press k = runInputDriver environment ["-k", k]
  writeFile target "#!/nonexistent/interpreter\n"
  setFileMode target 0o755
  let launch =
        Launch
          { launchFirstTarget = Right (UnixSocketPath socket)
          , launchReconnectTarget = Right (UnixSocketPath socket)
          , launchTarget = Right target
          , launchStateDir = Right (riverRuntimeDirectory nested </> "seki")
          , launchExec = \path args env -> executeFile path False args (Just env)
          }
      config = fallbackConfig{policyCheckRestart = checkRestart launch}
  result <- newEmptyMVar
  void (forkIO (runSessions launch (const config) Nothing >>= putMVar result))
  flip onException (tryReadMVar result >>= announce . ("m2-restart-fallback sessions: " <>) . show) $ do
    [(_, output)] <- waitUntil "seki shows the output" assertionTimeoutMicroseconds (outputNames socket) ((== 1) . length)
    void (runProcessCapture environment "wlr-randr" ["--output", Text.unpack output, "--custom-mode", "800x600", "--pos", "0,0", "--scale", "1", "--transform", "normal"])
    withKeyboard environment . withProbe socket $ \probe -> do
      let onOutput phase = outputPicture "m2-restart-fallback" socket output phase . pictureError
      press "F2"
      _ <- openWindow probe Blue
      onOutput "workspace 2 tiles its window" [Paint Blue (Box 2 2 796 596) True]
      press "F1"
      red <- openWindow probe Red
      _ <- openWindowWith probe (windowSpec Green){specParent = Just red, specCommit = CommitFixed 300 200}
      onOutput "workspace 1 tiles the parent under its floating dialog" before
      press "F3"
      onOutput "workspace 1 is unchanged after the restart" before
      press "F2"
      onOutput "workspace 2 keeps its window after the restart" [Paint Blue (Box 2 2 796 596) True]
      press "F1"
      onOutput "workspace 1 shows again" before
    stopManagedProcess (riverProcess nested)
    timeout (10 * 1000 * 1000) (takeMVar result) >>= assertEqual "the restored session ends when river does" (Just (Right ()))
 where
  before = [Paint Red (Box 2 2 796 596) False, Paint Green (Box 250 200 300 200) True]

-- | F1 and F2 view workspaces 1 and 2; F3 restarts.
fallbackConfig :: PolicyConfig String StdLayouts
fallbackConfig =
  (policyConfigOf def{workspaces = ["1", "2", "3"], manageHook = defaultManageHook})
    { policyBindings = [(RiverKeyBinding (0xffbe + i) (River.RiverSeatV1ModifiersFlags 0) (Just 0), action) | (i, action) <- [(0, windows (SS.view "1")), (1, windows (SS.view "2")), (2, restart)]]
    , policySceneStyle = SceneStyle 2 (BorderColor maxBound 0 maxBound maxBound) (BorderColor 0x40404040 0x40404040 0x40404040 maxBound)
    , policyPointerBindings = []
    , policyLaunch = \_ -> failIntegration "unexpected spawn in the fallback scenario"
    , policyLog = announce
    }
