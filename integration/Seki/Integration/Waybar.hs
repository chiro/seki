{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Waybar (waybarScenarios) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, onException)
import Control.Monad (forM_, void)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Fixture.Probe
import Seki.Integration.Fixture
import Seki.Integration.Harness
import Seki.Internal.Config (def)
import Seki.Run.Config (policyConfigOf)
import Seki.Run.IPC (withIPCAt)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.Process (proc)

waybarScenarios :: [(String, IO ())]
waybarScenarios = [("m3-ipc-waybar", waybar)]

waybar :: IO ()
waybar = withFixture (policyConfigOf def) "m3-ipc-waybar" 1 $ \fixture probe -> do
  root <- maybe (failIntegration "missing runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
  cli <- getEnv "SEKICTL_EXECUTABLE"
  executable <- fromMaybe "waybar" <$> lookupEnv "SEKI_WAYBAR_EXECUTABLE"
  libraryPath <- lookupEnv "SEKI_WAYBAR_LIBRARY_PATH"
  cwd <- getCurrentDirectory
  output <- case mOutputs fixture of [name] -> pure name; names -> failIntegration ("expected one output, got " <> show names)
  let config = root </> "waybar.json"
      style = root </> "waybar.css"
      marker = root </> "title-executed"
      script = cwd </> "examples/waybar/seki.py"
      base = mEnvironment fixture
      environment =
        [("PATH", takeDirectory cli <> ":" <> fromMaybe "/usr/bin:/bin" (lookup "PATH" base)), ("GDK_BACKEND", "wayland"), ("SEKI_WAYBAR_SCRIPT", script)]
          <> maybe [] (\path -> [("LD_LIBRARY_PATH", path)]) libraryPath
          <> filter ((`notElem` ("PATH" : "GDK_BACKEND" : "SEKI_WAYBAR_SCRIPT" : maybe [] (const ["LD_LIBRARY_PATH"]) libraryPath)) . fst) base
      capture = captureOutput (mSocket fixture) output
      waitFrame label predicate = do
        found <- waitUntil label assertionTimeoutMicroseconds (capture >>= \frame -> pure (if predicate frame then Just frame else Nothing)) isJust
        maybe (failIntegration "missing matched frame") pure found
      strip frame = BS.take (imageWidth frame * 32 * 3) (imageRgb frame)
      white frame = length [() | x <- [0 .. imageWidth frame - 1], y <- [0 .. 31], let { p = pixel frame x y }, BS.all (> 180) p] > 10
      red frame = or [r > 150 && g < 100 && b < 100 | x <- [0 .. imageWidth frame - 1], y <- [0 .. 31], let p = pixel frame x y, [r, g, b] <- [BS.unpack p]]
  BL.writeFile
    config
    ( encode
        ( object
            [ "layer" .= ("top" :: String)
            , "position" .= ("top" :: String)
            , "height" .= (32 :: Int)
            , "output" .= output
            , "modules-left" .= (["custom/seki"] :: [String])
            , "custom/seki" .= object ["exec" .= ("exec python3 \"$SEKI_WAYBAR_SCRIPT\"" :: String), "return-type" .= ("json" :: String), "format" .= ("{}" :: String)]
            ]
        )
    )
  writeFile style "* { font-family: DejaVu Sans; font-size: 14px; min-height: 0; } window#waybar { background: #202020; color: #ffffff; } #custom-seki { padding: 0 8px; }\n"
  version <- runProcessCapture environment executable ["--version"]
  announce ("M3_WAYBAR_VERSION " <> show version)
  withIPCAt (root </> "seki" </> "ipc.sock") (mPolicy fixture) $ withPrivateDbus environment $ \dbusEnvironment -> do
    redWindow <- openWindow probe Red
    setTitle probe redWindow "before"
    bracket (startManagedProcess "real Waybar" dbusEnvironment (proc executable ["--config", config, "--style", style])) stopManagedProcess $ \bar -> do
      before <- waitFrame "Waybar text rendered" white `onException` (renderManagedProcessLogs bar >>= announce)
      saveCapture fixture "waybar-initial"
      let hostile = Text.pack ("<span foreground='red'>PWN</span> & $(touch '" <> marker <> "') \\\"quote\\\"")
      setTitle probe redWindow hostile
      after <- waitFrame "title changes actual Waybar pixels" (\frame -> white frame && strip frame /= strip before)
      assertEqual "title cannot inject red Pango markup" False (red after)
      doesFileExist marker >>= assertEqual "title cannot execute shell code" False
      saveCapture fixture "waybar-adversarial"
      void (runProcessCapture environment cli ["workspace", "3"])
      green <- openWindow probe Green
      setTitle probe green "after"
      _ <- waitFrame "workspace change updates actual bar" (\frame -> white frame && strip frame /= strip after)
      saveCapture fixture "waybar-workspace"
      logs <- renderManagedProcessLogs bar
      announce ("M3_WAYBAR_LOGS " <> logs)
      video <- lookupEnv "SEKI_CAPTURE_VIDEO"
      artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
      case (video, artifacts) of
        (Just "1", Just directory) -> do
          let frames = directory </> "waybar-video"
          createDirectoryIfMissing True frames
          forM_ [0 .. 59 :: Int] $ \n -> do
            if n `mod` 10 == 0 then void (runProcessCapture environment cli ["workspace", if even (n `div` 10) then "1" else "3"]) else pure ()
            stamp <- getMonotonicTimeNSec
            announce ("M3_WAYBAR_FRAME " <> show n <> " " <> show stamp)
            saveOutputCapture (mSocket fixture) output (frames </> "frame-" <> replicate (3 - length (show n)) '0' <> show n <> ".png")
            threadDelay 500000
        _ -> pure ()
      doesFileExist marker >>= assertEqual "no title shell execution after updates" False
