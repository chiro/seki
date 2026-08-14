{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Text qualified as Text
import Seki.Run.Startup (
  WaylandSession,
  closeWaylandSession,
  openWaylandSession,
  readWaylandGlobalCatalog,
 )
import Seki.Wire.Connection.Registry (
  GlobalAnnouncement (..),
  announcedGlobals,
  globalNameToWord,
 )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  openWaylandSession >>= \case
    Left errorValue -> failSmoke ("Wayland startup failed: " <> show errorValue)
    Right session ->
      bracket (pure session) closeWaylandSession printCatalog

printCatalog :: WaylandSession -> IO ()
printCatalog session = do
  result <- readWaylandGlobalCatalog session
  case result of
    Left errorValue -> failSmoke ("registry catalog failed: " <> show errorValue)
    Right catalog -> mapM_ printAnnouncement (announcedGlobals catalog)

printAnnouncement :: GlobalAnnouncement -> IO ()
printAnnouncement announcement =
  putStrLn
    ( show (globalNameToWord (announcedGlobalName announcement))
        <> "\t"
        <> Text.unpack (announcedGlobalInterface announcement)
        <> "\t"
        <> show (announcedGlobalVersion announcement)
    )

failSmoke :: String -> IO value
failSmoke message = hPutStrLn stderr message >> exitFailure
