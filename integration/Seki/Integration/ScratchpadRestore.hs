{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.ScratchpadRestore (scratchpadRestoreScenarios) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Monad (forM_, void)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Seki.Fixture.Probe
import Seki.Integration.Fixture
import Seki.Integration.Harness
import Seki.Internal.Config (StdLayouts, def)
import Seki.Internal.Core.Action
import Seki.Internal.Core.Geometry
import Seki.Internal.Core.ManageHook (appId, (=?))
import Seki.Internal.Core.Restore
import Seki.Internal.Core.Scratchpad.Types
import Seki.Internal.Core.StackSet qualified as W
import Seki.Internal.Core.State
import Seki.Run.Config (policyConfigOf)
import Seki.Run.External
import Seki.Run.IPC (withIPCAt)
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (ConnectionTarget (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Process (shell)

type PadPolicy = Policy String StdLayouts

scratchpadRestoreScenarios :: [(String, IO ())]
scratchpadRestoreScenarios = [("m3-scratchpad-restart", restorePads RestartPads), ("m3-scratchpad-config-restore", restorePads ChangedDefinitions), ("m3-scratchpad-restore-missing-float", restorePads MissingFloatCopy)]

data RestoreMode = RestartPads | ChangedDefinitions | MissingFloatCopy deriving (Eq)

box :: OutputRect
box = either (error . show) OutputRect (mkSize 500 360 >>= mkRect (Point 80 60))

definitions :: [Scratchpad]
definitions = [Scratchpad name ("foot --app-id=seki-pad-" <> name <> " /bin/sh -c 'printf \"SEKI SCRATCHPAD " <> name <> "\\nStable restart identity\\nNo relaunch across reconnect\\n\"; exec cat'") (appId =? ("seki-pad-" <> name)) box | name <- ["term", "other"]]

withManager :: NestedRiver -> PolicyConfig String StdLayouts -> (PadPolicy -> RiverSession -> MVar (Either RiverRuntimeError RiverExit) -> IO a) -> IO a
withManager nested config action = do
  policy <- newPolicy config >>= requireRight "scratchpad restore policy"
  bracket (openRiverSessionAt (UnixSocketPath (riverSocket nested)) defaultObjectLimit (policyRuntimeHandlers policy) >>= requireRight "scratchpad restore session") closeRiverSession $ \session -> do
    ended <- newEmptyMVar
    dispatcher <- forkIO (runRiverSession session >>= putMVar ended)
    withIPCAt
      (riverRuntimeDirectory nested </> "seki" </> "ipc.sock")
      policy
      ( do
          void (waitUntil "restore manager attached" assertionTimeoutMicroseconds (readPolicyState policy) (\st -> case coreWorkspaces st of AttachedState{} -> True; _ -> False))
          action policy session ended
      )
      `finally` killThread dispatcher

apply :: PadPolicy -> Action String StdLayouts () -> IO ()
apply policy action = do
  receipt <- newEmptyTMVarIO
  submitExternal (policyExternalProducer policy) (ExternalInput action (Just receipt)) >>= assertEqual "restore action admitted" Accepted
  void (atomically (takeTMVar receipt))

awaitOwner :: PadPolicy -> String -> IO ScratchpadState
awaitOwner policy name = do
  slot <- waitUntil "restored pad ownership" assertionTimeoutMicroseconds (Map.lookup name . coreScratchpads <$> readPolicyState policy) (\s -> case s of Just ShownPad{} -> True; _ -> False)
  maybe (failIntegration "missing owner") pure slot

capture :: NestedRiver -> String -> IO ()
capture nested label = do
  directory <- getEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
  createDirectoryIfMissing True directory
  outputs <- outputNames (riverSocket nested)
  forM_ outputs $ \(_, name) -> saveOutputCapture (riverSocket nested) name (directory </> label <> "-" <> Text.unpack name <> ".png")

restorePads :: RestoreMode -> IO ()
restorePads mode = withNestedRiver 2 $ \nested -> do
  children <- newIORef []
  launches <- newIORef (0 :: Int)
  let changed = mode == ChangedDefinitions
      label = case mode of
        RestartPads -> "lane7"
        ChangedDefinitions -> "lane10"
        MissingFloatCopy -> "restore-missing-float"
      config =
        (policyConfigOf def)
          { policyScratchpads = definitions
          , policyCheckRestart = pure (Right ())
          , policyLaunch = \cmd -> do
              modifyIORef' launches (+ 1)
              child <- startManagedProcess "scratchpad restart terminal" (riverEnvironment nested) (shell cmd)
              modifyIORef' children (child :)
          }
      stopChildren = readIORef children >>= mapM_ stopManagedProcess
  ( withKeyboard (riverEnvironment nested) $ withProbe (riverSocket nested) $ \probe -> do
      (saved, ordinary) <- withManager nested config $ \policy _ ended -> do
        ordinary <- openWindow probe Green
        apply policy (toggleScratchpad "term")
        void (awaitOwner policy "term")
        apply policy (toggleScratchpad "term" >> toggleScratchpad "other")
        void (awaitOwner policy "other")
        capture nested (label <> "-before")
        apply policy restart
        requireRuntimeExit ended >>= assertEqual "scratchpad restart stop handshake" (Right RiverStopped)
        saved <- readRestoreState policy
        pure (saved, ordinary)
      assertEqual "two actual initial launches" 2 =<< readIORef launches
      if changed
        then do
          outputs <- outputNames (riverSocket nested)
          case outputs of
            (_, output) : _ -> void (runProcessCapture (riverEnvironment nested) "wlr-randr" ["--output", Text.unpack output, "--off"])
            _ -> failIntegration "no output to remove"
        else pure ()
      let restoreSaved = if mode == MissingFloatCopy then saved{savedFloats = []} else saved
          next = config{policyRestore = Just restoreSaved, policyScratchpads = if changed then drop 1 definitions else definitions}
      announce ("SCRATCHPAD_RESTORE_FLOATS_INPUT " <> show (savedFloats restoreSaved))
      withManager nested next $ \policy session _ -> do
        void (awaitOwner policy "other")
        restored <- readRestoreState policy
        announce ("SCRATCHPAD_RESTORE_BEFORE " <> show (savedScratchpads saved))
        announce ("SCRATCHPAD_RESTORE_AFTER " <> show (savedScratchpads restored))
        if changed
          then do
            st <- readPolicyState policy
            assertEqual "removed name not owned" Nothing (Map.lookup "term" (coreScratchpads st))
            let firstKey = [key | SavedScratchpad "term" key _ _ _ <- savedScratchpads saved]
                ordinaryKeys = concatMap (maybe [] W.integrate . savedStack) (savedWorkspaces restored)
            assertEqual "removed hidden survivor released into user workspace" True (all (`elem` ordinaryKeys) firstKey)
            capture nested (label <> "-definition-output-removed")
          else do
            assertEqual "shown/hidden identifiers and geometry survive reconnect" (savedScratchpads saved) (savedScratchpads restored)
            apply policy (toggleScratchpad "term")
            void (awaitOwner policy "term")
            capture nested (label <> "-hidden-returned")
            apply policy (toggleScratchpad "term" >> toggleScratchpad "other")
            void (waitUntil "real keyboard restored to ordinary window" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== Just ordinary) . keyboardOwner))
            before <- Map.findWithDefault 0 ordinary . pressedKeys <$> keyboardSnapshot probe
            runInputDriver (riverEnvironment nested) ["-k", "a"]
            void (waitUntil "ordinary input works after pad restart" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((> before) . Map.findWithDefault 0 ordinary . pressedKeys))
        assertEqual "no restore process launch" 2 =<< readIORef launches
        -- Keep the session alive until clients have actually closed.
        stopChildren
        void (waitUntil "restart terminals closed" assertionTimeoutMicroseconds (liveWindows . liveInventoryOf <$> readRiverInventory session) ((<= 1) . Map.size))
    )
    `finally` stopChildren
