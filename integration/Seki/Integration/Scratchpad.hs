{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Scratchpad (scratchpadScenarios) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, takeTMVar)
import Control.Exception (bracket, finally)
import Control.Monad (forM_, replicateM, void)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Fixture.Probe
import Seki.Integration.Fixture
import Seki.Integration.Harness
import Seki.Internal.Config (StdLayouts, def)
import Seki.Internal.Core.Action
import Seki.Internal.Core.Geometry
import Seki.Internal.Core.Layout (ChangeLayout (..))
import Seki.Internal.Core.ManageHook (appId, (=?))
import Seki.Internal.Core.Restore (LiveInventory (..), WindowKey)
import Seki.Internal.Core.Scratchpad.Types
import Seki.Internal.Core.StackSet qualified as W
import Seki.Internal.Core.State
import Seki.Run.Config (policyConfigOf)
import Seki.Run.External
import Seki.Run.IPC (withIPCAt)
import Seki.Run.Inventory (RiverSize (..), RiverWindowState (..), riverInventoryWindows)
import Seki.Run.Policy
import Seki.Run.Runtime (readRiverInputSnapshot, readRiverInventory)
import Seki.Wire.Connection.ObjectTable (objectId)
import Seki.Wire.Types (objectIdValue)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), shell)
import System.Timeout (timeout)
import Text.Printf (printf)

type PadFixture = Fixture StdLayouts

scratchpadScenarios :: [(String, IO ())]
scratchpadScenarios =
  [ ("m3-scratchpad-lifecycle", withPad "m3-scratchpad-lifecycle" 2 lifecycle)
  , ("m3-scratchpad-timeout", withPad "m3-scratchpad-timeout" 1 timeoutRecovery)
  , ("m3-scratchpad-pending", withPad "m3-scratchpad-pending" 1 pending)
  , ("m3-scratchpad-focus", withPad "m3-scratchpad-focus" 1 focusInput)
  , ("m3-scratchpad-bring", withPad "m3-scratchpad-bring" 2 bring)
  , ("m3-scratchpad-layouts", withPad "m3-scratchpad-layouts" 1 layouts)
  , ("m3-scratchpad-fullscreen", withPad "m3-scratchpad-fullscreen" 1 fullscreen)
  , ("m3-scratchpad-close", withPad "m3-scratchpad-close" 1 closeAgain)
  , ("m3-scratchpad-perf", withPad "m3-scratchpad-perf" 1 togglePerformance)
  , ("m3-scratchpad-duplicates", withPad "m3-scratchpad-duplicates" 1 duplicates)
  ]

padBox :: OutputRect
padBox = either (error . show) OutputRect (mkSize 520 360 >>= mkRect (Point 80 60))

withPad :: String -> Int -> (PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()) -> IO ()
withPad name outputs action = do
  environment <- newIORef []
  children <- newIORef []
  launches <- newIORef 0
  let command = "foot --app-id=seki-scratchpad /bin/sh -c 'printf \"SEKI NAMED SCRATCHPAD\\nLifecycle: open / hide / show / bring\\nIdentity stays with this terminal.\\nType here after show.\\n\"; exec cat'"
      config =
        (policyConfigOf def)
          { policyScratchpads = [Scratchpad "term" (if name == "m3-scratchpad-timeout" then "true" else (if name == "m3-scratchpad-pending" then "sleep 1; " else "") <> command) (appId =? "seki-scratchpad") padBox]
          , policyLaunch = \cmd -> do
              modifyIORef' launches (+ 1)
              envValue <- readIORef environment
              child <- startManagedProcess "scratchpad terminal" envValue (shell cmd){env = Just envValue}
              modifyIORef' children (child :)
          }
  withFixture config name outputs $ \fixture probe -> do
    writeIORef environment (mEnvironment fixture)
    ordinary <- openWindow probe Green
    root <- maybe (failIntegration "private runtime missing") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
    withIPCAt
      (root </> "seki" </> "ipc.sock")
      (mPolicy fixture)
      ( do
          cli <- getEnv "SEKICTL_EXECUTABLE"
          let script =
                if name == "m3-scratchpad-perf"
                  then "printf 'SEKI PERFORMANCE CONTEXT\\n'; " <> cli <> " snapshot; exec cat"
                  else "while :; do printf '\\033[H\\033[2JSEKI LIVE SNAPSHOT\\n\\n'; " <> cli <> " snapshot; sleep 1; done"
              quote text = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) text <> "'"
              context = "foot --app-id=seki-scratchpad-context /bin/sh -c " <> quote script
          bracket (startManagedProcess "scratchpad context" (mEnvironment fixture) (shell context)) stopManagedProcess $ \_ -> do
            void (waitUntil "context terminal managed" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) ((>= 2) . Map.size . liveWindows . liveInventoryOf))
            ordinaryKey <- ordinaryIdentity fixture
            void (apply fixture (windows (W.focusWindow ordinaryKey)))
            focus fixture probe (Just ordinary)
            action fixture probe ordinary launches
      )
      `finally` (readIORef children >>= mapM_ stopManagedProcess)

apply :: PadFixture -> Action String StdLayouts () -> IO Integer
apply fixture action = do
  receipt <- newEmptyTMVarIO
  submitExternal (policyExternalProducer (mPolicy fixture)) (ExternalInput action (Just receipt)) >>= assertEqual "scratchpad bounded admission" Accepted
  atomically (takeTMVar receipt)

stateOf :: PadFixture -> IO (Maybe ScratchpadState)
stateOf fixture = Map.lookup "term" . coreScratchpads <$> readPolicyState (mPolicy fixture)

waitShown :: PadFixture -> IO W.WindowId
waitShown fixture = do
  slot <- waitUntil "pad owns a mapped identity" assertionTimeoutMicroseconds (stateOf fixture) (\s -> case s of Just ShownPad{} -> True; _ -> False)
  case slot of Just (ShownPad w _) -> pure w; _ -> failIntegration "missing owner"

focus :: PadFixture -> Probe -> Maybe ProbeWindow -> IO ()
focus _ probe wanted = void (waitUntil "actual keyboard owner after render" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((== wanted) . keyboardOwner))

open :: PadFixture -> Probe -> IO W.WindowId
open fixture probe = void (apply fixture (toggleScratchpad "term")) >> waitShown fixture <* focus fixture probe Nothing

hide :: PadFixture -> Probe -> ProbeWindow -> IO ()
hide fixture probe ordinary = void (apply fixture (toggleScratchpad "term")) >> focus fixture probe (Just ordinary)

lifecycle :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
lifecycle fixture probe ordinary launches = do
  owner <- open fixture probe
  saveCapture fixture "lane1-open-content"
  hide fixture probe ordinary
  cli <- getEnv "SEKICTL_EXECUTABLE"
  response <- runProcessCapture (mEnvironment fixture) cli ["scratchpad", "term"]
  announce ("SCRATCHPAD_CLI_APPLIED " <> show response)
  same <- waitShown fixture
  focus fixture probe Nothing
  assertEqual "same owned window returns" owner same
  readIORef launches >>= assertEqual "return without relaunch" 1
  saveCapture fixture "lane1-return-content"
  record <- lookupEnv "SEKI_SCRATCHPAD_RECORD"
  forM_ record $ \directory -> do
    createDirectoryIfMissing True directory
    forM_ [1 .. 60 :: Int] $ \frame -> do
      case frame of
        10 -> hide fixture probe ordinary
        20 -> void (apply fixture (toggleScratchpad "term")) >> focus fixture probe Nothing
        30 -> void (apply fixture (windows (W.view "2") >> toggleScratchpad "term"))
        40 -> void (apply fixture (toggleScratchpad "term"))
        50 -> void (apply fixture (toggleScratchpad "term"))
        _ -> pure ()
      stamp <- getMonotonicTimeNSec
      st <- readPolicyState (mPolicy fixture)
      forM_ (zip [1 :: Int ..] (mOutputs fixture)) $ \(outputIndex, output) ->
        saveOutputCapture (mSocket fixture) output (directory </> printf "frame-%03d-output%d.png" frame outputIndex)
      appendFile (directory </> "frames.tsv") (show frame <> "\t" <> show stamp <> "\t" <> show (Map.lookup "term" (coreScratchpads st)) <> "\n")
      threadDelay 650000

pending :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
pending fixture probe _ launches = do
  void (apply fixture (toggleScratchpad "term"))
  forM_ [1 .. 100 :: Int] $ \_ -> void (apply fixture (toggleScratchpad "term"))
  void (waitShown fixture)
  focus fixture probe Nothing
  readIORef launches >>= assertEqual "pending burst launches once" 1
  saveCapture fixture "lane2-pending-content"

focusInput :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
focusInput fixture probe ordinary _ = do
  void (open fixture probe)
  hide fixture probe ordinary
  before <- Map.findWithDefault 0 ordinary . pressedKeys <$> keyboardSnapshot probe
  runInputDriver (mEnvironment fixture) ["-k", "a"]
  void (waitUntil "ordinary tile receives actual key after hide" assertionTimeoutMicroseconds (keyboardSnapshot probe) ((> before) . Map.findWithDefault 0 ordinary . pressedKeys))
  saveCapture fixture "lane3-hidden-input"
  void (apply fixture (toggleScratchpad "term"))
  focus fixture probe Nothing
  saveCapture fixture "lane3-restored-content"

bring :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
bring fixture probe _ launches = do
  owner <- open fixture probe
  case drop 1 (mOutputs fixture) of
    output : _ -> void (runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", Text.unpack output, "--custom-mode", "400x300"])
    _ -> failIntegration "second output missing"
  void (apply fixture (windows (W.view "2") >> toggleScratchpad "term"))
  void (waitUntil "actual float dimensions clamped on smaller output" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (any (\w -> riverWindowAppId w == Just "seki-scratchpad" && riverWindowDimensions w == Just (RiverSize 396 296)) . riverInventoryWindows))
  same <- waitShown fixture
  assertEqual "bring preserves identity" owner same
  st <- readPolicyState (mPolicy fixture)
  case coreWorkspaces st of
    AttachedState ss _ -> assertEqual "pad moved to current workspace" (Just "2") (W.findTag owner ss)
    _ -> failIntegration "bring headless"
  focus fixture probe Nothing
  readIORef launches >>= assertEqual "bring no new launch" 1
  saveCapture fixture "lane4-bring-content"

layouts :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
layouts fixture probe _ _ = do
  owner <- open fixture probe
  forM_ [1 :: Int .. 3] $ \n -> do
    void (apply fixture (sendMessage NextLayout))
    st <- readPolicyState (mPolicy fixture)
    case coreWorkspaces st of
      AttachedState ss _ -> assertEqual "pad remains float across layouts" True (Map.member owner (W.floating ss))
      _ -> failIntegration "layout headless"
    focus fixture probe Nothing
    saveCapture fixture ("lane5-layout" <> show n)

fullscreen :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
fullscreen fixture probe ordinary _ = do
  owner <- open fixture probe
  void (apply fixture (toggleFullscreen owner))
  saveCapture fixture "lane6-fullscreen-content"
  hide fixture probe ordinary
  st <- readPolicyState (mPolicy fixture)
  assertEqual "fullscreen cleared" False (owner `elem` coreFullscreen st)
  void (apply fixture (toggleScratchpad "term"))
  focus fixture probe Nothing
  saveCapture fixture "lane6-return-usable"

closeAgain :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
closeAgain fixture probe _ launches = do
  owner <- open fixture probe
  oldKey <- identifierFor fixture owner
  void (apply fixture kill)
  void (waitUntil "close clears pad" assertionTimeoutMicroseconds (stateOf fixture) (== Nothing))
  next <- open fixture probe
  newKey <- identifierFor fixture next
  assertEqual "close then new stable identity" False (oldKey == newKey)
  readIORef launches >>= assertEqual "close then one new launch" 2
  saveCapture fixture "lane8-new-content"

duplicates :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
duplicates fixture probe _ _ = do
  owner <- open fixture probe
  _ <- openWindowWith probe (windowSpec Red){specAppId = CustomAppId "seki-scratchpad"}
  same <- waitShown fixture
  assertEqual "duplicate cannot replace owner" owner same
  saveCapture fixture "lane9-extra-ordinary"

identifierFor :: PadFixture -> W.WindowId -> IO WindowKey
identifierFor fixture owner = do
  keys <-
    waitUntil
      "stable river identifier"
      assertionTimeoutMicroseconds
      ((Map.keys . Map.filter (== owner) . liveWindows . liveInventoryOf) <$> readRiverInventory (mSession fixture))
      (not . null)
  case keys of key : _ -> pure key; _ -> failIntegration "missing stable identifier"

ordinaryIdentity :: PadFixture -> IO W.WindowId
ordinaryIdentity fixture = do
  inventory <- readRiverInventory (mSession fixture)
  case [W.WindowId (objectIdValue (objectId (riverWindowObject w))) | w <- riverInventoryWindows inventory, riverWindowAppId w == Just "seki-color-probe-1"] of
    w : _ -> pure w
    _ -> failIntegration "ordinary probe missing"

{- | A receipt is only application. Endpoints below additionally require the
native probe's keyboard enter/leave after the visibility transition.
-}
togglePerformance :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
togglePerformance fixture probe ordinary launches = do
  launchStart <- getMonotonicTimeNSec
  owner <- open fixture probe
  launchEnd <- getMonotonicTimeNSec
  announce ("SCRATCHPAD_LAUNCH_TO_MAP_AND_INPUT_NS " <> show (launchEnd - launchStart))
  root <- maybe (failIntegration "missing private runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
  let path = root </> "seki" </> "ipc.sock"
      subscribe = do
        client <- socket AF_UNIX Stream defaultProtocol
        connect client (SockAddrUnix path)
        sendAll client "{\"version\":1,\"id\":\"load\",\"op\":\"subscribe\"}\n"
        count <- newIORef (0 :: Int)
        thread <-
          forkIO $
            let loop = do
                  bytes <- recv client 65536
                  if BS.null bytes then pure () else modifyIORef' count (+ BS.count 10 bytes) >> loop
             in loop
        pure (client, thread, count)
      closeSubscriber (client, thread, _) = killThread thread >> close client
  bracket (replicateM 32 subscribe) (mapM_ closeSubscriber) $ \subscribers -> do
    -- Separate untimed warmup and initial publication.
    forM_ [1 .. 20 :: Int] $ \n -> do
      void (apply fixture (toggleScratchpad "term"))
      focus fixture probe (if odd n then Just ordinary else Nothing)
    input <- readRiverInputSnapshot (mSession fixture)
    announce ("SCRATCHPAD_PRODUCTION_INPUT " <> show input)
    previous <- newIORef (0 :: Integer)
    profileMode <- (== Just "1") <$> lookupEnv "SEKI_SCRATCHPAD_PROFILE"
    forM_ [1 .. if profileMode then 1 else 5 :: Int] $ \trial -> do
      forM_ [1 .. if profileMode then 100 else 1000 :: Int] $ \n -> do
        let source = (trial - 1) * 1000 + n
        started <- getMonotonicTimeNSec
        revision <- apply fixture (toggleScratchpad "term")
        completed <- timeout assertionTimeoutMicroseconds (focus fixture probe (if odd n then Just ordinary else Nothing))
        ended <- getMonotonicTimeNSec
        assertEqual "native toggle endpoint completed" (Just ()) completed
        prior <- readIORef previous
        assertEqual "monotonic applied revision" True (revision > prior)
        writeIORef previous revision
        slot <- stateOf fixture
        assertEqual "identity survives every observed toggle" (Just owner) (case slot of Just (ShownPad w _) -> Just w; Just (HiddenPad w _) -> Just w; _ -> Nothing)
        announce ("SCRATCHPAD_TOGGLE_NS " <> show trial <> " " <> show source <> " " <> show revision <> " " <> show (ended - started))
    counts <- mapM (\(_, _, count) -> readIORef count) subscribers
    assertEqual "all32 subscribers received production snapshots" True (all (> (if profileMode then 20 else 1000)) counts)
    announce ("SCRATCHPAD_SUBSCRIBER_LINES " <> show counts)
  readIORef launches >>= assertEqual "existing toggles never relaunch" 1
  saveCapture fixture "performance-final-content"

timeoutRecovery :: PadFixture -> Probe -> ProbeWindow -> IORef Int -> IO ()
timeoutRecovery fixture probe _ launches = do
  void (apply fixture (toggleScratchpad "term"))
  void (waitUntil "real session timer reports latched timeout" 12000000 (stateOf fixture) (\s -> case s of Just LaunchTimedOut{} -> True; _ -> False))
  forM_ [1 .. 20 :: Int] $ \_ -> void (apply fixture (toggleScratchpad "term"))
  readIORef launches >>= assertEqual "timeout never relaunches automatically" 1
  cli <- getEnv "SEKICTL_EXECUTABLE"
  response <- runProcessCapture (mEnvironment fixture) cli ["scratchpad-reset", "term"]
  announce ("SCRATCHPAD_RESET_APPLIED " <> show response)
  stateOf fixture >>= assertEqual "reset clears unresolved latch" Nothing
  void (apply fixture (toggleScratchpad "term"))
  readIORef launches >>= assertEqual "manual reset deliberately permits new attempt" 2
  _ <- openWindowWith probe (windowSpec Red){specAppId = CustomAppId "seki-scratchpad"}
  void (waitShown fixture)
  saveCapture fixture "timeout-reset-late-match"
