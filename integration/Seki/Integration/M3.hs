{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.M3 (m3Scenarios, ipcScenarios, recompileScenarios) where

import Control.Concurrent (forkFinally, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forM_, replicateM, void)
import Data.Aeson (eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (threadStatus)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Fixture.LayerSurface qualified as Layer
import Seki.Fixture.Probe (Color (..), KeyboardSnapshot (..), keyboardSnapshot, openWindow, outputNames, setTitle, withProbe)
import Seki.Integration.Fixture
import Seki.Integration.Harness
import Seki.Integration.IPCPerf (ipcPerfScenarios)
import Seki.Integration.Waybar (waybarScenarios)
import Seki.Internal.Config (def)
import Seki.Internal.Core.Action (restart, spawn, windows)
import Seki.Internal.Core.Layout (description)
import Seki.Internal.Core.StackSet qualified as SS
import Seki.Internal.Core.State (CoreState (..), Workspaces (..))
import Seki.Run.Config (policyConfigOf)
import Seki.Run.ConfigBuild (BuildContext (..), Resources (..))
import Seki.Run.External
import Seki.Run.IPC (withIPCAt)
import Seki.Run.Inventory (riverInventoryOutputs)
import Seki.Run.Policy
import Seki.Run.Runtime
import Seki.Wire.Connection.ObjectTable (defaultObjectLimit)
import Seki.Wire.Transport (ConnectionTarget (..))
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Process (proc)
import System.Timeout (timeout)

m3Scenarios :: [(String, IO ())]
m3Scenarios = [("m3-external-wake", externalWake), ("m3-external-session", sessionIsolation)]

externalWake :: IO ()
externalWake = do
  applied <- newIORef []
  manageHook <- newIORef Nothing
  renderHook <- newIORef Nothing
  afterDrain <- newIORef Nothing
  let once ref = atomicModifyIORef' ref (Nothing,) >>= sequence_
      decorate handlers =
        handlers
          { prepareRiverManagePlan = \snapshot -> do
              once manageHook
              plan <- prepareRiverManagePlan handlers snapshot
              once afterDrain
              pure plan
          , prepareRiverRenderPlan = \snapshot -> once renderHook >> prepareRiverRenderPlan handlers snapshot
          }
      config = (policyConfigOf def){policyLaunch = \marker -> modifyIORef' applied (<> [marker])}
  withFixtureUsing decorate config "m3-external-wake" 1 $ \fixture probe -> do
    _ <- openWindow probe Red
    let producer = policyIngress (mPolicy fixture)
        submit marker = submitExternal producer (ExternalInput (spawn marker) Nothing) >>= assertEqual "accepted marker" Accepted
        expect markers = void (waitUntil "accepted commands applied in FIFO order" assertionTimeoutMicroseconds (readIORef applied) (== markers))
        inProducer action = do
          done <- newEmptyMVar
          void (forkIO (action >> putMVar done ()))
          takeMVar done
    idleStatus <- waitUntil "dispatcher sleeping on Wayland read" assertionTimeoutMicroseconds (show <$> threadStatus (mDispatcher fixture)) (== "ThreadBlocked BlockedOnMVar")
    announce ("M3_IDLE_DISPATCHER " <> idleStatus)
    forM_ [1 .. 1000 :: Int] $ \n -> do
      threadDelay 1000
      let tag = if odd n then "2" else "1"
      started <- getMonotonicTimeNSec
      submitExternal producer (ExternalInput (windows (SS.view tag)) Nothing) >>= assertEqual "idle accepted" Accepted
      let observe = do
            state <- readPolicyState (mPolicy fixture)
            case coreWorkspaces state of
              AttachedState ss _ | SS.currentTag ss == tag -> getMonotonicTimeNSec
              _ -> threadDelay 100 >> observe
      ended <- observe
      assertEqual "idle completion below 50ms ceiling" True (ended - started < 50000000)
      announce ("M3_IDLE_NS " <> show (ended - started))
    saveCapture fixture "external-wake"
    atomicModifyIORef' manageHook (const (Just (inProducer (submit "during-manage")), ()))
    submit "trigger-manage"
    expect ["trigger-manage", "during-manage"]
    saveCapture fixture "manage-queue"
    atomicModifyIORef' renderHook (const (Just (inProducer (submit "during-render")), ()))
    submit "trigger-render"
    expect ["trigger-manage", "during-manage", "trigger-render", "during-render"]
    saveCapture fixture "render-queue"
    let markers = map show [1 .. 255 :: Int]
    atomicModifyIORef'
      manageHook
      ( const
          ( Just
              ( inProducer
                  ( do
                      forM_ markers submit
                      submitExternal producer (ExternalInput (spawn "overflow") Nothing) >>= assertEqual "explicit overload" Full
                  )
              )
          , ()
          )
      )
    submit "trigger-burst"
    expect (["trigger-manage", "during-manage", "trigger-render", "during-render", "trigger-burst"] <> markers)
    saveCapture fixture "queue-limit"
    saveCapture fixture "burst"

    atomicModifyIORef' afterDrain (const (Just (inProducer (submit "final-drain")), ()))
    submit "trigger-drain"
    expect (["trigger-manage", "during-manage", "trigger-render", "during-render", "trigger-burst"] <> markers <> ["trigger-drain", "final-drain"])
    saveCapture fixture "drain-race"
    forM_ (mOutputs fixture) $ \output -> void (runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", Text.unpack output, "--off"])
    void (waitUntil "no outputs" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (null . riverInventoryOutputs))
    submitExternal producer (ExternalInput (spawn "headless") Nothing) >>= assertEqual "headless accepted" Accepted
    void (waitUntil "headless completion" assertionTimeoutMicroseconds (readIORef applied) (elem "headless"))
    forM_ (mOutputs fixture) $ \output -> void (runProcessCapture (mEnvironment fixture) "wlr-randr" ["--output", Text.unpack output, "--on", "--custom-mode", "800x600"])
    void (waitUntil "output restored" assertionTimeoutMicroseconds (readRiverInventory (mSession fixture)) (not . null . riverInventoryOutputs))
    saveCapture fixture "headless"

sessionIsolation :: IO ()
sessionIsolation = withNestedRiver 1 $ \river -> do
  launched <- newIORef []
  let config = (policyConfigOf def){policyLaunch = \marker -> modifyIORef' launched (<> [marker]), policyCheckRestart = const (pure (Right ()))}
      open policy = openRiverSessionAt (UnixSocketPath (riverSocket river)) defaultObjectLimit (policyRuntimeHandlers policy) >>= requireRight "session"
      run session = do
        done <- newEmptyMVar
        void (forkIO (runRiverSession session >>= putMVar done))
        pure done
      attached policy = void (waitUntil "session attached" assertionTimeoutMicroseconds (readPolicyState policy) (\state -> case coreWorkspaces state of AttachedState{} -> True; _ -> False))
  first <- newPolicy config >>= requireRight "first policy"
  session <- open first
  done <- run session
  let old = policyIngress first
      accepted action = submitExternal old (ExternalInput action Nothing) >>= assertEqual "first admission" Accepted
  ( do
      attached first
      accepted (windows (SS.view "2"))
      void (waitUntil "workspace before restart" assertionTimeoutMicroseconds (readPolicyState first) (\state -> case coreWorkspaces state of AttachedState ss _ -> SS.currentTag ss == "2"; _ -> False))
      accepted restart
      forM_ [1 .. 64 :: Int] (\n -> accepted (spawn (show n)))
      requireRuntimeExit done >>= assertEqual "stop handshake with pending work" (Right RiverStopped)
      submitExternal old (ExternalInput (windows (SS.view "3")) Nothing) >>= assertEqual "stopped session rejects completion" IngressClosed
      readIORef launched >>= assertEqual "stop closes leftover batch" (map show [1 .. 31 :: Int])
      saved <- readRestoreState first
      second <- newPolicy config{policyRestore = Just saved} >>= requireRight "restored policy"
      next <- open second
      nextDone <- run next
      ( do
          attached second
          submitExternal old (ExternalInput (windows (SS.view "3")) Nothing) >>= assertEqual "old token after restore" IngressClosed
          readPolicyState second >>= \state -> case coreWorkspaces state of
            AttachedState ss _ -> assertEqual "restored workspace unchanged by old token" "2" (SS.currentTag ss)
            _ -> failIntegration "restore headless"
          submitExternal (policyIngress second) (ExternalInput (windows (SS.view "1")) Nothing) >>= assertEqual "fresh producer" Accepted
          void (waitUntil "new session progresses" assertionTimeoutMicroseconds (readPolicyState second) (\state -> case coreWorkspaces state of AttachedState ss _ -> SS.currentTag ss == "1"; _ -> False))
        )
        `finally` (stopManagedProcess (riverProcess river) >> waitForRuntimeExit nextDone)
    )
    `finally` (stopManagedProcess (riverProcess river) >> waitForRuntimeExit done)

ipcScenarios :: [(String, IO ())]
ipcScenarios = [("m3-ipc-boundary", ipcBoundary), ("m3-ipc-metadata", ipcMetadata), ("m3-ipc-state", ipcState), ("m3-ipc-reconnect", ipcReconnect), ("m3-ipc-limits", ipcLimits)] <> ipcPerfScenarios <> waybarScenarios

ipcBoundary :: IO ()
ipcBoundary = do
  launched <- newIORef []
  let config = (policyConfigOf def){policyLaunch = \cmd -> modifyIORef' launched (<> [cmd])}
  withFixture config "m3-ipc-boundary" 1 $ \fixture _ -> do
    root <- maybe (failIntegration "missing fixture runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
    let path = root </> "seki" </> "ipc.sock"
        open = do
          client <- socket AF_UNIX Stream defaultProtocol
          connect client (SockAddrUnix path)
          pure client
        request bytes = bracket open close $ \client -> sendAll client bytes >> ipcLine client
    withIPCAt path (mPolicy fixture) $ do
      submitExternal (policyIngress (mPolicy fixture)) (ExternalInput (pure ()) Nothing) >>= assertEqual "initial publication trigger" Accepted
      initial <- request "{\"version\":1,\"id\":\"a\",\"op\":\"snapshot\"}\n"
      assertEqual "initial snapshot" True ("\"workspaces\"" `BS.isInfixOf` initial)
      cliExecutable <- requireSetting "SEKICTL_EXECUTABLE"
      cli <- runProcessCapture (mEnvironment fixture) cliExecutable ["workspace", "3"]
      assertEqual "CLI applied receipt" True ("\"revision\"" `BS.isInfixOf` cli)
      void (waitUntil "CLI workspace applied" assertionTimeoutMicroseconds (readPolicyState (mPolicy fixture)) (\st -> case coreWorkspaces st of AttachedState ss _ -> SS.currentTag ss == "3"; _ -> False))
      saveCapture fixture "ipc-regression"
      second <- try @IOException (withIPCAt path (mPolicy fixture) (pure ()))
      assertEqual "second live server refused" True (either (const True) (const False) second)
      _ <- request "{\"version\":1,\"id\":\"a\",\"op\":\"snapshot\"}\n"
      saveCapture fixture "socket-owner"
      bracket open close $ \client -> do
        sendAll client "{\"version\":1,"
        sendAll client "\"id\":\"frag\",\"op\":\"workspace\",\"tag\":\"2\"}\n"
        response <- ipcLine client
        assertEqual "fragment accepted once" True ("\"revision\"" `BS.isInfixOf` response)
      bracket open close $ \client -> do
        sendAll client (BS.replicate 65537 32)
        ended <- timeout assertionTimeoutMicroseconds (recv client 1)
        assertEqual "oversized disconnect" (Just BS.empty) ended
      forM_ [BS.pack [255, 10], "{\"version\":9,\"id\":\"bad\",\"op\":\"snapshot\"}\n"] $ \bad -> do
        response <- request bad
        assertEqual "invalid request error" True ("\"error\"" `BS.isInfixOf` response)
      saveCapture fixture "framing"
      bracket open close $ \unread -> do
        sendAll unread "{\"version\":1,\"id\":\"slow\",\"op\":\"subscribe\"}\n"
        bracket open close $ \client -> do
          forM_ [1 .. 1000 :: Int] $ \n -> do
            let tag = if odd n then "1" else "2"
            sendAll client ("{\"version\":1,\"id\":\"load\",\"op\":\"workspace\",\"tag\":\"" <> tag <> "\"}\n")
            response <- ipcLine client
            assertEqual "every update acknowledged after application" True ("\"revision\"" `BS.isInfixOf` response)
        saveCapture fixture "slow-reader"
      bracket open close $ \client -> sendAll client "{\"version\":1,\"id\":\"gone\",\"op\":\"spawn\",\"command\":\"disconnect-marker\"}\n"
      void (waitUntil "disconnected client's admitted command applied" assertionTimeoutMicroseconds (readIORef launched) (elem "disconnect-marker"))
      response <- request "{\"version\":1,\"id\":\"healthy\",\"op\":\"workspace\",\"tag\":\"3\"}\n"
      assertEqual "disconnect preserves session ingress" True ("\"revision\"" `BS.isInfixOf` response)
      announce "M3_IPC_BOUNDARY all 1000 updates acknowledged; disconnect isolation and duplicate server passed"

ipcLine :: Socket -> IO BS.ByteString
ipcLine client = do
  result <- timeout assertionTimeoutMicroseconds (readLine BS.empty)
  maybe (failIntegration "IPC response timed out") pure result
 where
  readLine pending = do
    byte <- recv client 1
    if byte == "\n" || BS.null byte
      then pure pending
      else if BS.length pending >= 65536 then failIntegration "oversized IPC reply" else readLine (pending <> byte)

ipcMetadata :: IO ()
ipcMetadata = withFixture (policyConfigOf def) "m3-ipc-metadata" 1 $ \fixture probe -> do
  root <- maybe (failIntegration "missing runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
  let path = root </> "seki" </> "ipc.sock"
      open = do
        client <- socket AF_UNIX Stream defaultProtocol
        connect client (SockAddrUnix path)
        pure client
      title = "<b>&\"\n$(touch /tmp/seki-title-must-not-run)"
  withIPCAt path (mPolicy fixture) $ bracket open close $ \subscriber -> do
    sendAll subscriber "{\"version\":1,\"id\":\"sub\",\"op\":\"subscribe\"}\n"
    submitExternal (policyIngress (mPolicy fixture)) (ExternalInput (pure ()) Nothing) >>= assertEqual "publish initial" Accepted
    initial <- ipcLine subscriber
    (initialRevision, initialTitle) <- snapshotTitle initial
    assertEqual "initial subscription has no title" Nothing initialTitle
    announce ("M3_IPC_INITIAL " <> show initial)
    window <- openWindow probe Red
    setTitle probe window "first title"
    let waitTitle expected = do
          line <- ipcLine subscriber
          (revision, observed) <- snapshotTitle line
          if observed == Just expected then pure (revision, line) else waitTitle expected
    (mappedRevision, mapped) <- waitTitle "first title"
    assertEqual "mapped revision increases" True (mappedRevision > initialRevision)
    announce ("M3_IPC_MAPPED " <> show mapped)
    saveCapture fixture "subscribe"
    setTitle probe window title
    (titleRevision, changed) <- waitTitle title
    assertEqual "title-only revision increases" True (titleRevision > mappedRevision)
    (stillOldRevision, stillOldTitle) <- snapshotTitle mapped
    assertEqual "old immutable projection unchanged" (mappedRevision, Just "first title") (stillOldRevision, stillOldTitle)
    announce ("M3_IPC_TITLE_ONLY " <> show changed)
    saveCapture fixture "title"
    setTitle probe window (Text.replicate 3000 "x")
    (_, giant) <- waitTitle (Text.replicate 3000 "x")
    announce ("M3_IPC_GIANT_TITLE " <> show giant)

snapshotTitle :: BS.ByteString -> IO (Integer, Maybe Text.Text)
snapshotTitle bytes = do
  value <- requireRight "JSON snapshot" (eitherDecodeStrict' bytes)
  requireRight "snapshot fields" (parseEither parser value)
 where
  parser = withObject "envelope" $ \envelope -> do
    result <- envelope .: "result"
    withObject "result" (\o -> o .: "snapshot" >>= withObject "snapshot" (\snapshot -> (,) <$> snapshot .: "revision" <*> snapshot .: "focused_title")) result

ipcState :: IO ()
ipcState = withFixture (policyConfigOf def) "m3-ipc-state" 2 $ \fixture probe -> do
  root <- maybe (failIntegration "missing runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
  let path = root </> "seki" </> "ipc.sock"
      open = do
        client <- socket AF_UNIX Stream defaultProtocol
        connect client (SockAddrUnix path)
        pure client
      request bytes = bracket open close $ \client -> sendAll client bytes >> ipcLine client
      snapshot = request "{\"version\":1,\"id\":\"state\",\"op\":\"snapshot\"}\n"
  withIPCAt path (mPolicy fixture) $ do
    _ <- openWindow probe Red
    submitExternal (policyIngress (mPolicy fixture)) (ExternalInput (pure ()) Nothing) >>= assertEqual "publish" Accepted
    initial <- snapshot
    (names, flags, available) <- snapshotFlags initial
    assertEqual "two connector names" [Just "HEADLESS-1", Just "HEADLESS-2"] (sort names)
    assertEqual "two workspaces visible" 2 (length (filter (\(_, shown, _) -> shown) flags))
    assertEqual "one intended workspace focus" 1 (length (filter (\(_, _, focused) -> focused) flags))
    assertEqual "window keyboard available" True available
    cliExecutable <- requireSetting "SEKICTL_EXECUTABLE"
    _ <- runProcessCapture (mEnvironment fixture) cliExecutable ["workspace", "3"]
    _ <- openWindow probe Green
    changed <- snapshot
    (_, flagsAfter, _) <- snapshotFlags changed
    assertEqual "requested workspace focused" [("3", True, True)] (filter (\(_, _, focused) -> focused) flagsAfter)
    announce ("M3_IPC_TWO_OUTPUTS " <> show changed)
    saveCapture fixture "multi-output"
    control <- Layer.newFixtureControl
    ready <- newEmptyMVar
    stopped <- newEmptyMVar
    thread <- forkFinally (Layer.runLayerSurfaceFixture control (Layer.FixtureConfig 32 Layer.KeyboardInteractivityExclusive) (UnixSocketPath (mSocket fixture)) (putMVar ready)) (putMVar stopped)
    let stopLayer = Layer.requestStop control >> waitForMVar "layer stopped" stopped >>= requireRight "layer thread" >>= requireRight "layer protocol" >>= assertEqual "layer clean stop" Layer.StoppedByRequest
    ( do
        _ <- waitForMVar "exclusive layer ready" ready
        _ <- waitUntil "exclusive layer owns keyboard" assertionTimeoutMicroseconds (readRiverSeatBindings (mSession fixture)) (any ((== RiverLayerFocusExclusive) . riverSeatLayerFocus))
        layerSnapshot <- waitUntil "IPC reflects layer keyboard" assertionTimeoutMicroseconds (snapshot >>= snapshotFlags) (\(_, _, keyboard) -> not keyboard)
        let (_, layerFlags, layerKeyboard) = layerSnapshot
        assertEqual "workspace intent preserved under layer focus" [("3", True, True)] (filter (\(_, _, focused) -> focused) layerFlags)
        assertEqual "keyboard is unavailable under exclusive layer" False layerKeyboard
        saveCapture fixture "layer-focus"
        announce ("M3_IPC_EXCLUSIVE_LAYER " <> show layerSnapshot)
      )
      `finally` (stopLayer `finally` killThread thread)
    bracket
      ( replicateM 32 $ do
          client <- open
          sendAll client "{\"version\":1,\"id\":\"capacity\",\"op\":\"subscribe\"}\n"
          initialLine <- ipcLine client
          assertEqual "connection admitted" True ("\"snapshot\"" `BS.isInfixOf` initialLine)
          pure client
      )
      (mapM_ close)
      $ \_ -> bracket open close $ \extra -> do
        response <- timeout assertionTimeoutMicroseconds (try @IOException (recv extra 1))
        assertEqual "33rd connection closed" True (case response of Just (Left _) -> True; Just (Right bytes) -> BS.null bytes; Nothing -> False)
    _ <- waitUntil "subscriber slots released on EOF without new state" assertionTimeoutMicroseconds (try @IOException snapshot) (either (const False) (BS.isInfixOf "\"snapshot\""))
    announce "M3_IPC_CAPACITY 32 admitted; 33rd closed; idle EOF releases slots"

snapshotFlags :: BS.ByteString -> IO ([Maybe Text.Text], [(Text.Text, Bool, Bool)], Bool)
snapshotFlags bytes = do
  value <- requireRight "snapshot JSON" (eitherDecodeStrict' bytes)
  requireRight "state fields" (parseEither parser value)
 where
  parser = withObject "envelope" $ \envelope -> do
    result <- envelope .: "result"
    withObject
      "result"
      ( \o ->
          o .: "snapshot"
            >>= withObject
              "snapshot"
              ( \s -> do
                  outputs <- s .: "outputs"
                  names <- mapM (withObject "output" (.: "name")) outputs
                  workspaces <- s .: "workspaces"
                  flags <- mapM (withObject "workspace" (\w -> (,,) <$> w .: "tag" <*> w .: "visible" <*> w .: "focused")) workspaces
                  available <- s .: "keyboard_available"
                  pure (names, flags, available)
              )
      )
      result

ipcReconnect :: IO ()
ipcReconnect = withNestedRiver 1 $ \nested -> do
  executable <- requireSetting "SEKI_EXECUTABLE"
  cli <- requireSetting "SEKICTL_EXECUTABLE"
  let environment = riverEnvironment nested
      runtime = riverRuntimeDirectory nested
      marker = runtime </> "spawn-count"
      logFile = runtime </> "subscription.jsonl"
      readLines path = doesFileExist path >>= \exists -> if exists then BS.split 10 <$> BS.readFile path else pure []
      key = modKeys environment ["q"]
  bracket (startManagedProcess "seki" environment (proc executable [])) stopManagedProcess $ \manager -> do
    [(_, output)] <- waitUntil "one output" assertionTimeoutMicroseconds (outputNames (riverSocket nested)) ((== 1) . length)
    withKeyboard environment . withProbe (riverSocket nested) $ \probe -> do
      _ <- openWindow probe Red
      withSekictlSubscription environment cli logFile $ \received -> do
        let sessions = received >>= mapM snapshotSession
        first <- waitUntil "initial CLI subscription" assertionTimeoutMicroseconds sessions (not . null)
        _ <- runProcessCapture environment cli ["spawn", "printf 'once\n' >> '" <> marker <> "'"]
        _ <- waitUntil "spawn executed once" assertionTimeoutMicroseconds (readLines marker) ((== ["once"]) . filter (not . BS.null))
        pid <- managedProcessPid manager >>= maybe (failIntegration "manager pid missing") pure
        key
        _ <- waitUntil "real exec restart" assertionTimeoutMicroseconds (BS.readFile ("/proc/" <> show pid <> "/cmdline")) (isInfixOf "--restore" . map (toEnum . fromEnum) . BS.unpack)
        after <- waitUntil "subscriber reconnects to a fresh session" (assertionTimeoutMicroseconds + 2000000) sessions ((>= 2) . length . nub)
        assertEqual "session identity changed" True (length (nub after) > length (nub first))
        readLines marker >>= assertEqual "spawn was not replayed" ["once", ""]
        announce ("M3_IPC_RECONNECT " <> show (nub after))
        artifacts <- lookupEnv "SEKI_INTEGRATION_ARTIFACT_DIR"
        forM_ artifacts $ \directory -> do
          BS.readFile logFile >>= BS.writeFile (directory </> "reconnect-subscription.jsonl")
          saveOutputCapture (riverSocket nested) output (directory </> "reconnect.png")

snapshotSession :: BS.ByteString -> IO Text.Text
snapshotSession bytes = do
  value <- requireRight "subscription JSON" (eitherDecodeStrict' bytes)
  requireRight "session identity" (parseEither parser value)
 where
  parser = withObject "envelope" $ \e -> e .: "result" >>= withObject "result" (\r -> r .: "snapshot" >>= withObject "snapshot" (.: "session"))

ipcLimits :: IO ()
ipcLimits = do
  cli <- requireSetting "SEKICTL_EXECUTABLE"
  cwd <- getCurrentDirectory
  _ <- runProcessCapture [] "python3" [cwd </> "integration/ipc-stream.py", cli]
  launched <- newIORef []
  let config =
        (policyConfigOf def)
          { policyTags = [show n <> replicate 1024 'x' | n <- [1 .. 128 :: Int]]
          , policyLaunch = \command -> modifyIORef' launched (<> [command])
          }
  withFixture config "m3-ipc-limits" 1 $ \fixture _ -> do
    root <- maybe (failIntegration "missing runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
    let path = root </> "seki" </> "ipc.sock"
        open = do
          client <- socket AF_UNIX Stream defaultProtocol
          connect client (SockAddrUnix path)
          pure client
        request bytes = bracket open close $ \client -> sendAll client bytes >> ipcLine client
    withIPCAt path (mPolicy fixture) $ do
      submitExternal (policyIngress (mPolicy fixture)) (ExternalInput (pure ()) Nothing) >>= assertEqual "publish oversized projection" Accepted
      tooLarge <- request "{\"version\":1,\"id\":\"large-id\",\"op\":\"snapshot\"}\n"
      assertEqual "oversized snapshot reported" True ("snapshot_too_large" `BS.isInfixOf` tooLarge)
      assertEqual "oversize error preserves request ID" True ("large-id" `BS.isInfixOf` tooLarge)
      bracket open close $ \subscriber -> do
        sendAll subscriber "{\"version\":1,\"id\":\"subscribe-id\",\"op\":\"subscribe\"}\n"
        initial <- ipcLine subscriber
        assertEqual "oversized subscription is explicit" True ("snapshot_too_large" `BS.isInfixOf` initial)
        applied <- request "{\"version\":1,\"id\":\"ack-id\",\"op\":\"spawn\",\"command\":\"large-snapshot-marker\"}\n"
        assertEqual "oversized projection cannot drop application reply" True ("\"revision\"" `BS.isInfixOf` applied && "ack-id" `BS.isInfixOf` applied)
        readIORef launched >>= assertEqual "spawn intent ran once" ["large-snapshot-marker"]
      announce "M3_IPC_LIMITS oversized snapshot/subscription explicit; independent application reply preserved"

recompileScenarios :: [(String, IO ())]
recompileScenarios = [("m3-recompile", recompileResponsive)]

recompileResponsive :: IO ()
recompileResponsive = withFixture (policyConfigOf def) "m3-recompile" 1 $ \fixture probe -> do
  root <- maybe (failIntegration "missing private runtime") pure (lookup "XDG_RUNTIME_DIR" (mEnvironment fixture))
  cli <- requireSetting "SEKICTL_EXECUTABLE"
  launcher <- requireSetting "SEKI_LAUNCHER_EXECUTABLE"
  let directory = root </> "build-proof"
      wrapper = directory </> "compiler"
      started = directory </> "started"
      release = directory </> "release"
      profile = directory </> "profile-sentinel"
      environment = mEnvironment fixture
      layoutDescription =
        readPolicyState (mPolicy fixture) >>= \st -> case coreWorkspaces st of
          AttachedState ss _ -> pure (description (SS.layout (SS.workspace (SS.current ss))))
          HeadlessState _ -> failIntegration "held-build fixture lost its output"
  createDirectory directory
  mapM_ (\name -> writeFile (directory </> name) "") ["Main.hs", "seki-user.cabal", "cabal.project"]
  writeFile profile "old target"
  writeFile wrapper ("#!/bin/sh\necho started >> '" <> started <> "'\nwhile [ ! -f '" <> release <> "' ]; do sleep 0.02; done\necho deliberate-compile-error\nexit 1\n")
  setFileMode wrapper 0o700
  configureBuild (mPolicy fixture) (BuildContext (Resources "/unused" "/unused" "/unused" wrapper directory) directory directory)
  withIPCAt (root </> "seki" </> "ipc.sock") (mPolicy fixture) $ do
    _ <- openWindow probe Red
    _ <- openWindow probe Green
    receipt <- runProcessCapture environment launcher ["--recompile"]
    assertEqual "launcher receives applied request receipt" True ("\"revision\"" `BS.isInfixOf` receipt)
    void (waitUntil "compiler is held" assertionTimeoutMicroseconds (doesFileExist started) id)
    forM_ [1 .. 8 :: Int] $ \_ -> do
      _ <- runProcessCapture environment cli ["recompile"]
      before <- keyboardOwner <$> keyboardSnapshot probe
      beforeLayout <- layoutDescription
      modKeys environment ["j", "space"]
      void (waitUntil "real focus changes while compiler held" assertionTimeoutMicroseconds (keyboardOwner <$> keyboardSnapshot probe) (/= before))
      void (waitUntil "real layout changes while compiler held" assertionTimeoutMicroseconds layoutDescription (/= beforeLayout))
    _ <- runProcessCapture environment cli ["workspace", "2"]
    void (waitUntil "workspace command during held build" assertionTimeoutMicroseconds (readPolicyState (mPolicy fixture)) (\st -> case coreWorkspaces st of AttachedState ss _ -> SS.currentTag ss == "2"; _ -> False))
    doesFileExist release >>= assertEqual "input completed before compiler release" False
    BS.readFile started >>= assertEqual "one active compiler under duplicate requests" "started\n"
    _ <- runProcessCapture environment cli ["workspace", "1"]
    saveCapture fixture "build-responsive"
    writeFile release ""
    void (waitUntil "queued rebuild also fails" assertionTimeoutMicroseconds (policyBuildStatus (mPolicy fixture)) (isInfixOf "failed" . show))
    BS.readFile started >>= assertEqual "only one queued compiler" "started\nstarted\n"
    readFile profile >>= assertEqual "failure preserved target" "old target"
    names <- listDirectory directory
    assertEqual "full logs retained" True (any (\name -> "build" `isPrefixOf` name && ".log" `isSuffixOf` name) names)
    _ <- runProcessCapture environment cli ["workspace", "3"]
    void (waitUntil "input and broker survive failed build" assertionTimeoutMicroseconds (readPolicyState (mPolicy fixture)) (\st -> case coreWorkspaces st of AttachedState ss _ -> SS.currentTag ss == "3"; _ -> False))
    saveCapture fixture "build-error"
