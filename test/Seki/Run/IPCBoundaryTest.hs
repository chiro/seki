{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.IPCBoundaryTest (tests) where

import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (atomically, takeTMVar, tryTakeTMVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (replicateM, replicateM_, void)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Internal.Config (def)
import Seki.Run.Config (policyConfigOf)
import Seki.Run.External qualified as External
import Seki.Run.IPC (withIPC, withIPCAt)
import Seki.Run.Inventory (emptyRiverInventory)
import Seki.Run.ManageWake
import Seki.Run.Policy (ExternalInput (..), Policy, newPolicy, policyIngress, policyRuntimeHandlers)
import Seki.Run.Runtime (RiverInputSnapshot (..), RiverRuntimeHandlers (..), RiverRuntimeSnapshot (..))
import System.Directory (doesPathExist, getSymbolicLinkTarget, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Directory qualified as Posix
import System.Posix.Files (createSymbolicLink, fileID, getSymbolicLinkStatus, setFileMode)
import System.Process (createProcess, getProcessExitCode, proc, terminateProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "IPC endpoint ownership"
    [ testCase "refuse foreign regular files and symlinks" $ temporary $ \path -> do
        writeFile path "foreign"
        refuses path
        readFile path >>= assertEqual "foreign contents preserved" "foreign"
        removeFile path
        createSymbolicLink "target" path
        refuses path
        getSymbolicLinkTarget path >>= assertEqual "symlink preserved" "target"
    , testCase "refuse live foreign socket and preserve its inode" $ temporary $ \path ->
        bracket (socket AF_UNIX Stream defaultProtocol) close $ \peer -> do
          bind peer (SockAddrUnix path)
          listen peer 4
          before <- getSymbolicLinkStatus path
          refuses path
          after <- getSymbolicLinkStatus path
          fileID before @?= fileID after
    , testCase "replace stale owned socket and clean own endpoint" $ temporary $ \path -> do
        bracket (socket AF_UNIX Stream defaultProtocol) close $ \stale -> bind stale (SockAddrUnix path)
        policy <- fresh
        withIPCAt path policy (doesPathExist path >>= assertBool "bound endpoint exists")
        doesPathExist path >>= assertEqual "own endpoint removed" False
    , testCase "unsafe directory modes and read-only directory refuse startup" $
        withSystemTempDirectory "seki-ipc-modes" $ \root -> do
          let dir = root </> "seki"
              path = dir </> "ipc.sock"
          Posix.createDirectory dir 0o755
          refuses path
          setFileMode dir 0o500
          refuses path `finally` setFileMode dir 0o700
    , testCase "session cancellation closes connected client sockets" $ temporary $ \path -> do
        policy <- fresh
        ready <- newEmptyMVar
        hold <- newEmptyMVar
        joined <- newEmptyMVar
        owner <- forkFinally (withIPCAt path policy (putMVar ready () >> takeMVar hold)) (putMVar joined)
        let stop = do
              killThread owner
              timeout 1000000 (readMVar joined) >>= maybe (assertFailure "server cancellation did not finish") (const (pure ()))
            open = do
              client <- socket AF_UNIX Stream defaultProtocol
              connect client (SockAddrUnix path)
              sendAll client "{\"version\":1,\"id\":\"cancel\",\"op\":\"subscribe\"}\n"
              pure client
        ( do
            timeout 1000000 (takeMVar ready) >>= assertEqual "server ready" (Just ())
            bracket (replicateM 32 open) (mapM_ close) $ \clients -> do
              stop
              mapM_
                ( \client -> do
                    ended <- timeout 1000000 (try @IOException (recv client 1))
                    assertBool "peer closed without relying on GC" (case ended of Just (Left _) -> True; Just (Right bytes) -> BS.null bytes; Nothing -> False)
                )
                clients
          )
          `finally` stop
    , testCase "optional endpoint failure runs the body once and logs" $ temporary $ \path -> do
        writeFile path "foreign"
        policy <- fresh
        logs <- newIORef []
        calls <- newIORef (0 :: Int)
        mapM_
          ( \runtime ->
              withRuntime runtime $
                withIPC (\message -> modifyIORef' logs (<> [message])) policy (modifyIORef' calls (+ 1))
          )
          [Just (takeDirectory (takeDirectory path)), Just "relative", Nothing]
        readIORef calls >>= assertEqual "WM body still runs" 3
        readIORef logs >>= assertBool "setup failure is diagnosed" . any (\message -> take 15 message == "IPC unavailable")
        readFile path >>= assertEqual "foreign endpoint untouched" "foreign"
    , testCase "optional IPC never retries an exception from the WM body" $ temporary $ \path -> do
        policy <- fresh
        calls <- newIORef (0 :: Int)
        result <-
          withRuntime (Just (takeDirectory (takeDirectory path))) $
            try @IOException (withIPC (const (pure ())) policy (modifyIORef' calls (+ 1) >> ioError (userError "WM body failed")))
        assertBool "body exception propagates" (either (const True) (const False) result)
        readIORef calls >>= assertEqual "body ran only once" 1
    , testCase "a timed-out receipt leaves later commands available" $ temporary $ \path -> do
        policy <- fresh
        (clearWake, awaitWake) <- wakeOf policy
        withIPCAt path policy $ bracket (openClient path) close $ \client -> do
          clearWake
          sendAll client "{\"version\":1,\"id\":\"held\",\"op\":\"layout\",\"action\":\"next\"}\n"
          awaitWake >>= assertEqual "first command admitted" (Just ())
          reply <- lineWithin 8000000 client
          assertBool "no false failure claim for potentially applied command" ("application is unknown" `BS.isInfixOf` reply)
          let manage = prepareRiverManagePlan (policyRuntimeHandlers policy) (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) [])
          _ <- manage
          clearWake
          sendAll client "{\"version\":1,\"id\":\"next\",\"op\":\"layout\",\"action\":\"next\"}\n"
          awaitWake >>= assertEqual "later command admitted after timeout" (Just ())
          _ <- manage
          next <- line client
          assertBool "later command receives its applied revision" ("\"result\"" `BS.isInfixOf` next && "\"revision\"" `BS.isInfixOf` next)
          bracket (openClient path) close $ \other -> do
            clearWake
            sendAll other "{\"version\":1,\"id\":\"other\",\"op\":\"layout\",\"action\":\"next\"}\n"
            awaitWake >>= assertEqual "another client remains available" (Just ())
            _ <- manage
            line other >>= assertBool "other client receives its applied revision" . BS.isInfixOf "\"revision\""
    , testCase "IPC overload uses the sole 256-input ingress bound" $ temporary $ \path -> do
        policy <- fresh
        (clearWake, awaitWake) <- wakeOf policy
        let producer = policyIngress policy
            manage = prepareRiverManagePlan (policyRuntimeHandlers policy) (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) [])
        replicateM 256 (External.submitExternal producer (ExternalInput (pure ()) Nothing)) >>= assertEqual "256 inputs accepted" (replicate 256 External.Accepted)
        awaitWake >>= assertEqual "admission woke the owner" (Just ())
        withIPCAt path policy $ bracket (openClient path) close $ \client -> do
          let request = "{\"version\":1,\"id\":\"bound\",\"op\":\"layout\",\"action\":\"next\"}\n"
          sendAll client request
          line client >>= assertBool "real ingress full maps to overloaded" . BS.isInfixOf "overloaded"
          _ <- manage
          External.submitExternal producer (ExternalInput (pure ()) Nothing) >>= assertEqual "one batch restores admission" External.Accepted
          replicateM_ 8 manage
          clearWake
          sendAll client request
          awaitWake >>= assertEqual "IPC command admitted after drains" (Just ())
          _ <- manage
          line client >>= assertBool "capacity returns after bounded drains" . BS.isInfixOf "\"revision\""
    , testCase "disconnect behind a blocked runtime wake leaves later clients available" $ temporary $ \path -> do
        policy <- fresh
        entered <- newEmptyMVar
        release <- newEmptyMVar
        handle <- maybe (assertFailure "missing ingress") pure (riverExternalIngress (policyRuntimeHandlers policy))
        bracket
          (newManageWake (External.ingressWake handle) (putMVar entered () >> readMVar release >> pure (Right ())) (assertFailure . show))
          abortManageWake
          ( \wake -> do
              startManageWake wake
              withIPCAt path policy $ do
                bracket (openClient path) close $ \client -> do
                  sendAll client "{\"version\":1,\"id\":\"gone\",\"op\":\"layout\",\"action\":\"next\"}\n"
                  timeout 1000000 (takeMVar entered) >>= assertEqual "admission reached the blocked runtime sender" (Just ())
                putMVar release ()
                let manage = prepareRiverManagePlan (policyRuntimeHandlers policy) (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) [])
                _ <- manage
                bracket (openClient path) close $ \client -> do
                  sendAll client "{\"version\":1,\"id\":\"later\",\"op\":\"layout\",\"action\":\"next\"}\n"
                  timeout 1000000 (takeMVar entered) >>= assertEqual "later client admitted" (Just ())
                  _ <- manage
                  line client >>= assertBool "later client receives application receipt" . BS.isInfixOf "\"revision\""
          )
    , testCase "semantic request error preserves the next request" $ temporary $ \path -> do
        policy <- fresh
        withIPCAt path policy $ do
          _ <- prepareRiverManagePlan (policyRuntimeHandlers policy) (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) [])
          bracket (openClient path) close $ \client -> do
            sendAll client "{\"version\":1,\"id\":\"bad\",\"op\":\"layout\",\"action\":\"bogus\"}\n{\"version\":1,\"id\":\"next\",\"op\":\"snapshot\"}\n"
            first <- line client
            assertBool "invalid action has an error" ("invalid_request" `BS.isInfixOf` first)
            second <- line client
            assertBool "same connection returns snapshot" ("snapshot" `BS.isInfixOf` second)
    , testCase "subscriber input is not mistaken for EOF" $ temporary $ \path -> do
        policy <- fresh
        withIPCAt path policy $ do
          let manage = prepareRiverManagePlan (policyRuntimeHandlers policy) (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) [])
          _ <- manage
          bracket (openClient path) close $ \client -> do
            sendAll client "{\"version\":1,\"id\":\"sub\",\"op\":\"subscribe\"}\n"
            _ <- line client
            sendAll client "\nkeepalive\n"
            threadDelay 10000
            _ <- manage
            next <- line client
            assertBool "subscription still receives a snapshot" ("snapshot" `BS.isInfixOf` next)
    , testCase "child processes cannot retain the server guard" $ temporary $ \path -> do
        policy <- fresh
        bracket
          ( withIPCAt path policy $ do
              (_, _, _, child) <- createProcess (proc "sleep" ["10"])
              pure child
          )
          (\child -> terminateProcess child >> reap child)
          ( \child -> do
              getProcessExitCode child >>= assertEqual "child still running" Nothing
              next <- fresh
              withIPCAt path next (pure ())
          )
    ]
 where
  fresh = newPolicy (policyConfigOf def) >>= either (assertFailure . show) pure
  temporary body = withSystemTempDirectory "seki-ipc-boundary" $ \root -> do
    let dir = root </> "seki"
    Posix.createDirectory dir 0o700
    body (dir </> "ipc.sock")
  refuses path = do
    policy <- fresh
    result <- try @IOException (withIPCAt path policy (pure ()))
    assertBool "startup refused" (either (const True) (const False) result)
  reap child = getProcessExitCode child >>= maybe (threadDelay 1000 >> reap child) (const (pure ()))

openClient :: FilePath -> IO Socket
openClient path = do
  client <- socket AF_UNIX Stream defaultProtocol
  connect client (SockAddrUnix path)
  pure client

line :: Socket -> IO BS.ByteString
line = lineWithin 2000000

lineWithin :: Int -> Socket -> IO BS.ByteString
lineWithin micros client = do
  result <- timeout micros (go BS.empty)
  maybe (assertFailure "IPC response timeout" >> pure BS.empty) pure result
 where
  go bytes = do
    chunk <- recv client 1
    if BS.null chunk || chunk == "\n" then pure bytes else go (bytes <> chunk)

withRuntime :: Maybe FilePath -> IO a -> IO a
withRuntime value action = bracket (lookupEnv "XDG_RUNTIME_DIR") restore $ \_ -> restore value >> action
 where
  restore = maybe (unsetEnv "XDG_RUNTIME_DIR") (setEnv "XDG_RUNTIME_DIR")

wakeOf :: Policy String l -> IO (IO (), IO (Maybe ()))
wakeOf policy = do
  let slot = External.ingressWake (External.ingressHandle (policyIngress policy))
  pure (void (atomically (tryTakeTMVar slot)), timeout 1000000 (atomically (takeTMVar slot)))
