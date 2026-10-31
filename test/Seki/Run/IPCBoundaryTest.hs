{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.IPCBoundaryTest (tests) where

import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (replicateM)
import Data.ByteString qualified as BS
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Internal.Config (def)
import Seki.Run.Config (policyConfigOf)
import Seki.Run.IPC (withIPCAt)
import Seki.Run.Policy (newPolicy)
import System.Directory (doesPathExist, getSymbolicLinkTarget, removeFile)
import System.FilePath ((</>))
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
