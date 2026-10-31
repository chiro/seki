{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecodeStrict', toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Run.IPC (ipcPath)
import Seki.Run.IPCSchema (Operation (Subscribe), Request (..), cliUsage, encodeLine, maxLineBytes, parseCLI)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  if args == ["--help"] then putStrLn cliUsage >> exitSuccess else pure ()
  operation <- either (\message -> hPutStrLn stderr message >> exitWith (ExitFailure 64)) pure (parseCLI args)
  let subscription = operation == Subscribe
  line <-
    either
      (\message -> hPutStrLn stderr message >> exitWith (ExitFailure 64))
      pure
      (encodeLine (toJSON (Request "sekictl" operation)))
  let attempt = do
        path <- ipcPath
        bracket (socket AF_UNIX Stream defaultProtocol) close $ \client -> do
          connect client (SockAddrUnix path)
          sendAll client line
          consume client subscription BS.empty
      run =
        try @IOException attempt >>= \case
          Right () | not subscription -> pure ()
          outcome -> do
            case outcome of
              Left e -> hPutStrLn stderr ("sekictl: " <> show e)
              Right () -> hPutStrLn stderr "sekictl: subscription disconnected"
            if subscription then threadDelay 1000000 >> run else exitWith (ExitFailure 69)
  run

consume :: Socket -> Bool -> BS.ByteString -> IO ()
consume client subscription pending = case BS.elemIndex 10 pending of
  Just n -> do
    let (line, rest) = BS.splitAt n pending
    unless (n <= maxLineBytes) (fail "oversized IPC response")
    case eitherDecodeStrict' line of
      Left message -> fail ("invalid IPC response: " <> message)
      Right (Object value)
        | KeyMap.lookup "version" value /= Just (Number 1) || KeyMap.lookup "id" value /= Just (String "sekictl") -> fail "invalid IPC response version or request ID"
        | KeyMap.member "error" value -> do
            Char8.hPutStrLn stderr line
            exitWith (ExitFailure 1)
        | Just Object{} <- KeyMap.lookup "result" value -> do
            Char8.putStrLn line
            if subscription then consume client True (BS.drop 1 rest) else pure ()
        | otherwise -> fail "missing IPC response result"
      Right _ -> fail "IPC response must be an object"
  Nothing
    | BS.length pending > maxLineBytes -> fail "oversized IPC response"
    | otherwise -> do
        chunk <- recv client (min 4096 (maxLineBytes + 1 - BS.length pending))
        unless (BS.null chunk) (consume client subscription (pending <> chunk))
        if BS.null chunk && not subscription then fail "disconnected before reply; command application is unknown" else pure ()
