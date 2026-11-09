{-# LANGUAGE OverloadedStrings #-}

module Seki.Latency.Commands (
  Commands,
  Receipt (..),
  Published (..),
  withCommands,
  command,
  awaitPublished,
) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (bracket, displayException)
import Data.Aeson (toJSON)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Seki.Integration.Harness (appliedRevision, bufferedLine)
import Seki.Latency.Snapshot (BuildReport, decodeSnapshot)
import Seki.Run.IPCSchema (Operation (Subscribe), Request (..), encodeLine)
import System.Timeout (timeout)

data Published = Published
  { publishedAt :: Word64
  , publishedRevision :: Integer
  , publishedFocus :: Text
  , publishedBuild :: Maybe BuildReport
  }
  deriving (Eq, Show)

data Receipt = Receipt
  { sentAt :: Word64
  , receiptRevision :: Integer
  }
  deriving (Eq, Show)

data Commands = Commands Socket (IO BS.ByteString) (Chan (Either String Published))

withCommands :: FilePath -> (Commands -> IO a) -> IO a
withCommands path body =
  bracket (connectTo path) close $ \commander ->
    bracket (connectTo path) close $ \subscriber -> do
      sendLine subscriber Subscribe
      readReply <- bufferedLine commander
      readSnapshot <- bufferedLine subscriber
      published <- newChan
      let loop = do
            line <- readSnapshot
            received <- getMonotonicTimeNSec
            writeChan published (fmap (\(revision, focus, build) -> Published received revision focus build) (decodeSnapshot line))
            loop
      bracket
        (forkFinally loop (either (writeChan published . Left . displayException) pure))
        killThread
        (\_ -> body (Commands commander readReply published))

connectTo :: FilePath -> IO Socket
connectTo path = do
  client <- socket AF_UNIX Stream defaultProtocol
  connect client (SockAddrUnix path)
  pure client

sendLine :: Socket -> Operation -> IO ()
sendLine client operation = either fail (sendAll client) (encodeLine (toJSON (Request "latency" operation)))

command :: Commands -> Operation -> IO Receipt
command (Commands client readReply _) operation = do
  started <- getMonotonicTimeNSec
  sendLine client operation
  Receipt started <$> (readReply >>= appliedRevision)

awaitPublished :: Commands -> Int -> (Published -> Bool) -> IO Published
awaitPublished (Commands _ _ published) limit wanted = loop
 where
  loop = do
    next <- timeout limit (readChan published) >>= maybe (fail "timed out waiting for IPC publication") pure >>= either fail pure
    if wanted next then pure next else loop
