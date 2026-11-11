{-# LANGUAGE OverloadedStrings #-}

module Seki.Latency.Commands (
  Commands,
  Receipt (..),
  Published (..),
  withCommands,
  command,
  awaitPublished,
) where

import Control.Exception (bracket)
import Data.Aeson (toJSON)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Seki.Integration.Harness (BuildView, Snapshot (..), appliedRevision, bufferedLine, connectUnix, focusedTag, withSnapshotPump)
import Seki.Run.IPCSchema (Operation (Subscribe), Request (..), encodeLine)
import System.Timeout (timeout)

data Published = Published
  { publishedAt :: Word64
  , publishedRevision :: Integer
  , publishedFocus :: Text
  , publishedBuild :: Maybe BuildView
  }
  deriving (Eq, Show)

data Receipt = Receipt
  { sentAt :: Word64
  , receiptRevision :: Integer
  }
  deriving (Eq, Show)

data Commands = Commands Socket (IO BS.ByteString) (IO (Either String Published))

withCommands :: FilePath -> (Commands -> IO a) -> IO a
withCommands path body =
  bracket (connectUnix path) close $ \commander ->
    bracket (connectUnix path) close $ \subscriber -> do
      sendLine subscriber Subscribe
      readReply <- bufferedLine commander
      readSnapshot <- bufferedLine subscriber
      withSnapshotPump readSnapshot $ \arrivals ->
        body (Commands commander readReply ((>>= publication) <$> arrivals))

publication :: (Word64, Snapshot) -> Either String Published
publication (received, shot) = (\focus -> Published received (snapRevision shot) focus (snapBuild shot)) <$> focusedTag shot

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
    next <- timeout limit published >>= maybe (fail "timed out waiting for IPC publication") pure >>= either fail pure
    if wanted next then pure next else loop
