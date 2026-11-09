{-# LANGUAGE OverloadedStrings #-}

module Seki.Latency.Commands (
  Commands,
  Receipt (..),
  Published (..),
  BuildReport (..),
  withCommands,
  command,
  awaitPublished,
) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (bracket, displayException)
import Data.Aeson (Value, eitherDecodeStrict', toJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Seki.Run.IPCSchema (Operation (Subscribe), Request (..), encodeLine)
import System.Timeout (timeout)

data BuildReport = BuildReport
  { reportedState :: Text
  , reportedGeneration :: Integer
  , reportedError :: Maybe Text
  }
  deriving (Eq, Show)

data Published = Published
  { publishedAt :: Word64
  , publishedRevision :: Integer
  , publishedFocus :: Text
  , publishedBuild :: Maybe BuildReport
  }
  deriving (Eq, Show)

data Receipt = Receipt
  { sentAt :: Word64
  , appliedRevision :: Integer
  }
  deriving (Eq, Show)

data Commands = Commands Socket (IO BS.ByteString) (Chan (Either String Published))

withCommands :: FilePath -> Int -> (Commands -> IO a) -> IO a
withCommands path limit body =
  bracket (connectTo path) close $ \commander ->
    bracket (connectTo path) close $ \subscriber -> do
      sendLine subscriber Subscribe
      readReply <- lineReader commander
      readSnapshot <- lineReader subscriber
      published <- newChan
      let loop = do
            line <- readSnapshot
            received <- getMonotonicTimeNSec
            writeChan published (fmap (\(revision, focus, build) -> Published received revision focus build) (decodeSnapshot line))
            loop
      bracket
        (forkFinally loop (either (writeChan published . Left . displayException) pure))
        killThread
        (\_ -> body (Commands commander (bounded limit "IPC receipt" readReply) published))

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
  line <- readReply
  revision <- either fail pure (eitherDecodeStrict' line >>= parseEither receipt)
  pure (Receipt started revision)
 where
  receipt = withObject "receipt" $ \o -> o .: "result" >>= withObject "result" (.: "revision")

awaitPublished :: Commands -> Int -> (Published -> Bool) -> IO Published
awaitPublished (Commands _ _ published) limit wanted = loop
 where
  loop = do
    next <- bounded limit "IPC publication" (readChan published) >>= either fail pure
    if wanted next then pure next else loop

decodeSnapshot :: BS.ByteString -> Either String (Integer, Text, Maybe BuildReport)
decodeSnapshot line = eitherDecodeStrict' line >>= parseEither envelope
 where
  envelope = withObject "envelope" $ \o -> o .: "result" >>= withObject "result" (\r -> r .: "snapshot" >>= snapshot)
  snapshot :: Value -> Parser (Integer, Text, Maybe BuildReport)
  snapshot = withObject "snapshot" $ \s -> do
    revision <- s .: "revision"
    workspaces <- s .: "workspaces" >>= mapM (withObject "workspace" (\w -> (,) <$> w .: "tag" <*> w .: "focused"))
    focus <- case [tag | (tag, True) <- workspaces] of
      [tag] -> pure tag
      _ -> fail "snapshot must focus one workspace"
    build <- s .:? "build" >>= traverse (withObject "build" (\b -> BuildReport <$> b .: "state" <*> b .: "generation" <*> b .:? "error"))
    pure (revision, focus, build)

lineReader :: Socket -> IO (IO BS.ByteString)
lineReader client = do
  pending <- newIORef BS.empty
  let readOne = do
        bytes <- readIORef pending
        case BS.elemIndex 10 bytes of
          Just n -> writeIORef pending (BS.drop (n + 1) bytes) >> pure (BS.take n bytes)
          Nothing -> do
            chunk <- recv client 65536
            if BS.null chunk then fail "IPC connection closed" else writeIORef pending (bytes <> chunk) >> readOne
  pure readOne

bounded :: Int -> String -> IO a -> IO a
bounded limit what action = timeout limit action >>= maybe (fail ("timed out waiting for " <> what)) pure
