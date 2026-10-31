{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.IPCSchema (
  Request (..),
  RequestError (..),
  Operation (..),
  LayoutOp (..),
  decodeRequest,
  parseCLI,
  cliUsage,
  encodeLine,
  maxLineBytes,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecodeStrict', encode, object, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text

data RequestError = RequestError {errorRequestId :: Text, errorMessage :: String} deriving (Eq, Show)

data Request = Request {requestId :: Text, requestOperation :: Operation} deriving (Eq, Show)
data Operation = Snapshot | Subscribe | Workspace Text | Spawn Text | Layout LayoutOp deriving (Eq, Show)
data LayoutOp = Next | Shrink | Expand | IncMaster | DecMaster deriving (Eq, Show)

maxLineBytes :: Int
maxLineBytes = 65536

instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> do
    version <- o .: "version" :: Parser Int
    if version /= 1 then fail "unsupported version" else pure ()
    ident <- o .: "id"
    if Text.length ident > 128 then fail "id exceeds 128 characters" else pure ()
    op <- o .: "op" :: Parser Text
    operation <- case op of
      "snapshot" -> pure Snapshot
      "subscribe" -> pure Subscribe
      "workspace" -> Workspace <$> o .: "tag"
      "spawn" -> do
        command <- o .: "command"
        if Text.null (Text.strip command) then fail "empty command" else pure (Spawn command)
      "layout" -> do
        action <- o .: "action" :: Parser Text
        Layout <$> case action of
          "next" -> pure Next
          "shrink" -> pure Shrink
          "expand" -> pure Expand
          "inc-master" -> pure IncMaster
          "dec-master" -> pure DecMaster
          _ -> fail "unknown layout action"
      _ -> fail "unknown operation"
    pure (Request ident operation)

decodeRequest :: BS.ByteString -> Either RequestError Request
decodeRequest bytes
  | BS.length bytes > maxLineBytes = Left (RequestError "" "line exceeds 65536 bytes")
  | otherwise = do
      value <- first (RequestError "") (eitherDecodeStrict' bytes)
      let ident = case value of
            Object fields | Just (String text) <- KeyMap.lookup "id" fields, Text.length text <= 128 -> text
            _ -> ""
      first (RequestError ident) (parseEither parseJSON value)

encodeLine :: Value -> Either String BS.ByteString
encodeLine value
  | BL.length bytes > fromIntegral maxLineBytes = Left "line exceeds 65536 bytes"
  | otherwise = Right (BL.toStrict bytes <> "\n")
 where
  bytes = BL.take (fromIntegral maxLineBytes + 1) (encode value)

cliUsage :: String
cliUsage = "usage: sekictl snapshot | subscribe | workspace TAG | spawn COMMAND | layout {next|shrink|expand|inc-master|dec-master}"

parseCLI :: [String] -> Either String Operation
parseCLI args = case args of
  ["snapshot"] -> Right Snapshot
  ["subscribe"] -> Right Subscribe
  ["workspace", tag] -> Right (Workspace (Text.pack tag))
  ["spawn", command] | not (Text.null (Text.strip (Text.pack command))) -> Right (Spawn (Text.pack command))
  ["layout", action] ->
    Layout <$> case action of
      "next" -> Right Next
      "shrink" -> Right Shrink
      "expand" -> Right Expand
      "inc-master" -> Right IncMaster
      "dec-master" -> Right DecMaster
      _ -> Left cliUsage
  _ -> Left cliUsage

instance ToJSON Request where
  toJSON (Request ident operation) = object (["version" .= (1 :: Int), "id" .= ident] <> fields)
   where
    fields = case operation of
      Snapshot -> ["op" .= ("snapshot" :: Text)]
      Subscribe -> ["op" .= ("subscribe" :: Text)]
      Workspace tag -> ["op" .= ("workspace" :: Text), "tag" .= tag]
      Spawn command -> ["op" .= ("spawn" :: Text), "command" .= command]
      Layout action -> ["op" .= ("layout" :: Text), "action" .= layoutName action]
    layoutName action = case action of
      Next -> "next" :: Text
      Shrink -> "shrink"
      Expand -> "expand"
      IncMaster -> "inc-master"
      DecMaster -> "dec-master"
