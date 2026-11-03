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
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text

data RequestError = RequestError {errorRequestId :: Text, errorMessage :: String, errorClosesConnection :: Bool} deriving (Eq, Show)

data Request = Request {requestId :: Text, requestOperation :: Operation} deriving (Eq, Show)
data Operation = Snapshot | Subscribe | Recompile | Scratchpad Text | ScratchpadReset Text | Workspace Text | Spawn Text | Layout LayoutOp deriving (Eq, Show)
data LayoutOp = Next | Shrink | Expand | IncMaster | DecMaster deriving (Eq, Show, Enum, Bounded)

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
      "recompile" -> pure Recompile
      "scratchpad" -> Scratchpad <$> o .: "name"
      "scratchpad-reset" -> ScratchpadReset <$> o .: "name"
      "workspace" -> Workspace <$> o .: "tag"
      "spawn" -> do
        command <- o .: "command"
        either fail pure (spawnOperation command)
      "layout" -> do
        action <- o .: "action" :: Parser Text
        maybe (fail "unknown layout action") (pure . Layout) (layoutOperation action)
      _ -> fail "unknown operation"
    pure (Request ident operation)

decodeRequest :: BS.ByteString -> Either RequestError Request
decodeRequest bytes
  | BS.length bytes > maxLineBytes = Left (RequestError "" "line exceeds 65536 bytes" True)
  | otherwise = do
      value <- first (\message -> RequestError "" message True) (eitherDecodeStrict' bytes)
      let ident = case value of
            Object fields | Just (String text) <- KeyMap.lookup "id" fields, Text.length text <= 128 -> text
            _ -> ""
      first (\message -> RequestError ident message False) (parseEither parseJSON value)

encodeLine :: Value -> Either String BS.ByteString
encodeLine value
  | BL.length bytes > fromIntegral maxLineBytes = Left "line exceeds 65536 bytes"
  | otherwise = Right (BL.toStrict bytes <> "\n")
 where
  bytes = BL.take (fromIntegral maxLineBytes + 1) (encode value)

cliUsage :: String
cliUsage = "usage: sekictl snapshot | subscribe | recompile | scratchpad NAME | scratchpad-reset NAME | workspace TAG | spawn COMMAND | layout {next|shrink|expand|inc-master|dec-master}"

parseCLI :: [String] -> Either String Operation
parseCLI args = case args of
  ["snapshot"] -> Right Snapshot
  ["subscribe"] -> Right Subscribe
  ["recompile"] -> Right Recompile
  ["scratchpad", name] -> Right (Scratchpad (Text.pack name))
  ["scratchpad-reset", name] -> Right (ScratchpadReset (Text.pack name))
  ["workspace", tag] -> Right (Workspace (Text.pack tag))
  ["spawn", command] -> first (const cliUsage) (spawnOperation (Text.pack command))
  ["layout", action] -> maybe (Left cliUsage) (Right . Layout) (layoutOperation (Text.pack action))
  _ -> Left cliUsage

instance ToJSON Request where
  toJSON (Request ident operation) = object (["version" .= (1 :: Int), "id" .= ident] <> fields)
   where
    fields = case operation of
      Snapshot -> ["op" .= ("snapshot" :: Text)]
      Subscribe -> ["op" .= ("subscribe" :: Text)]
      Recompile -> ["op" .= ("recompile" :: Text)]
      Scratchpad name -> ["op" .= ("scratchpad" :: Text), "name" .= name]
      ScratchpadReset name -> ["op" .= ("scratchpad-reset" :: Text), "name" .= name]
      Workspace tag -> ["op" .= ("workspace" :: Text), "tag" .= tag]
      Spawn command -> ["op" .= ("spawn" :: Text), "command" .= command]
      Layout action -> ["op" .= ("layout" :: Text), "action" .= layoutName action]

layoutName :: LayoutOp -> Text
layoutName action = case action of
  Next -> "next"
  Shrink -> "shrink"
  Expand -> "expand"
  IncMaster -> "inc-master"
  DecMaster -> "dec-master"

layoutOperation :: Text -> Maybe LayoutOp
layoutOperation name = find ((== name) . layoutName) [minBound .. maxBound]

spawnOperation :: Text -> Either String Operation
spawnOperation command
  | Text.null (Text.strip command) = Left "empty command"
  | otherwise = Right (Spawn command)
