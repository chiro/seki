{-# LANGUAGE OverloadedStrings #-}

module Seki.Latency.Snapshot (BuildReport (..), decodeSnapshot) where

import Data.Aeson (Value, eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.Text (Text)

data BuildReport = BuildReport
  { reportedState :: Text
  , reportedGeneration :: Integer
  , reportedError :: Maybe Text
  }
  deriving (Eq, Show)

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
