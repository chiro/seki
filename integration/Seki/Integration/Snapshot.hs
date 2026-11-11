{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.Snapshot (
  Snapshot (..),
  OutputView (..),
  WorkspaceView (..),
  DeviceView (..),
  BuildView (..),
  replySnapshot,
  focusedTag,
) where

import Data.Aeson (Value, eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)

data Snapshot = Snapshot
  { snapSession :: Text
  , snapRevision :: Integer
  , snapOutputs :: [OutputView]
  , snapWorkspaces :: [WorkspaceView]
  , snapTitle :: Maybe Text
  , snapPads :: [(Text, Text)]
  , snapKeyboard :: Bool
  , snapDevices :: [DeviceView]
  , snapBuild :: Maybe BuildView
  }
  deriving (Eq, Show)

data OutputView = OutputView {outputName :: Maybe Text, outputWorkspace :: Maybe Text} deriving (Eq, Show)

data WorkspaceView = WorkspaceView {wsTag :: Text, wsOccupied :: Bool, wsVisible :: Bool, wsFocused :: Bool, wsLayout :: Maybe Text} deriving (Eq, Show)

data DeviceView = DeviceView {deviceName :: Maybe Text, deviceLifecycle :: Text, deviceResults :: [(Text, Text)], deviceKeymap :: Maybe FilePath} deriving (Eq, Show)

data BuildView = BuildView {buildState :: Text, buildGeneration :: Integer, buildError :: Maybe Text, buildImage :: Maybe FilePath} deriving (Eq, Show)

replySnapshot :: BS.ByteString -> Either String Snapshot
replySnapshot bytes = eitherDecodeStrict' bytes >>= parseEither (withObject "reply" (\reply -> reply .: "result" >>= withObject "result" (\result -> result .: "snapshot" >>= parseSnapshot)))

focusedTag :: Snapshot -> Either String Text
focusedTag shot = case [wsTag w | w <- snapWorkspaces shot, wsFocused w] of
  [tag] -> Right tag
  _ -> Left "snapshot must focus one workspace"

parseSnapshot :: Value -> Parser Snapshot
parseSnapshot = withObject "snapshot" $ \o -> do
  outputs <- o .: "outputs" >>= mapM (withObject "output" (\v -> OutputView <$> v .: "name" <*> v .: "workspace"))
  workspaces <- o .: "workspaces" >>= mapM (withObject "workspace" (\v -> WorkspaceView <$> v .: "tag" <*> v .: "occupied" <*> v .: "visible" <*> v .: "focused" <*> v .: "layout"))
  pads <- o .: "scratchpads" >>= mapM (withObject "scratchpad" (\v -> (,) <$> v .: "name" <*> v .: "state"))
  devices <- o .: "input" >>= withObject "input" (\i -> i .: "devices" >>= withObject "devices" (\d -> d .: "items" >>= mapM parseDevice))
  build <- o .:? "build" >>= traverse (withObject "build" (\b -> BuildView <$> b .: "state" <*> b .: "generation" <*> b .:? "error" <*> b .:? "active_image"))
  Snapshot <$> o .: "session" <*> o .: "revision" <*> pure outputs <*> pure workspaces <*> o .: "focused_title" <*> pure pads <*> o .: "keyboard_available" <*> pure devices <*> pure build
 where
  parseDevice = withObject "device" $ \d -> do
    results <- d .: "results" >>= mapM (withObject "result" (\r -> (,,) <$> r .: "setting" <*> r .: "status" <*> r .: "requested"))
    keymap <- traverse (withObject "keymap request" (.: "path")) (listToMaybe [requested | (setting', _, requested) <- results, setting' == ("keymap" :: Text)])
    DeviceView <$> d .: "name" <*> d .: "lifecycle" <*> pure [(setting', status) | (setting', status, _ :: Value) <- results] <*> pure keymap
