{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.LatencySnapshotTest (tests) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Either (isLeft)
import Data.Text (Text)
import Seki.Internal.Core.State (newCoreState)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Latency.Snapshot
import Seki.Run.Inventory (emptyRiverInventory)
import Seki.Run.Runtime (RiverInputSnapshot (..), RiverRuntimeSnapshot (..))
import Seki.Run.Snapshot (projectSnapshot)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "latency subscriber snapshot"
    [ testCase "a production snapshot line gives its revision, focused workspace, and build report" $ do
        snapshot <- projected
        let failed = object ["state" .= ("failed" :: Text), "generation" .= (3 :: Int), "error" .= ("stub check failed" :: Text), "log_path" .= ("/state/build.log" :: Text)]
        decodeSnapshot (line (with "build" failed snapshot)) @?= Right (7, "2", Just (BuildReport "failed" 3 (Just "stub check failed")))
    , testCase "a snapshot from a WM without a build session has no build report" $ do
        snapshot <- projected
        decodeSnapshot (line (with "build" Null snapshot)) @?= Right (7, "2", Nothing)
    , testCase "a snapshot that focuses no workspace is refused" $ do
        snapshot <- projected
        let unfocused = object ["tag" .= ("1" :: Text), "occupied" .= False, "visible" .= False, "focused" .= False, "layout" .= ("Full" :: Text)]
        assertBool "no focused workspace" (isLeft (decodeSnapshot (line (with "workspaces" (toJSON [unfocused]) snapshot))))
    , testCase "an error reply is refused" $
        assertBool "error envelope" (isLeft (decodeSnapshot (Lazy.toStrict (encode (object ["version" .= (1 :: Int), "id" .= ("latency" :: Text), "error" .= object ["code" .= ("busy" :: Text)]])))))
    ]
 where
  projected = do
    st <- either (assertFailure . show) pure (newCoreState Full ["2", "1"])
    pure (projectSnapshot "latency-session" id ["2", "1"] 7 st (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) []))
  with key value (Object fields) = Object (KeyMap.insert key value fields)
  with _ _ other = other
  line :: Value -> BS.ByteString
  line snapshot = Lazy.toStrict (encode (object ["version" .= (1 :: Int), "id" .= ("latency" :: Text), "result" .= object ["snapshot" .= snapshot]]))
