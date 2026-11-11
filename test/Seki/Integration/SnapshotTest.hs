{-# LANGUAGE OverloadedStrings #-}

module Seki.Integration.SnapshotTest (tests) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Either (isLeft)
import Data.Text (Text)
import Seki.Integration.Snapshot
import Seki.Internal.Core.State (newCoreState)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Run.Inventory (emptyRiverInventory)
import Seki.Run.Runtime (RiverInputSnapshot (..), RiverRuntimeSnapshot (..))
import Seki.Run.Snapshot (projectSnapshot)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "integration snapshot decoder"
    [ testCase "a production snapshot line decodes into every field the scenarios read" $ do
        snapshot <- projected
        let failed = object ["state" .= ("failed" :: Text), "generation" .= (3 :: Int), "error" .= ("stub check failed" :: Text), "log_path" .= ("/state/build.log" :: Text), "active_image" .= ("/state/seki" :: Text)]
        replySnapshot (line (with "build" failed snapshot))
          @?= Right
            Snapshot
              { snapSession = "latency-session"
              , snapRevision = 7
              , snapOutputs = []
              , snapWorkspaces = [WorkspaceView "2" False False True (Just "Full"), WorkspaceView "1" False False False (Just "Full")]
              , snapTitle = Nothing
              , snapPads = []
              , snapKeyboard = False
              , snapDevices = []
              , snapBuild = Just (BuildView "failed" 3 (Just "stub check failed") (Just "/state/seki"))
              }
    , testCase "the focused tag is the one workspace marked focused" $ do
        snapshot <- projected
        fmap focusedTag (replySnapshot (line snapshot)) @?= Right (Right "2")
    , testCase "a snapshot that focuses no workspace has no focused tag" $ do
        snapshot <- projected
        let unfocused = object ["tag" .= ("1" :: Text), "occupied" .= False, "visible" .= False, "focused" .= False, "layout" .= ("Full" :: Text)]
        fmap (isLeft . focusedTag) (replySnapshot (line (with "workspaces" (toJSON [unfocused]) snapshot))) @?= Right True
    , testCase "a snapshot from a WM without a build session has no build report" $ do
        snapshot <- projected
        fmap snapBuild (replySnapshot (line (with "build" Null snapshot))) @?= Right Nothing
    , testCase "an error reply is refused" $
        assertBool "error envelope" (isLeft (replySnapshot (Lazy.toStrict (encode (object ["version" .= (1 :: Int), "id" .= ("latency" :: Text), "error" .= object ["code" .= ("busy" :: Text)]])))))
    ]
 where
  projected = do
    st <- either (assertFailure . show) pure (newCoreState Full ["2", "1"])
    pure (projectSnapshot "latency-session" id ["2", "1"] 7 st (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) []))
  with key value (Object fields) = Object (KeyMap.insert key value fields)
  with _ _ other = other
  line :: Value -> BS.ByteString
  line snapshot = Lazy.toStrict (encode (object ["version" .= (1 :: Int), "id" .= ("latency" :: Text), "result" .= object ["snapshot" .= snapshot]]))
