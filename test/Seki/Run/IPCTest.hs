{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.IPCTest (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Seki.Internal.Core.State (newCoreState)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Run.IPCSchema
import Seki.Run.Inventory (emptyRiverInventory)
import Seki.Run.Runtime (RiverRuntimeSnapshot (..))
import Seki.Run.Snapshot (projectSnapshot)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "IPC"
    [ testCase "CLI operations and usage errors" $ do
        parseCLI ["workspace", "3"] @?= Right (Workspace "3")
        parseCLI ["spawn", "echo hello world"] @?= Right (Spawn "echo hello world")
        parseCLI ["layout", "next"] @?= Right (Layout Next)
        mapM_ (\args -> assertBool "usage error" (either (const True) (const False) (parseCLI args))) [[], ["layout", "bogus"], ["spawn", "  "], ["snapshot", "extra"]]
    , testCase "typed command schema" $
        decodeRequest "{\"version\":1,\"id\":\"a\",\"op\":\"workspace\",\"tag\":\"3\"}" @?= Right (Request "a" (Workspace "3"))
    , testCase "reject invalid commands and input" $
        mapM_
          (assertBool "refused" . either (const True) (const False) . decodeRequest)
          ["{\"version\":2,\"id\":\"a\",\"op\":\"snapshot\"}", "{\"version\":1,\"id\":\"a\",\"op\":\"layout\",\"action\":\"bogus\"}", BS.pack [255], BS.replicate 65537 32]
    , testCase "JSON escaping and byte bound" $ do
        let value = object ["title" .= ("<b>\"\n$(touch /tmp/oops)" :: Text.Text)]
        assertBool "valid escaped line" (either (const False) ((== 10) . BS.last) (encodeLine value))
        assertBool "escaped size bounded" (either (const True) (const False) (encodeLine (String (Text.replicate 65536 "\n"))))
    , testCase "headless projection preserves configured order and unsupported capabilities" $ do
        st <- either (assertFailure . show) pure (newCoreState Full ["3", "1"])
        projectSnapshot "test-session" id ["3", "1"] 7 st (RiverRuntimeSnapshot emptyRiverInventory [])
          @?= object
            [ "session" .= ("test-session" :: Text.Text)
            , "revision" .= (7 :: Int)
            , "outputs" .= ([] :: [Value])
            , "workspaces"
                .= [ object ["tag" .= ("3" :: Text.Text), "occupied" .= False, "visible" .= False, "focused" .= True, "layout" .= ("Full" :: String)]
                   , object ["tag" .= ("1" :: Text.Text), "occupied" .= False, "visible" .= False, "focused" .= False, "layout" .= ("Full" :: String)]
                   ]
            , "focused_title" .= Null
            , "focused_title_truncated" .= False
            , "keyboard_available" .= False
            , "urgent" .= Null
            , "xkb" .= Null
            , "submap" .= Null
            ]
    ]
