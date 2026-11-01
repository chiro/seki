{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.IPCTest (tests) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Seki.Internal.Core.State (newCoreState)
import Seki.Internal.Input
import Seki.Internal.Layout.Full (Full (..))
import Seki.Protocol.RiverInputManagementV1 qualified as Input
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Run.IPCSchema
import Seki.Run.Input (InputDevice (..), InputInventory (..), SettingOutcome (..), discoverInput, emptyInputInventory)
import Seki.Run.InputSnapshot (inputValue, outcomeName, settingValue)
import Seki.Run.Inventory (emptyRiverInventory)
import Seki.Run.Runtime (RiverInputSnapshot (..), RiverRuntimeSnapshot (..))
import Seki.Run.Snapshot (projectSnapshot)
import Seki.Wire.Connection.ObjectTable
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "IPC"
    [ testCase "large input metadata retains explicit truncation within framing" $ do
        (table, _) <- either (assertFailure . show) pure (newObjectTable defaultObjectLimit Wayland.wlDisplayInterface)
        (_, device) <- either (assertFailure . show) pure (allocateClientObject Input.riverInputDeviceV1Interface 2 table)
        let discovered = foldl' (\i _ -> discoverInput device i) emptyInputInventory [1 .. 200 :: Int]
            inventory = discovered{inputDevices = Map.map (\d -> d{inputName = Just (Text.replicate 4096 "界"), inputType = Just Keyboard}) (inputDevices discovered)}
            projected = inputValue (InputAvailable inventory)
        case projected of
          Object fields -> case KM.lookup "devices" fields of
            Just (Object devices) -> do
              KM.lookup "count" devices @?= Just (Number 200)
              KM.lookup "truncated" devices @?= Just (Bool True)
            _ -> assertFailure "device collection missing"
          _ -> assertFailure "input object missing"
        st <- either (assertFailure . show) pure (newCoreState Full ["1"])
        let snapshot = projectSnapshot "large-input" id ["1"] 1 st (RiverRuntimeSnapshot emptyRiverInventory (InputAvailable inventory) [])
        assertBool "bounded actual IPC line" (either (const False) ((< 65536) . BS.length) (encodeLine snapshot))
    , testCase "curve projection is bounded and finite" $ do
        let value = settingValue (Acceleration (AccelerationCurves [AccelCurve CurveMotion 1 (replicate 4096 0.5)]))
        assertBool "maximum curve summarized" (Lazy.length (encode value) < 512)
        settingValue (AccelSpeed (0 / 0)) @?= Null
        outcomeName SettingSent @?= "sent"
        outcomeName SettingObserved @?= "observed"
        outcomeName SettingApplied @?= "applied"
    , testCase "CLI operations and usage errors" $ do
        parseCLI ["workspace", "3"] @?= Right (Workspace "3")
        parseCLI ["spawn", "echo hello world"] @?= Right (Spawn "echo hello world")
        parseCLI ["layout", "next"] @?= Right (Layout Next)
        mapM_ (\args -> assertBool "usage error" (either (const True) (const False) (parseCLI args))) [[], ["layout", "bogus"], ["spawn", "  "], ["snapshot", "extra"]]
    , testCase "typed command schema" $
        decodeRequest "{\"version\":1,\"id\":\"a\",\"op\":\"workspace\",\"tag\":\"3\"}" @?= Right (Request "a" (Workspace "3"))
    , testCase "invalid version preserves request correlation" $ case decodeRequest "{\"version\":9,\"id\":\"request-42\",\"op\":\"snapshot\"}" of
        Left err -> errorRequestId err @?= "request-42"
        Right _ -> assertFailure "unsupported version accepted"
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
        projectSnapshot "test-session" id ["3", "1"] 7 st (RiverRuntimeSnapshot emptyRiverInventory (InputUnavailable []) [])
          @?= object
            [ "session" .= ("test-session" :: Text.Text)
            , "revision" .= (7 :: Int)
            , "outputs" .= ([] :: [Value])
            , "workspaces"
                .= [ object ["tag" .= ("3" :: Text.Text), "occupied" .= False, "visible" .= False, "focused" .= True, "layout" .= ("Full" :: String), "layout_truncated" .= False]
                   , object ["tag" .= ("1" :: Text.Text), "occupied" .= False, "visible" .= False, "focused" .= False, "layout" .= ("Full" :: String), "layout_truncated" .= False]
                   ]
            , "focused_title" .= Null
            , "focused_title_truncated" .= False
            , "keyboard_available" .= False
            , "urgent" .= Null
            , "input" .= object ["availability" .= ("unavailable" :: Text.Text), "requested_rule_count" .= (0 :: Int), "requested_rules" .= object ["items" .= ([] :: [Value]), "count" .= (0 :: Int), "truncated" .= False], "devices" .= object ["items" .= ([] :: [Value]), "count" .= (0 :: Int), "truncated" .= False]]
            , "xkb" .= Null
            , "submap" .= Null
            ]
    ]
