module Seki.Fixture.ProbeTest (tests) where

import Data.ByteString qualified as ByteString
import Seki.Fixture.Probe (captureLayout)
import Seki.Protocol.Wayland qualified as W
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Seki.Fixture.Probe"
    [ testCase "capture prefers XRGB8888 when both channel orders are offered" $
        fst <$> captureLayout [W.WlShmFormatEnumXbgr8888, W.WlShmFormatEnumXrgb8888] @?= Just W.WlShmFormatEnumXrgb8888
    , testCase "capture accepts XBGR8888 alone" $
        fst <$> captureLayout [W.WlShmFormatEnumXbgr8888] @?= Just W.WlShmFormatEnumXbgr8888
    , testCase "capture accepts ABGR8888 alone" $
        fst <$> captureLayout [W.WlShmFormatEnumAbgr8888] @?= Just W.WlShmFormatEnumAbgr8888
    , testCase "capture rejects formats with other layouts" $
        fst <$> captureLayout [W.WlShmFormatEnumRgb565] @?= Nothing
    , testCase "XRGB8888 pixels read as red, green, blue" $
        rgb [W.WlShmFormatEnumXrgb8888] [0x30, 0x20, 0x10, 0xff, 0x60, 0x50, 0x40, 0xff] @?= Just [0x10, 0x20, 0x30, 0x40, 0x50, 0x60]
    , testCase "XBGR8888 pixels read as red, green, blue" $
        rgb [W.WlShmFormatEnumXbgr8888] [0x10, 0x20, 0x30, 0xff, 0x40, 0x50, 0x60, 0xff] @?= Just [0x10, 0x20, 0x30, 0x40, 0x50, 0x60]
    ]
 where
  rgb offered bytes = ByteString.unpack . ($ ByteString.pack bytes) . snd <$> captureLayout offered
