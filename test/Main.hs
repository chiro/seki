module Main (main) where

import Seki.Wire.ArgumentsTest qualified as ArgumentsTest
import Seki.Wire.CodecTest qualified as CodecTest
import Seki.Wire.TransportTest qualified as TransportTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "seki" [CodecTest.tests, ArgumentsTest.tests, TransportTest.tests])
