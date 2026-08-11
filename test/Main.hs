module Main (main) where

import Seki.Scanner.GenerateTest qualified as GeneratorTest
import Seki.Scanner.ParseTest qualified as ScannerTest
import Seki.Wire.ArgumentsTest qualified as ArgumentsTest
import Seki.Wire.CodecTest qualified as CodecTest
import Seki.Wire.Connection.BufferTest qualified as BufferTest
import Seki.Wire.Connection.ObjectTableTest qualified as ObjectTableTest
import Seki.Wire.Transport.DriverTest qualified as DriverTest
import Seki.Wire.TransportTest qualified as TransportTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "seki" [CodecTest.tests, ArgumentsTest.tests, TransportTest.tests, DriverTest.tests, BufferTest.tests, ObjectTableTest.tests, ScannerTest.tests, GeneratorTest.tests])
