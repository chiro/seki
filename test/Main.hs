module Main (main) where

import Seki.Run.RiverTest qualified as RiverTest
import Seki.Scanner.GenerateTest qualified as GeneratorTest
import Seki.Scanner.ParseTest qualified as ScannerTest
import Seki.Wire.ArgumentsTest qualified as ArgumentsTest
import Seki.Wire.CodecTest qualified as CodecTest
import Seki.Wire.Connection.BufferTest qualified as BufferTest
import Seki.Wire.Connection.DispatchTest qualified as DispatchTest
import Seki.Wire.Connection.EventTest qualified as EventTest
import Seki.Wire.Connection.ObjectTableTest qualified as ObjectTableTest
import Seki.Wire.Connection.RegistryTest qualified as RegistryTest
import Seki.Wire.Connection.RequestTest qualified as RequestTest
import Seki.Wire.ConnectionTest qualified as ConnectionTest
import Seki.Wire.Transport.DriverTest qualified as DriverTest
import Seki.Wire.TransportTest qualified as TransportTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "seki" [CodecTest.tests, ArgumentsTest.tests, TransportTest.tests, DriverTest.tests, BufferTest.tests, ObjectTableTest.tests, RequestTest.tests, EventTest.tests, ConnectionTest.tests, DispatchTest.tests, RegistryTest.tests, RiverTest.tests, ScannerTest.tests, GeneratorTest.tests])
