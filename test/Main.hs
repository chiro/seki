module Main (main) where

import Seki.Fixture.LayerSurfaceTest qualified as LayerSurfaceFixtureTest
import Seki.Internal.Core.ActionTest qualified as ActionTest
import Seki.Internal.Core.EventTest qualified as CoreEventTest
import Seki.Internal.Core.GeometryTest qualified as GeometryTest
import Seki.Internal.Core.LayoutTest qualified as LayoutTest
import Seki.Internal.Core.SceneTest qualified as SceneTest
import Seki.Internal.Core.StackSetTest qualified as StackSetTest
import Seki.Internal.Core.StateTest qualified as StateTest
import Seki.Internal.Layout.FullTest qualified as FullLayoutTest
import Seki.Internal.Layout.TallTest qualified as TallTest
import Seki.Run.ConfigTest qualified as ConfigTest
import Seki.Run.InventoryTest qualified as InventoryTest
import Seki.Run.LatencyPureTest qualified as LatencyPureTest
import Seki.Run.LatencyTest qualified as LatencyTest
import Seki.Run.PhaseTest qualified as PhaseTest
import Seki.Run.PolicyTest qualified as PolicyTest
import Seki.Run.ReconcileTest qualified as ReconcileTest
import Seki.Run.ReconcileTraceTest qualified as ReconcileTraceTest
import Seki.Run.RiverTest qualified as RiverTest
import Seki.Run.RuntimeTest qualified as RuntimeTest
import Seki.Run.SequenceTest qualified as SequenceTest
import Seki.Run.StartupTest qualified as StartupTest
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
main = defaultMain (testGroup "seki" [LatencyPureTest.tests, LatencyTest.tests, ReconcileTest.tests, ReconcileTraceTest.tests, SceneTest.tests, GeometryTest.tests, StackSetTest.tests, StateTest.tests, LayoutTest.tests, FullLayoutTest.tests, TallTest.tests, ActionTest.tests, CoreEventTest.tests, CodecTest.tests, ArgumentsTest.tests, TransportTest.tests, DriverTest.tests, BufferTest.tests, ObjectTableTest.tests, RequestTest.tests, EventTest.tests, ConnectionTest.tests, DispatchTest.tests, RegistryTest.tests, StartupTest.tests, RiverTest.tests, RuntimeTest.tests, SequenceTest.tests, PhaseTest.tests, InventoryTest.tests, ConfigTest.tests, PolicyTest.tests, ScannerTest.tests, GeneratorTest.tests, LayerSurfaceFixtureTest.tests])
