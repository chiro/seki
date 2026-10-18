{-# LANGUAGE OverloadedStrings #-}

module Seki.Scanner.GenerateTest (tests) where

import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Protocol.RiverXkbBindingsV1 qualified as RiverXkb
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Scanner.Generate (GenerationConfig (..), generateProtocolModule)
import Seki.Scanner.IR (Protocol)
import Seki.Scanner.Parse (parseProtocolFile)
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  putNewId,
  putObject,
  putString,
  putUint,
  runArgumentEncoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Binding qualified as Binding
import Seki.Wire.Types (ObjectId, Opcode (..), mkObjectId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland binding generator"
    [ testCase "matches the committed Wayland binding" testGeneratedBindingFreshness
    , testCase "matches the committed river bindings" testGeneratedRiverBindingsFreshness
    , testCase "matches the committed layer-surface fixture binding" testGeneratedFixtureBindingFreshness
    , testCase "matches the committed native probe bindings" testProbeBindingsFreshness
    , testCase "matches the committed virtual pointer binding" testVirtualPointerBindingFreshness
    , testCase "produces identical output on repeated runs" testDeterministicGeneration
    , testCase "resolves cross-protocol enums to qualified modules" testCrossProtocolEnum
    , testCase "encodes static and dynamic new IDs" testRequestEncoders
    , testCase "decodes typed registry events and lifecycle metadata" testEventDecoders
    , testCase "compiles representative cross-protocol river bindings" testRiverBindings
    , testCase "types layer-shell children by the river objects they extend" testLayerShellObjectReferences
    , testCase "resolves wlr-layer-shell's xdg_popup only with xdg-shell" testWlrLayerShellReferences
    ]

testGeneratedBindingFreshness :: Assertion
testGeneratedBindingFreshness = do
  generated <- generateWayland
  committed <- TextIO.readFile generatedWaylandPath
  assertEqual "generated Wayland module" committed generated

testGeneratedRiverBindingsFreshness :: Assertion
testGeneratedRiverBindingsFreshness = do
  generatedWindow <- generateRiverWindow
  committedWindow <- TextIO.readFile generatedRiverWindowPath
  assertEqual "generated river window-management module" committedWindow generatedWindow
  generatedXkb <- generateRiverXkb
  committedXkb <- TextIO.readFile generatedRiverXkbPath
  assertEqual "generated river XKB module" committedXkb generatedXkb
  generatedLayerShell <- generateRiverLayerShell
  committedLayerShell <- TextIO.readFile generatedRiverLayerShellPath
  assertEqual "generated river layer-shell module" committedLayerShell generatedLayerShell

testGeneratedFixtureBindingFreshness :: Assertion
testGeneratedFixtureBindingFreshness = do
  generated <- generateWlrLayerShell
  committed <- TextIO.readFile generatedWlrLayerShellPath
  assertEqual "generated wlr-layer-shell fixture module" committed generated

testDeterministicGeneration :: Assertion
testDeterministicGeneration = do
  firstOutput <- generateWayland
  secondOutput <- generateWayland
  assertEqual "repeated generation" firstOutput secondOutput

testCrossProtocolEnum :: Assertion
testCrossProtocolEnum = do
  protocols <- mapM expectProtocolFile protocolPaths
  xkbProtocol <- case reverse protocols of
    protocol : _ -> pure protocol
    [] -> assertFailure "missing river XKB protocol"
  generated <- expectGenerated riverXkbConfig protocols xkbProtocol
  assertBool
    "qualified module import"
    ("import Seki.Protocol.RiverWindowManagementV1 qualified" `Text.isInfixOf` generated)
  assertBool
    "qualified enum field"
    ("Seki.Protocol.RiverWindowManagementV1.RiverSeatV1ModifiersFlags" `Text.isInfixOf` generated)
  assertBool
    "qualified enum accessor"
    ("Seki.Protocol.RiverWindowManagementV1.riverSeatV1ModifiersFlagsValue" `Text.isInfixOf` generated)

testRequestEncoders :: Assertion
testRequestEncoders = do
  callbackId <- requireObjectId 9
  let syncRequest = Wayland.encodeWlDisplaySync callbackId :: Binding.RequestEncoder ()
      expectedSync = runArgumentEncoder (putNewId callbackId)
  assertEqual "sync name" "sync" (Binding.requestName syncRequest)
  assertEqual "sync opcode" (Opcode 0) (Binding.requestOpcode syncRequest)
  assertEqual "sync since" 1 (Binding.requestSince syncRequest)
  assertBool "sync is not a destructor" (not (Binding.requestDestructor syncRequest))
  assertEqual "sync arguments" expectedSync (runArgumentEncoder (Binding.requestArguments syncRequest))

  boundId <- requireObjectId 12
  let dynamicId = Binding.DynamicNewId "wl_compositor" 4 boundId
      bindRequest = Wayland.encodeWlRegistryBind 27 dynamicId :: Binding.RequestEncoder ()
      expectedBind = runArgumentEncoder $ do
        putUint 27
        Binding.putDynamicNewId dynamicId
  assertEqual "bind opcode" (Opcode 0) (Binding.requestOpcode bindRequest)
  assertEqual "expanded dynamic new_id" expectedBind (runArgumentEncoder (Binding.requestArguments bindRequest))

testEventDecoders :: Assertion
testEventDecoders = do
  payload <- expectEncoded $ do
    putUint 27
    putString "wl_compositor"
    putUint 4
  registryDecoder <- case Wayland.decodeWlRegistryEvent (Opcode 0) of
    Nothing -> assertFailure "missing registry global decoder"
    Just decoder -> pure decoder
  assertEqual "global name" "global" (Binding.eventName registryDecoder)
  assertEqual "global since" 1 (Binding.eventSince registryDecoder)
  assertBool "global is not a destructor" (not (Binding.eventDestructor registryDecoder))
  assertEqual
    "typed registry event"
    (Done (Wayland.WlRegistryGlobal 27 "wl_compositor" 4) ByteString.empty [])
    (runCompleteArgumentDecoder (Binding.eventArguments registryDecoder) payload ([] :: [()]))

  callbackDecoder <- case Wayland.decodeWlCallbackEvent (Opcode 0) of
    Nothing -> assertFailure "missing callback done decoder"
    Just decoder -> pure decoder
  assertBool "callback done destroys its object" (Binding.eventDestructor callbackDecoder)
  assertBool "unknown opcode is rejected" (case Wayland.decodeWlRegistryEvent (Opcode 2) of Nothing -> True; Just _ -> False)

testRiverBindings :: Assertion
testRiverBindings = do
  assertEqual "window manager maximum" 4 (Binding.interfaceMaximumVersion River.riverWindowManagerV1Interface)
  assertEqual "XKB global maximum" 3 (Binding.interfaceMaximumVersion RiverXkb.riverXkbBindingsV1Interface)
  seatId <- requireObjectId 21
  bindingId <- requireObjectId 22
  let request =
        RiverXkb.encodeRiverXkbBindingsV1GetXkbBinding
          seatId
          bindingId
          0x41
          River.RiverSeatV1ModifiersFlagsShift ::
          Binding.RequestEncoder ()
      expected = runArgumentEncoder $ do
        putObject seatId
        putNewId bindingId
        putUint 0x41
        putUint 1
  assertEqual "cross-protocol request name" "get_xkb_binding" (Binding.requestName request)
  assertEqual "cross-protocol enum encoding" expected (runArgumentEncoder (Binding.requestArguments request))
  managerDecoder <- case River.decodeRiverWindowManagerV1Event (Opcode 2) of
    Nothing -> assertFailure "missing manage_start decoder"
    Just decoder -> pure decoder
  assertEqual
    "typed manage_start event"
    (Done River.RiverWindowManagerV1ManageStart ByteString.empty [])
    (runCompleteArgumentDecoder (Binding.eventArguments managerDecoder) ByteString.empty ([] :: [()]))
  serverId <- requireObjectId 0xff000001
  serverPayload <- expectEncoded (putNewId serverId)
  windowDecoder <- case River.decodeRiverWindowManagerV1Event (Opcode 6) of
    Nothing -> assertFailure "missing window decoder"
    Just decoder -> pure decoder
  case runCompleteArgumentDecoder (Binding.eventArguments windowDecoder) serverPayload ([] :: [()]) of
    Done event@(River.RiverWindowManagerV1Window newWindow) remaining remainingFds -> do
      assertEqual "server-created window ID" serverId (Binding.newObjectId newWindow)
      assertEqual "generated new-ID metadata" [serverId] (Binding.eventNewIds windowDecoder event)
      assertEqual "consumed server-created payload" ByteString.empty remaining
      assertEqual "consumed server-created descriptors" [] remainingFds
    other -> assertFailure ("unexpected server-created window decode: " <> show other)

generateWayland :: IO Text
generateWayland = do
  protocol <- expectProtocolFile waylandProtocolPath
  expectGenerated waylandConfig [protocol] protocol

generateRiverWindow :: IO Text
generateRiverWindow = do
  wayland <- expectProtocolFile waylandProtocolPath
  windowManagement <- expectProtocolFile riverWindowProtocolPath
  xkbBindings <- expectProtocolFile riverXkbProtocolPath
  expectGenerated riverWindowConfig [windowManagement, wayland, xkbBindings] windowManagement

generateRiverLayerShell :: IO Text
generateRiverLayerShell = do
  wayland <- expectProtocolFile waylandProtocolPath
  windowManagement <- expectProtocolFile riverWindowProtocolPath
  layerShell <- expectProtocolFile riverLayerShellProtocolPath
  expectGenerated riverLayerShellConfig [layerShell, wayland, windowManagement] layerShell

testLayerShellObjectReferences :: Assertion
testLayerShellObjectReferences = do
  generated <- generateRiverLayerShell
  assertBool
    "get_output takes the river output it extends"
    ("encodeRiverLayerShellV1GetOutput :: Wire.ObjectId -> Wire.ObjectId -> Binding.RequestEncoder fd" `Text.isInfixOf` generated)
  assertBool
    "get_seat takes the river seat it extends"
    ("encodeRiverLayerShellV1GetSeat :: Wire.ObjectId -> Wire.ObjectId -> Binding.RequestEncoder fd" `Text.isInfixOf` generated)

testWlrLayerShellReferences :: Assertion
testWlrLayerShellReferences = do
  wayland <- expectProtocolFile waylandProtocolPath
  wlrLayerShell <- expectProtocolFile wlrLayerShellProtocolPath
  case generateProtocolModule wlrLayerShellConfig [wlrLayerShell, wayland] wlrLayerShell of
    Left _ -> pure ()
    Right _ -> assertFailure "generated wlr-layer-shell without the xdg_popup definition"
  _ <- generateWlrLayerShell
  pure ()

generateWlrLayerShell :: IO Text
generateWlrLayerShell = do
  wayland <- expectProtocolFile waylandProtocolPath
  xdgShell <- expectProtocolFile xdgShellProtocolPath
  wlrLayerShell <- expectProtocolFile wlrLayerShellProtocolPath
  expectGenerated wlrLayerShellConfig [wlrLayerShell, wayland, xdgShell] wlrLayerShell

generateRiverXkb :: IO Text
generateRiverXkb = do
  wayland <- expectProtocolFile waylandProtocolPath
  windowManagement <- expectProtocolFile riverWindowProtocolPath
  xkbBindings <- expectProtocolFile riverXkbProtocolPath
  expectGenerated riverXkbConfig [xkbBindings, wayland, windowManagement] xkbBindings

expectGenerated :: GenerationConfig -> [Protocol] -> Protocol -> IO Text
expectGenerated config protocols target = case generateProtocolModule config protocols target of
  Left errorValue -> assertFailure ("unexpected generation error: " <> show errorValue)
  Right generated -> pure generated

expectProtocolFile :: FilePath -> IO Protocol
expectProtocolFile path = do
  result <- parseProtocolFile path
  case result of
    Left errorValue -> assertFailure ("unexpected scanner error: " <> show errorValue)
    Right protocol -> pure protocol

expectEncoded :: BindingRequest -> IO ByteString.ByteString
expectEncoded encoder = case runArgumentEncoder encoder of
  Left errorValue -> assertFailure ("unexpected argument encoding error: " <> show errorValue)
  Right ((), bytes, []) -> pure bytes
  Right (_, _, descriptors) -> assertFailure ("unexpected descriptors: " <> show (length descriptors))

type BindingRequest = ArgumentEncoder () ()

requireObjectId :: Word -> IO ObjectId
requireObjectId value = case mkObjectId (fromIntegral value) of
  Nothing -> assertFailure "test object ID must be non-zero"
  Just objectId -> pure objectId

waylandConfig :: GenerationConfig
waylandConfig =
  GenerationConfig
    { generationModuleName = "Seki.Protocol.Wayland"
    , generationSourcePath = Text.pack waylandProtocolPath
    , generationSourcePin = "Wayland 1.25.0, commit 3e673a438b0a9749e3bdf5cac4befac86333024c"
    }

riverXkbConfig :: GenerationConfig
riverXkbConfig =
  GenerationConfig
    { generationModuleName = "Seki.Protocol.RiverXkbBindingsV1"
    , generationSourcePath = "protocol/river/river-xkb-bindings-v1.xml"
    , generationSourcePin = riverSourcePin
    }

riverWindowConfig :: GenerationConfig
riverWindowConfig =
  GenerationConfig
    { generationModuleName = "Seki.Protocol.RiverWindowManagementV1"
    , generationSourcePath = Text.pack riverWindowProtocolPath
    , generationSourcePin = riverSourcePin
    }

riverLayerShellConfig :: GenerationConfig
riverLayerShellConfig =
  GenerationConfig
    { generationModuleName = "Seki.Protocol.RiverLayerShellV1"
    , generationSourcePath = Text.pack riverLayerShellProtocolPath
    , generationSourcePin = riverSourcePin
    }

wlrLayerShellConfig :: GenerationConfig
wlrLayerShellConfig =
  GenerationConfig
    { generationModuleName = "Seki.Fixture.Protocol.WlrLayerShellUnstableV1"
    , generationSourcePath = Text.pack wlrLayerShellProtocolPath
    , generationSourcePin = riverSourcePin
    }

riverSourcePin :: Text
riverSourcePin = "river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f"

waylandProtocolPath :: FilePath
waylandProtocolPath = "protocol/wayland/wayland.xml"

generatedWaylandPath :: FilePath
generatedWaylandPath = "src-generated/Seki/Protocol/Wayland.hs"

generatedRiverWindowPath :: FilePath
generatedRiverWindowPath = "src-generated/Seki/Protocol/RiverWindowManagementV1.hs"

generatedRiverXkbPath :: FilePath
generatedRiverXkbPath = "src-generated/Seki/Protocol/RiverXkbBindingsV1.hs"

riverWindowProtocolPath :: FilePath
riverWindowProtocolPath = "protocol/river/river-window-management-v1.xml"

riverXkbProtocolPath :: FilePath
riverXkbProtocolPath = "protocol/river/river-xkb-bindings-v1.xml"

generatedRiverLayerShellPath :: FilePath
generatedRiverLayerShellPath = "src-generated/Seki/Protocol/RiverLayerShellV1.hs"

riverLayerShellProtocolPath :: FilePath
riverLayerShellProtocolPath = "protocol/river/river-layer-shell-v1.xml"

wlrLayerShellProtocolPath :: FilePath
wlrLayerShellProtocolPath = "protocol/river/upstream/wlr-layer-shell-unstable-v1.xml"

generatedWlrLayerShellPath :: FilePath
generatedWlrLayerShellPath = "tools-generated/layer-surface-fixture/Seki/Fixture/Protocol/WlrLayerShellUnstableV1.hs"

xdgShellProtocolPath :: FilePath
xdgShellProtocolPath = "protocol/wayland-protocols/xdg-shell.xml"

protocolPaths :: [FilePath]
protocolPaths =
  [ waylandProtocolPath
  , riverWindowProtocolPath
  , riverXkbProtocolPath
  ]

testVirtualPointerBindingFreshness :: Assertion
testVirtualPointerBindingFreshness = do
  wayland <- expectProtocolFile waylandProtocolPath
  target <- expectProtocolFile path
  generated <-
    expectGenerated
      GenerationConfig
        { generationModuleName = "Seki.Fixture.Protocol.WlrVirtualPointerUnstableV1"
        , generationSourcePath = Text.pack path
        , generationSourcePin = "wayland-protocols-wlr 0.3.12"
        }
      [target, wayland]
      target
  committed <- TextIO.readFile "tools-generated/layer-surface-fixture/Seki/Fixture/Protocol/WlrVirtualPointerUnstableV1.hs"
  assertEqual "virtual pointer generated binding" committed generated
 where
  path = "protocol/wlr-protocols/wlr-virtual-pointer-unstable-v1.xml"

testProbeBindingsFreshness :: Assertion
testProbeBindingsFreshness = do
  wayland <- expectProtocolFile waylandProtocolPath
  foreignList <- expectProtocolFile "protocol/wayland-protocols/ext-foreign-toplevel-list-v1.xml"
  source <- expectProtocolFile "protocol/wayland-protocols/ext-image-capture-source-v1.xml"
  mapM_
    (check wayland foreignList source)
    [("XdgShell", "xdg-shell"), ("ExtImageCaptureSourceV1", "ext-image-capture-source-v1"), ("ExtImageCopyCaptureV1", "ext-image-copy-capture-v1")]
 where
  check wayland foreignList source (moduleSuffix, stem) = do
    let path = "protocol/wayland-protocols/" <> stem <> ".xml"
        config =
          GenerationConfig
            { generationModuleName = "Seki.Fixture.Protocol." <> Text.pack moduleSuffix
            , generationSourcePath = Text.pack path
            , generationSourcePin = "wayland-protocols 1.49, commit ee78491a237eaff9389a0ccf8680521d074407d3"
            }
    target <- expectProtocolFile path
    let references = if stem == "ext-image-copy-capture-v1" then [wayland, foreignList, source] else [wayland, foreignList]
    generated <- expectGenerated config (target : references) target
    committed <- TextIO.readFile ("tools-generated/layer-surface-fixture/Seki/Fixture/Protocol/" <> moduleSuffix <> ".hs")
    assertEqual (moduleSuffix <> " generated binding") committed generated
