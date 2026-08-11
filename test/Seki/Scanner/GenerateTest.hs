{-# LANGUAGE OverloadedStrings #-}

module Seki.Scanner.GenerateTest (tests) where

import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Scanner.Generate (GenerationConfig (..), generateProtocolModule)
import Seki.Scanner.IR (Protocol)
import Seki.Scanner.Parse (parseProtocolFile)
import Seki.Wire.Arguments (
  ArgumentEncoder,
  DecodeResult (..),
  putNewId,
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
    , testCase "produces identical output on repeated runs" testDeterministicGeneration
    , testCase "resolves cross-protocol enums to qualified modules" testCrossProtocolEnum
    , testCase "encodes static and dynamic new IDs" testRequestEncoders
    , testCase "decodes typed registry events and lifecycle metadata" testEventDecoders
    ]

testGeneratedBindingFreshness :: Assertion
testGeneratedBindingFreshness = do
  generated <- generateWayland
  committed <- TextIO.readFile generatedWaylandPath
  assertEqual "generated Wayland module" committed generated

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

generateWayland :: IO Text
generateWayland = do
  protocol <- expectProtocolFile waylandProtocolPath
  expectGenerated waylandConfig [protocol] protocol

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
    , generationSourcePin = "river v0.4.5"
    }

waylandProtocolPath :: FilePath
waylandProtocolPath = "protocol/wayland/wayland.xml"

generatedWaylandPath :: FilePath
generatedWaylandPath = "src-generated/Seki/Protocol/Wayland.hs"

protocolPaths :: [FilePath]
protocolPaths =
  [ waylandProtocolPath
  , "protocol/river/river-window-management-v1.xml"
  , "protocol/river/river-xkb-bindings-v1.xml"
  ]
