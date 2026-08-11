{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Scanner.ParseTest (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Either (isRight)
import Data.List (find)
import Data.Text (Text)
import Seki.Scanner.IR (
  Argument (..),
  ArgumentType (..),
  Description (..),
  EnumDefinition (..),
  EnumEntry (..),
  Interface (..),
  Message (..),
  Protocol (..),
 )
import Seki.Scanner.Parse (
  ScannerError (..),
  ScannerProblem (..),
  parseProtocolBytes,
  parseProtocolFile,
  validateProtocols,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland protocol scanner"
    [ testCase "parses every schema form into the IR" testSchemaForms
    , testCase "parses and cross-validates all vendored protocols" testVendoredProtocols
    , testCase "rejects mismatched closing tags" testMismatchedClosingTag
    , testCase "rejects document type declarations" testDoctype
    , testCase "rejects unknown elements and attributes" testUnknownSchema
    , testCase "rejects invalid nullability and reference placement" testArgumentRules
    , testCase "rejects duplicate names" testDuplicateNames
    , testCase "rejects duplicate protocols and cross-document interfaces" testDuplicateDocuments
    , testCase "rejects versions beyond the interface" testVersionRange
    , testCase "rejects invalid and out-of-range enum values" testEnumValues
    , testCase "reports unresolved cross-document references" testUnknownReferences
    ]

testSchemaForms :: Assertion
testSchemaForms = do
  protocol <- expectParsed schemaFixture
  assertEqual "protocol name" "fixture" (protocolName protocol)
  assertEqual
    "decoded description entities"
    (Just (Description "A & B" "body λ"))
    (protocolDescription protocol)
  demo <- requireInterface "demo" protocol
  assertEqual "interface version" 3 (interfaceVersion demo)
  assertBool "frozen extension" (interfaceFrozen demo)
  request <- requireHead "request" (interfaceRequests demo)
  assertEqual "request opcode" 0 (messageOpcode request)
  assertEqual "request since" 2 (messageSince request)
  assertEqual "request deprecation" (Just 3) (messageDeprecatedSince request)
  assertBool "destructor request" (messageDestructor request)
  assertEqual
    "all argument types"
    [ArgumentInt, ArgumentUint, ArgumentFixed, ArgumentString, ArgumentObject, ArgumentNewId, ArgumentNewId, ArgumentArray, ArgumentFd]
    (map argumentType (messageArguments request))
  assertEqual "nullable string" True (argumentNullable (messageArguments request !! 3))
  assertEqual "interface reference" (Just "target") (argumentInterface (messageArguments request !! 4))
  assertEqual "dynamic new_id" Nothing (argumentInterface (messageArguments request !! 6))
  event <- requireHead "event" (interfaceEvents demo)
  assertBool "destructor event" (messageDestructor event)
  enumDefinition <- requireHead "enum" (interfaceEnums demo)
  assertBool "bitfield enum" (enumBitfield enumDefinition)
  assertEqual "enum values" [-1, 16] (map enumEntryValue (enumEntries enumDefinition))
  assertBool "fixture references validate" (isRight (validateProtocols [protocol]))

testVendoredProtocols :: Assertion
testVendoredProtocols = do
  protocols <- mapM expectProtocolFile protocolPaths
  assertEqual
    "protocol names"
    ["wayland", "river_window_management_v1", "river_xkb_bindings_v1"]
    (map protocolName protocols)
  assertEqual "interface counts" [23, 8, 3] (map (length . protocolInterfaces) protocols)
  assertBool "all references resolve" (isRight (validateProtocols protocols))
  wayland <- requireHead "wayland protocol" protocols
  callback <- requireInterface "wl_callback" wayland
  done <- requireHead "wl_callback event" (interfaceEvents callback)
  assertBool "core destructor event" (messageDestructor done)

testMismatchedClosingTag :: Assertion
testMismatchedClosingTag =
  expectProblem
    (XmlMismatchedClosingTag "interface" "protocol")
    "<protocol name=\"p\"><interface name=\"i\" version=\"1\"></protocol>"

testDoctype :: Assertion
testDoctype =
  expectProblem
    XmlDoctypeNotAllowed
    "<!DOCTYPE protocol><protocol name=\"p\"/>"

testUnknownSchema :: Assertion
testUnknownSchema = do
  expectProblem
    (UnknownAttribute "future")
    "<protocol name=\"p\" future=\"yes\"/>"
  expectProblem
    (UnknownElement "future")
    "<protocol name=\"p\"><future/></protocol>"

testArgumentRules :: Assertion
testArgumentRules = do
  expectProblem
    (InvalidAttribute "allow-null" "true")
    (protocolWithArgument "<arg name=\"number\" type=\"uint\" allow-null=\"true\"/>")
  expectProblem
    (InvalidAttribute "interface" "target")
    (protocolWithArgument "<arg name=\"number\" type=\"uint\" interface=\"target\"/>")
  expectProblem
    (InvalidAttribute "enum" "mode")
    (protocolWithArgument "<arg name=\"text\" type=\"string\" enum=\"mode\"/>")

testDuplicateNames :: Assertion
testDuplicateNames =
  expectProblem
    (DuplicateName "same")
    "<protocol name=\"p\"><interface name=\"same\" version=\"1\"/><interface name=\"same\" version=\"1\"/></protocol>"

testDuplicateDocuments :: Assertion
testDuplicateDocuments = do
  protocol <- expectParsed "<protocol name=\"p\"><interface name=\"i\" version=\"1\"/></protocol>"
  case validateProtocols [protocol, protocol] of
    Left errors -> do
      assertBool "duplicate protocol" (hasProblem (DuplicateProtocolName "p") errors)
      assertBool "duplicate interface" (hasProblem (DuplicateInterfaceName "i") errors)
    Right _ -> assertFailure "expected duplicate document validation failure"

testVersionRange :: Assertion
testVersionRange =
  expectProblem
    (VersionOutOfRange 3 2)
    "<protocol name=\"p\"><interface name=\"i\" version=\"2\"><request name=\"late\" since=\"3\"/></interface></protocol>"

testEnumValues :: Assertion
testEnumValues = do
  expectProblem
    (InvalidAttribute "value" "1 << 2")
    (protocolWithEntry "1 &lt;&lt; 2")
  expectProblem
    (NumericValueOutOfRange 4294967296)
    (protocolWithEntry "4294967296")

testUnknownReferences :: Assertion
testUnknownReferences = do
  protocol <- expectParsed (protocolWithArgument "<arg name=\"object\" type=\"object\" interface=\"missing\"/>")
  case validateProtocols [protocol] of
    Left errors -> assertBool "unknown interface is reported" (hasProblem (UnknownInterfaceReference "missing") errors)
    Right _ -> assertFailure "expected reference validation failure"
  enumProtocol <- expectParsed (protocolWithArgument "<arg name=\"number\" type=\"uint\" enum=\"missing.mode\"/>")
  case validateProtocols [enumProtocol] of
    Left errors -> assertBool "unknown enum is reported" (hasProblem (UnknownEnumReference "missing.mode") errors)
    Right _ -> assertFailure "expected enum reference validation failure"

schemaFixture :: ByteString
schemaFixture =
  ByteString.pack
    "<protocol name=\"fixture\">\
    \<description summary=\"A &amp; B\">body &#x3bb;</description>\
    \<interface name=\"target\" version=\"1\"/>\
    \<interface name=\"demo\" version=\"3\" frozen=\"true\">\
    \<request name=\"everything\" since=\"2\" deprecated-since=\"3\" type=\"destructor\">\
    \<arg name=\"signed\" type=\"int\"/>\
    \<arg name=\"flags\" type=\"uint\" enum=\"mode\"/>\
    \<arg name=\"coordinate\" type=\"fixed\"/>\
    \<arg name=\"text\" type=\"string\" allow-null=\"true\"/>\
    \<arg name=\"object\" type=\"object\" interface=\"target\"/>\
    \<arg name=\"created\" type=\"new_id\" interface=\"target\"/>\
    \<arg name=\"dynamic\" type=\"new_id\"/>\
    \<arg name=\"bytes\" type=\"array\"/>\
    \<arg name=\"descriptor\" type=\"fd\"/>\
    \</request>\
    \<event name=\"finished\" type=\"destructor\"/>\
    \<enum name=\"mode\" since=\"2\" bitfield=\"true\">\
    \<entry name=\"negative\" value=\"-1\" since=\"2\"/>\
    \<entry name=\"hexadecimal\" value=\"0x10\" since=\"3\" deprecated-since=\"3\"/>\
    \</enum>\
    \</interface>\
    \</protocol>"

protocolPaths :: [FilePath]
protocolPaths =
  [ "protocol/wayland/wayland.xml"
  , "protocol/river/river-window-management-v1.xml"
  , "protocol/river/river-xkb-bindings-v1.xml"
  ]

protocolWithArgument :: String -> ByteString
protocolWithArgument argument =
  ByteString.pack
    ("<protocol name=\"p\"><interface name=\"i\" version=\"1\"><request name=\"r\">" <> argument <> "</request></interface></protocol>")

protocolWithEntry :: String -> ByteString
protocolWithEntry value =
  ByteString.pack
    ("<protocol name=\"p\"><interface name=\"i\" version=\"1\"><enum name=\"e\"><entry name=\"x\" value=\"" <> value <> "\"/></enum></interface></protocol>")

expectParsed :: ByteString -> IO Protocol
expectParsed bytes = case parseProtocolBytes bytes of
  Left errorValue -> assertFailure ("unexpected scanner error: " <> show errorValue)
  Right protocol -> pure protocol

expectProtocolFile :: FilePath -> IO Protocol
expectProtocolFile path =
  parseProtocolFile path >>= \case
    Left errorValue -> assertFailure ("unexpected scanner error for " <> path <> ": " <> show errorValue)
    Right protocol -> pure protocol

expectProblem :: ScannerProblem -> ByteString -> Assertion
expectProblem expected bytes = case parseProtocolBytes bytes of
  Left ScannerError{scannerProblem = actual} -> assertEqual "scanner problem" expected actual
  Right protocol -> assertFailure ("expected scanner error, parsed: " <> show protocol)

hasProblem :: ScannerProblem -> [ScannerError] -> Bool
hasProblem expected = any ((== expected) . scannerProblem)

requireInterface :: Text -> Protocol -> IO Interface
requireInterface name protocol = case find ((== name) . interfaceName) (protocolInterfaces protocol) of
  Nothing -> assertFailure ("missing interface: " <> show name)
  Just interface -> pure interface

requireHead :: String -> [value] -> IO value
requireHead label = \case
  [] -> assertFailure ("missing " <> label)
  value : _ -> pure value
