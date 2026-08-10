{-# LANGUAGE OverloadedStrings #-}

module Seki.Wire.ArgumentsTest (tests) where

import Data.Binary.Put (putByteString, putWord32host, runPut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import Seki.Wire.Arguments (
  ArgumentDecoder,
  ArgumentEncoder,
  ArgumentError (..),
  DecodeResult (..),
  fixedFromRaw,
  getArray,
  getFd,
  getFixed,
  getInt,
  getNullableObject,
  getNullableString,
  getObject,
  getString,
  getUint,
  putArray,
  putFd,
  putFixed,
  putInt,
  putNullableObject,
  putNullableString,
  putObject,
  putString,
  putUint,
  runArgumentEncoder,
  runCompleteArgumentDecoder,
 )
import Seki.Wire.Codec (maxFramePayloadSize)
import Seki.Wire.Types (ObjectId, mkObjectId)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, chooseInt, counterexample, forAll, listOf, suchThat, vectorOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QuickCheck

tests :: TestTree
tests =
  testGroup
    "Wayland argument codec"
    [ testCase "encodes primitive words in host byte order" testGoldenWords
    , testCase "round trips a mixed argument sequence" testMixedArguments
    , testCase "distinguishes null and empty strings" testNullableStrings
    , testCase "distinguishes null and present objects" testNullableObjects
    , testCase "passes descriptors separately and in order" testDescriptors
    , testCase "reports exact missing bytes" testMissingBytes
    , testCase "reports missing descriptors separately" testMissingDescriptors
    , testCase "rejects interior NUL and invalid UTF-8 strings" testInvalidStrings
    , testCase "rejects missing string terminators" testMissingTerminator
    , testCase "rejects non-zero padding" testNonZeroPadding
    , testCase "rejects null for non-null arguments" testUnexpectedNull
    , testCase "rejects undeclared trailing payload" testTrailingPayload
    , testCase "accepts the largest single array argument" testLargestArray
    , testCase "rejects an argument too large for a frame" testOversizedArgument
    , QuickCheck.testProperty "integers round trip" propIntegerRoundTrip
    , QuickCheck.testProperty "strings round trip" propStringRoundTrip
    , QuickCheck.testProperty "arrays round trip" propArrayRoundTrip
    , QuickCheck.testProperty "every array split requests the exact byte count" propArrayFragmentation
    ]

testGoldenWords :: IO ()
testGoldenWords =
  assertEqual
    "int, uint, and fixed"
    (Right ((), nativeWords [0xffffffff, 0x01020304, 0xffffff80], [] :: [()]))
    ( runArgumentEncoder $ do
        putInt (-1)
        putUint 0x01020304
        putFixed (fixedFromRaw (-128))
    )

testMixedArguments :: IO ()
testMixedArguments = do
  objectId <- requireObjectId 42
  let encoder = do
        putInt (-7)
        putUint 9
        putFixed (fixedFromRaw 384)
        putString "river"
        putObject objectId
        putArray (ByteString.pack [1, 2, 3])
      decoder = (,,,,,) <$> getInt <*> getUint <*> getFixed <*> getString <*> getObject <*> getArray
      expected = (-7, 9, fixedFromRaw 384, "river", objectId, ByteString.pack [1, 2, 3])
  bytes <- expectBytes encoder
  assertEqual "decoded values" (Done expected ByteString.empty []) (runCompleteArgumentDecoder decoder bytes ([] :: [()]))

testNullableStrings :: IO ()
testNullableStrings = do
  bytes <- expectBytes $ do
    putNullableString Nothing
    putNullableString (Just "")
  assertEqual
    "null and empty"
    (Done (Nothing, Just "") ByteString.empty [])
    (runCompleteArgumentDecoder ((,) <$> getNullableString <*> getNullableString) bytes ([] :: [()]))

testNullableObjects :: IO ()
testNullableObjects = do
  objectId <- requireObjectId 7
  bytes <- expectBytes $ do
    putNullableObject Nothing
    putNullableObject (Just objectId)
  assertEqual
    "null and present"
    (Done (Nothing, Just objectId) ByteString.empty [])
    (runCompleteArgumentDecoder ((,) <$> getNullableObject <*> getNullableObject) bytes ([] :: [()]))

testDescriptors :: IO ()
testDescriptors = do
  let encoder = putFd (10 :: Int) >> putUint 5 >> putFd 20
      decoder = (,,) <$> getFd <*> getUint <*> getFd
  case runArgumentEncoder encoder of
    Left errorValue -> fail ("unexpected encode error: " <> show errorValue)
    Right ((), bytes, descriptors) -> do
      assertEqual "descriptor-free payload" (nativeWords [5]) bytes
      assertEqual "descriptor order" [10, 20] descriptors
      assertEqual
        "decoded descriptors"
        (Done (10, 5, 20) ByteString.empty [30])
        (runCompleteArgumentDecoder decoder bytes (descriptors <> [30]))

testMissingBytes :: IO ()
testMissingBytes = do
  bytes <- expectBytes (putArray (ByteString.pack [1, 2, 3, 4, 5]))
  assertEqual "missing length word" (NeedBytes 2) (decodeArray (ByteString.take 2 bytes))
  assertEqual "missing data and padding" (NeedBytes 6) (decodeArray (ByteString.take 6 bytes))
 where
  decodeArray bytes = runCompleteArgumentDecoder getArray bytes ([] :: [()])

testMissingDescriptors :: IO ()
testMissingDescriptors =
  assertEqual
    "one descriptor"
    (NeedFds 1)
    (runCompleteArgumentDecoder getFd ByteString.empty ([] :: [Int]))

testInvalidStrings :: IO ()
testInvalidStrings = do
  assertEqual
    "encoder rejects NUL"
    (Left StringContainsNul)
    (encodedResult (putString "a\0b" :: ArgumentEncoder () ()))
  assertEqual
    "decoder rejects interior NUL"
    (DecodeFailed StringContainsInteriorNul)
    (decodeString (lengthPrefixed (ByteString.pack [97, 0, 98, 0])))
  assertEqual
    "decoder rejects invalid UTF-8"
    (DecodeFailed InvalidStringUtf8)
    (decodeString (lengthPrefixed (ByteString.pack [0xff, 0])))
 where
  decodeString bytes = runCompleteArgumentDecoder getString bytes ([] :: [()])

testMissingTerminator :: IO ()
testMissingTerminator =
  assertEqual
    "last byte is not NUL"
    (DecodeFailed StringMissingTerminatingNul)
    (runCompleteArgumentDecoder getString (lengthPrefixed (ByteString.pack [97, 98])) ([] :: [()]))

testNonZeroPadding :: IO ()
testNonZeroPadding =
  assertEqual
    "array padding"
    (DecodeFailed NonZeroPadding)
    (runCompleteArgumentDecoder getArray bytes ([] :: [()]))
 where
  bytes = nativeWords [1] <> ByteString.pack [0xaa, 0, 1, 0]

testUnexpectedNull :: IO ()
testUnexpectedNull = do
  assertEqual
    "non-null string"
    (DecodeFailed StringIsNull)
    (runCompleteArgumentDecoder getString (nativeWords [0]) ([] :: [()]))
  assertEqual
    "non-null object"
    (DecodeFailed NullObjectId)
    (runCompleteArgumentDecoder getObject (nativeWords [0]) ([] :: [()]))

testTrailingPayload :: IO ()
testTrailingPayload =
  assertEqual
    "one undeclared word"
    (DecodeFailed (TrailingPayload 4))
    (runCompleteArgumentDecoder getUint (nativeWords [1, 2]) ([] :: [()]))

testLargestArray :: IO ()
testLargestArray = do
  let value = ByteString.replicate (maxFramePayloadSize - 4) 0xaa
  bytes <- expectBytes (putArray value)
  assertEqual "fills payload" maxFramePayloadSize (ByteString.length bytes)
  assertEqual
    "largest array"
    (Done value ByteString.empty [])
    (runCompleteArgumentDecoder getArray bytes ([] :: [()]))

testOversizedArgument :: IO ()
testOversizedArgument = do
  let valueSize = maxFramePayloadSize - 3
      value = ByteString.replicate valueSize 0
  assertEqual
    "encoder limit"
    (Left (ArgumentSizeTooLarge valueSize))
    (encodedResult (putArray value :: ArgumentEncoder () ()))
  assertEqual
    "decoder limit"
    (DecodeFailed (ArgumentSizeTooLarge valueSize))
    (runCompleteArgumentDecoder getArray (nativeWords [fromIntegral valueSize]) ([] :: [()]))

propIntegerRoundTrip :: Int32 -> Word32 -> Property
propIntegerRoundTrip signed unsigned =
  roundTrip ((,) <$> getInt <*> getUint) (putInt signed >> putUint unsigned) (signed, unsigned)

propStringRoundTrip :: Property
propStringRoundTrip =
  forAll validText $ \value -> roundTrip getString (putString value) value

propArrayRoundTrip :: Property
propArrayRoundTrip =
  forAll byteArray $ \value -> roundTrip getArray (putArray value) value

propArrayFragmentation :: Property
propArrayFragmentation =
  forAll nonEmptyByteArray $ \value ->
    case runArgumentEncoder (putArray value :: ArgumentEncoder () ()) of
      Left errorValue -> counterexample ("unexpected encode error: " <> show errorValue) False
      Right ((), bytes, _) ->
        forAll (chooseInt (0, ByteString.length bytes - 1)) $ \splitPoint ->
          let expectedNeeded
                | splitPoint < 4 = 4 - splitPoint
                | otherwise = ByteString.length bytes - splitPoint
           in runCompleteArgumentDecoder getArray (ByteString.take splitPoint bytes) ([] :: [()])
                === NeedBytes expectedNeeded

roundTrip :: (Eq value, Show value) => ArgumentDecoder () value -> ArgumentEncoder () () -> value -> Property
roundTrip decoder encoder expected =
  case runArgumentEncoder encoder of
    Left errorValue -> counterexample ("unexpected encode error: " <> show errorValue) False
    Right ((), bytes, descriptors) ->
      runCompleteArgumentDecoder decoder bytes descriptors === Done expected ByteString.empty []

validText :: Gen Text
validText = Text.pack <$> listOf (arbitrary `suchThat` (/= '\0'))

byteArray :: Gen ByteString
byteArray = do
  size <- chooseInt (0, 256)
  ByteString.pack <$> vectorOf size arbitrary

nonEmptyByteArray :: Gen ByteString
nonEmptyByteArray = do
  size <- chooseInt (1, 256)
  ByteString.pack <$> vectorOf size arbitrary

expectBytes :: ArgumentEncoder () () -> IO ByteString
expectBytes encoder =
  case runArgumentEncoder encoder of
    Left errorValue -> fail ("unexpected encode error: " <> show errorValue)
    Right ((), bytes, []) -> pure bytes
    Right ((), _, descriptors) -> fail ("unexpected descriptors: " <> show descriptors)

encodedResult :: ArgumentEncoder fd () -> Either ArgumentError ()
encodedResult encoder = case runArgumentEncoder encoder of
  Left errorValue -> Left errorValue
  Right _ -> Right ()

requireObjectId :: Word32 -> IO ObjectId
requireObjectId value = maybe (fail "test attempted to construct object ID zero") pure (mkObjectId value)

nativeWords :: [Word32] -> ByteString
nativeWords = LazyByteString.toStrict . runPut . mapM_ putWord32host

lengthPrefixed :: ByteString -> ByteString
lengthPrefixed value =
  LazyByteString.toStrict . runPut $ do
    putWord32host (fromIntegral (ByteString.length value))
    putByteString value
    putByteString (ByteString.replicate padding 0)
 where
  padding = (4 - ByteString.length value `rem` 4) `rem` 4
