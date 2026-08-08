module Seki.Wire.CodecTest (tests) where

import Data.Binary.Put (putWord32host, runPut)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word16, Word32)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import Seki.Wire.Codec (
  decodeFrame,
  encodeFrame,
  frameHeaderSize,
  maxFramePayloadSize,
  maxFrameSize,
 )
import Seki.Wire.Types (
  FrameDecode (..),
  FrameError (..),
  ObjectId,
  Opcode (..),
  RawFrame (..),
  mkObjectId,
 )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, chooseInt, counterexample, forAll, suchThat, vectorOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QuickCheck

tests :: TestTree
tests =
  testGroup
    "Wayland frame codec"
    [ testCase "encodes a golden header in host byte order" testGoldenHeader
    , testCase "returns the exact missing header byte count" testIncompleteHeader
    , testCase "returns the exact missing payload byte count" testIncompletePayload
    , testCase "decodes one frame and retains the next" testCoalescedFrames
    , testCase "rejects sender object ID zero" testZeroSender
    , testCase "rejects a frame smaller than its header" testFrameTooSmall
    , testCase "rejects an unaligned frame size" testUnalignedFrame
    , testCase "rejects an unaligned payload" testUnalignedPayload
    , testCase "accepts the largest aligned frame" testLargestFrame
    , testCase "rejects a frame beyond the 16-bit aligned maximum" testFrameTooLarge
    , QuickCheck.testProperty "encoded frames round trip" propRoundTrip
    , QuickCheck.testProperty "every incomplete split requests the exact byte count" propFragmentation
    ]

testGoldenHeader :: IO ()
testGoldenHeader = do
  frame <- makeFrame 0x01020304 0x0506 (ByteString.pack [0xaa, 0xbb, 0xcc, 0xdd])
  assertEqual "encoded frame" (Right goldenFrame) (encodeFrame frame)
 where
  goldenFrame = ByteString.pack (goldenHeader <> [0xaa, 0xbb, 0xcc, 0xdd])
  goldenHeader = case targetByteOrder of
    BigEndian -> [0x01, 0x02, 0x03, 0x04, 0x00, 0x0c, 0x05, 0x06]
    LittleEndian -> [0x04, 0x03, 0x02, 0x01, 0x06, 0x05, 0x0c, 0x00]

testIncompleteHeader :: IO ()
testIncompleteHeader = do
  assertEqual "empty input" (Right (NeedMoreBytes frameHeaderSize)) (decodeFrame ByteString.empty)
  assertEqual "seven header bytes" (Right (NeedMoreBytes 1)) (decodeFrame (ByteString.replicate 7 0))

testIncompletePayload :: IO ()
testIncompletePayload =
  assertEqual
    "complete header and absent payload"
    (Right (NeedMoreBytes 4))
    (decodeFrame (header 1 12 2))

testCoalescedFrames :: IO ()
testCoalescedFrames = do
  first <- makeFrame 1 2 ByteString.empty
  second <- makeFrame 3 4 (ByteString.pack [1, 2, 3, 4])
  firstBytes <- expectEncoded first
  secondBytes <- expectEncoded second
  assertEqual
    "first frame and untouched second frame"
    (Right (DecodedFrame first secondBytes))
    (decodeFrame (firstBytes <> secondBytes))

testZeroSender :: IO ()
testZeroSender =
  assertEqual "zero sender" (Left SenderIdIsZero) (decodeFrame (header 0 8 0))

testFrameTooSmall :: IO ()
testFrameTooSmall =
  assertEqual "four-byte frame" (Left (FrameSizeTooSmall 4)) (decodeFrame (header 1 4 0))

testUnalignedFrame :: IO ()
testUnalignedFrame =
  assertEqual "ten-byte frame" (Left (FrameSizeNotAligned 10)) (decodeFrame (header 1 10 0))

testUnalignedPayload :: IO ()
testUnalignedPayload = do
  frame <- makeFrame 1 0 (ByteString.singleton 0)
  assertEqual "one-byte payload" (Left (PayloadSizeNotAligned 1)) (encodeFrame frame)

testLargestFrame :: IO ()
testLargestFrame = do
  frame <- makeFrame 1 maxBound (ByteString.replicate maxFramePayloadSize 0xff)
  encoded <- expectEncoded frame
  assertEqual "maximum frame length" maxFrameSize (ByteString.length encoded)
  assertEqual "maximum frame decode" (Right (DecodedFrame frame ByteString.empty)) (decodeFrame encoded)

testFrameTooLarge :: IO ()
testFrameTooLarge = do
  frame <- makeFrame 1 0 (ByteString.replicate (maxFramePayloadSize + 4) 0)
  assertEqual
    "frame beyond maximum"
    (Left (FrameSizeTooLarge (maxFrameSize + 4)))
    (encodeFrame frame)

propRoundTrip :: ValidFrame -> Property
propRoundTrip (ValidFrame frame) =
  case encodeFrame frame of
    Left err -> counterexample ("unexpected encode error: " <> show err) False
    Right bytes -> decodeFrame bytes === Right (DecodedFrame frame ByteString.empty)

propFragmentation :: ValidFrame -> Property
propFragmentation (ValidFrame frame) =
  case encodeFrame frame of
    Left err -> counterexample ("unexpected encode error: " <> show err) False
    Right bytes ->
      forAll (chooseInt (0, ByteString.length bytes - 1)) $ \splitPoint ->
        let expectedNeeded
              | splitPoint < frameHeaderSize = frameHeaderSize - splitPoint
              | otherwise = ByteString.length bytes - splitPoint
         in decodeFrame (ByteString.take splitPoint bytes) === Right (NeedMoreBytes expectedNeeded)

newtype ValidFrame = ValidFrame RawFrame
  deriving (Show)

instance Arbitrary ValidFrame where
  arbitrary = do
    senderId <- arbitraryObjectId
    opcode <- arbitrary
    wordCount <- chooseInt (0, 64)
    payload <- ByteString.pack <$> vectorOf (wordCount * 4) arbitrary
    pure (ValidFrame (RawFrame senderId (Opcode opcode) payload))

makeFrame :: Word32 -> Word16 -> ByteString.ByteString -> IO RawFrame
makeFrame sender opcode payload =
  case mkObjectId sender of
    Nothing -> fail "test attempted to construct object ID zero"
    Just senderId -> pure (RawFrame senderId (Opcode opcode) payload)

arbitraryObjectId :: Gen ObjectId
arbitraryObjectId = do
  value <- arbitrary `suchThat` (/= 0)
  maybe arbitraryObjectId pure (mkObjectId value)

expectEncoded :: RawFrame -> IO ByteString.ByteString
expectEncoded frame =
  case encodeFrame frame of
    Left err -> fail ("unexpected encode error: " <> show err)
    Right bytes -> pure bytes

header :: Word32 -> Word16 -> Word16 -> ByteString.ByteString
header sender size opcode =
  LazyByteString.toStrict . runPut $ do
    putWord32host sender
    putWord32host ((fromIntegral size * 65536) + fromIntegral opcode)
