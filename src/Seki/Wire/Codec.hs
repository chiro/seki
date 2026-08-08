-- | Pure encoding and incremental decoding of Wayland wire frames.
module Seki.Wire.Codec (
  frameHeaderSize,
  maxFrameSize,
  maxFramePayloadSize,
  encodeFrame,
  decodeFrame,
)
where

import Data.Binary.Get (Get, getWord32host, runGet)
import Data.Binary.Put (putByteString, putWord32host, runPut)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word32)
import Seki.Wire.Types (
  FrameDecode (..),
  FrameError (..),
  ObjectId,
  Opcode (..),
  RawFrame (..),
  mkObjectId,
  objectIdValue,
 )

-- | The size in bytes of every Wayland frame header.
frameHeaderSize :: Int
frameHeaderSize = 8

-- | The largest aligned frame size representable by the 16-bit size field.
maxFrameSize :: Int
maxFrameSize = 65532

-- | The largest payload that can be carried by a valid frame.
maxFramePayloadSize :: Int
maxFramePayloadSize = maxFrameSize - frameHeaderSize

-- | Encode a complete frame in host byte order.
encodeFrame :: RawFrame -> Either FrameError ByteString.ByteString
encodeFrame frame
  | payloadSize `rem` 4 /= 0 = Left (PayloadSizeNotAligned payloadSize)
  | frameSize > maxFrameSize = Left (FrameSizeTooLarge frameSize)
  | otherwise = Right encoded
 where
  payload = rawFramePayload frame
  payloadSize = ByteString.length payload
  frameSize = frameHeaderSize + payloadSize
  sizeAndOpcode =
    (fromIntegral frameSize `shiftL` 16)
      .|. fromIntegral (opcodeValue (rawFrameOpcode frame))
  encoded =
    LazyByteString.toStrict . runPut $ do
      putWord32host (objectIdValue (rawFrameSender frame))
      putWord32host sizeAndOpcode
      putByteString payload

{- | Extract one frame from the front of a buffer.

A successful decode returns the unused suffix so callers can process
coalesced frames without copying it into a separate framing state.
-}
decodeFrame :: ByteString.ByteString -> Either FrameError FrameDecode
decodeFrame input
  | available < frameHeaderSize = Right (NeedMoreBytes (frameHeaderSize - available))
  | otherwise = decodeWithHeader senderWord sizeAndOpcode
 where
  available = ByteString.length input
  (senderWord, sizeAndOpcode) = decodeHeader input

  decodeWithHeader :: Word32 -> Word32 -> Either FrameError FrameDecode
  decodeWithHeader sender encodedHeader = do
    senderId <- maybe (Left SenderIdIsZero) Right (mkObjectId sender)
    validateFrameSize size
    if available < size
      then Right (NeedMoreBytes (size - available))
      else Right (DecodedFrame (makeFrame senderId opcode size) (ByteString.drop size input))
   where
    size = fromIntegral (encodedHeader `shiftR` 16)
    opcode = Opcode (fromIntegral (encodedHeader .&. 0xffff))

  makeFrame :: ObjectId -> Opcode -> Int -> RawFrame
  makeFrame sender opcode size =
    RawFrame
      { rawFrameSender = sender
      , rawFrameOpcode = opcode
      , rawFramePayload = ByteString.take (size - frameHeaderSize) (ByteString.drop frameHeaderSize input)
      }

decodeHeader :: ByteString.ByteString -> (Word32, Word32)
decodeHeader = runGet getHeader . LazyByteString.fromStrict . ByteString.take frameHeaderSize

getHeader :: Get (Word32, Word32)
getHeader = (,) <$> getWord32host <*> getWord32host

validateFrameSize :: Int -> Either FrameError ()
validateFrameSize size
  | size < frameHeaderSize = Left (FrameSizeTooSmall (fromIntegral size))
  | size `rem` 4 /= 0 = Left (FrameSizeNotAligned (fromIntegral size))
  | otherwise = Right ()
