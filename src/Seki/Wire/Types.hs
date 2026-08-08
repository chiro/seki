-- | Fundamental types used by the Wayland wire codec.
module Seki.Wire.Types (
  ObjectId,
  mkObjectId,
  objectIdValue,
  Opcode (..),
  RawFrame (..),
  FrameDecode (..),
  FrameError (..),
)
where

import Data.ByteString (ByteString)
import Data.Word (Word16, Word32)

-- | A non-zero Wayland object ID.
newtype ObjectId = ObjectId
  { objectIdValue :: Word32
  }
  deriving (Eq, Ord, Show)

-- | Construct an object ID, rejecting the null object representation.
mkObjectId :: Word32 -> Maybe ObjectId
mkObjectId 0 = Nothing
mkObjectId value = Just (ObjectId value)

-- | An opcode in an interface's request or event opcode space.
newtype Opcode = Opcode
  { opcodeValue :: Word16
  }
  deriving (Eq, Ord, Show)

-- | A complete frame whose payload has not yet been decoded as arguments.
data RawFrame = RawFrame
  { rawFrameSender :: ObjectId
  , rawFrameOpcode :: Opcode
  , rawFramePayload :: ByteString
  }
  deriving (Eq, Show)

-- | The result of trying to extract one frame from an input buffer.
data FrameDecode
  = DecodedFrame RawFrame ByteString
  | NeedMoreBytes Int
  deriving (Eq, Show)

-- | A framing error detected before argument decoding begins.
data FrameError
  = SenderIdIsZero
  | FrameSizeTooSmall Word16
  | FrameSizeNotAligned Word16
  | FrameSizeTooLarge Int
  | PayloadSizeNotAligned Int
  deriving (Eq, Show)
