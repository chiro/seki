{- | Pure encoding and decoding of Wayland wire arguments.

The descriptor type is deliberately polymorphic. The transport layer supplies
an ownership-tracked descriptor type without making the byte codec effectful.
-}
module Seki.Wire.Arguments (
  Fixed,
  fixedFromRaw,
  fixedRawValue,
  ArgumentError (..),
  ArgumentEncoder,
  runArgumentEncoder,
  putInt,
  putUint,
  putFixed,
  putString,
  putNullableString,
  putObject,
  putNullableObject,
  putNewId,
  putArray,
  putFd,
  ArgumentDecoder,
  DecodeResult (..),
  runArgumentDecoder,
  runCompleteArgumentDecoder,
  getInt,
  getUint,
  getFixed,
  getString,
  getNullableString,
  getObject,
  getNullableObject,
  getNewId,
  getArray,
  getFd,
)
where

import Control.Monad (ap)
import Data.Bifunctor (first)
import Data.Binary.Get (getWord32host, runGet)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, word32BE, word32LE)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word32)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import Seki.Wire.Codec (maxFramePayloadSize)
import Seki.Wire.Types (ObjectId, mkObjectId, objectIdValue)

-- | A Wayland signed 24.8 fixed-point value in its lossless wire form.
newtype Fixed = Fixed
  { fixedRawValue :: Int32
  }
  deriving (Eq, Ord, Show)

-- | Construct a fixed-point value from its raw signed 24.8 representation.
fixedFromRaw :: Int32 -> Fixed
fixedFromRaw = Fixed

-- | An invalid argument or argument sequence.
data ArgumentError
  = ArgumentSizeTooLarge Int
  | StringContainsNul
  | StringIsNull
  | StringMissingTerminatingNul
  | StringContainsInteriorNul
  | InvalidStringUtf8
  | NullObjectId
  | NonZeroPadding
  | TrailingPayload Int
  deriving (Eq, Show)

data Encoded fd = Encoded
  { encodedBytes :: Builder
  , encodedFds :: [fd]
  }

instance Semigroup (Encoded fd) where
  leftOutput <> rightOutput =
    Encoded
      { encodedBytes = encodedBytes leftOutput <> encodedBytes rightOutput
      , encodedFds = encodedFds leftOutput <> encodedFds rightOutput
      }

instance Monoid (Encoded fd) where
  mempty = Encoded mempty []

-- | A composable argument encoder whose only possible effect is validation.
newtype ArgumentEncoder fd a = ArgumentEncoder
  { unArgumentEncoder :: Either ArgumentError (a, Encoded fd)
  }

instance Functor (ArgumentEncoder fd) where
  fmap function (ArgumentEncoder result) =
    ArgumentEncoder (fmap (first function) result)

instance Applicative (ArgumentEncoder fd) where
  pure value = ArgumentEncoder (Right (value, mempty))
  (<*>) = ap

instance Monad (ArgumentEncoder fd) where
  ArgumentEncoder result >>= next =
    ArgumentEncoder $ do
      (value, firstOutput) <- result
      (nextValue, secondOutput) <- unArgumentEncoder (next value)
      pure (nextValue, firstOutput <> secondOutput)

-- | Finish an encoder, returning payload bytes and descriptors in argument order.
runArgumentEncoder :: ArgumentEncoder fd a -> Either ArgumentError (a, ByteString, [fd])
runArgumentEncoder (ArgumentEncoder result) = do
  (value, output) <- result
  pure
    ( value
    , LazyByteString.toStrict (toLazyByteString (encodedBytes output))
    , encodedFds output
    )

-- | Encode a signed 32-bit integer.
putInt :: Int32 -> ArgumentEncoder fd ()
putInt = putWord . fromIntegral

-- | Encode an unsigned 32-bit integer.
putUint :: Word32 -> ArgumentEncoder fd ()
putUint = putWord

-- | Encode a signed 24.8 fixed-point value.
putFixed :: Fixed -> ArgumentEncoder fd ()
putFixed = putInt . fixedRawValue

-- | Encode a non-null UTF-8 string.
putString :: Text -> ArgumentEncoder fd ()
putString value
  | Text.any (== '\0') value = failEncoding StringContainsNul
  | otherwise = putLengthPrefixed (TextEncoding.encodeUtf8 value <> ByteString.singleton 0)

-- | Encode a nullable UTF-8 string. Null and the empty string remain distinct.
putNullableString :: Maybe Text -> ArgumentEncoder fd ()
putNullableString Nothing = putUint 0
putNullableString (Just value) = putString value

-- | Encode a non-null object ID.
putObject :: ObjectId -> ArgumentEncoder fd ()
putObject = putUint . objectIdValue

-- | Encode a nullable object ID, using zero for null.
putNullableObject :: Maybe ObjectId -> ArgumentEncoder fd ()
putNullableObject Nothing = putUint 0
putNullableObject (Just objectId) = putObject objectId

-- | Encode an object ID allocated for a @new_id@ argument.
putNewId :: ObjectId -> ArgumentEncoder fd ()
putNewId = putObject

-- | Encode an opaque Wayland byte array.
putArray :: ByteString -> ArgumentEncoder fd ()
putArray = putLengthPrefixed

-- | Append a descriptor without adding payload bytes.
putFd :: fd -> ArgumentEncoder fd ()
putFd descriptor = emit (Encoded mempty [descriptor])

putWord :: Word32 -> ArgumentEncoder fd ()
putWord value = emit (Encoded (word32Host value) [])

putLengthPrefixed :: ByteString -> ArgumentEncoder fd ()
putLengthPrefixed value
  | totalSize > maxFramePayloadSize = failEncoding (ArgumentSizeTooLarge valueSize)
  | otherwise = do
      putUint (fromIntegral valueSize)
      emit (Encoded (byteString value <> byteString (ByteString.replicate padding 0)) [])
 where
  valueSize = ByteString.length value
  padding = paddingSize valueSize
  totalSize = 4 + valueSize + padding

emit :: Encoded fd -> ArgumentEncoder fd ()
emit output = ArgumentEncoder (Right ((), output))

failEncoding :: ArgumentError -> ArgumentEncoder fd a
failEncoding errorValue = ArgumentEncoder (Left errorValue)

word32Host :: Word32 -> Builder
word32Host = case targetByteOrder of
  BigEndian -> word32BE
  LittleEndian -> word32LE

data DecodeState fd = DecodeState
  { remainingBytes :: ByteString
  , remainingFds :: [fd]
  }

data DecodeStep fd a
  = StepDone a (DecodeState fd)
  | StepNeedBytes Int
  | StepNeedFds Int
  | StepFailed ArgumentError

-- | A pure decoder over one frame's payload and the connection's descriptor queue.
newtype ArgumentDecoder fd a = ArgumentDecoder
  { unArgumentDecoder :: DecodeState fd -> DecodeStep fd a
  }

instance Functor (ArgumentDecoder fd) where
  fmap function (ArgumentDecoder decode) =
    ArgumentDecoder (mapDecodeStep function . decode)

instance Applicative (ArgumentDecoder fd) where
  pure value = ArgumentDecoder (StepDone value)
  (<*>) = ap

instance Monad (ArgumentDecoder fd) where
  ArgumentDecoder decode >>= next =
    ArgumentDecoder $ \state ->
      case decode state of
        StepDone value nextState -> unArgumentDecoder (next value) nextState
        StepNeedBytes count -> StepNeedBytes count
        StepNeedFds count -> StepNeedFds count
        StepFailed errorValue -> StepFailed errorValue

-- | The result of incrementally decoding arguments.
data DecodeResult fd a
  = Done a ByteString [fd]
  | NeedBytes Int
  | NeedFds Int
  | DecodeFailed ArgumentError
  deriving (Eq, Show)

-- | Run a decoder and retain unconsumed payload bytes and descriptors.
runArgumentDecoder :: ArgumentDecoder fd a -> ByteString -> [fd] -> DecodeResult fd a
runArgumentDecoder decoder bytes descriptors =
  publicResult (unArgumentDecoder decoder (DecodeState bytes descriptors))

-- | Run a decoder and reject any undeclared payload bytes.
runCompleteArgumentDecoder :: ArgumentDecoder fd a -> ByteString -> [fd] -> DecodeResult fd a
runCompleteArgumentDecoder decoder bytes descriptors =
  case unArgumentDecoder decoder (DecodeState bytes descriptors) of
    StepDone value state
      | ByteString.null (remainingBytes state) -> Done value ByteString.empty (remainingFds state)
      | otherwise -> DecodeFailed (TrailingPayload (ByteString.length (remainingBytes state)))
    other -> publicResult other

-- | Decode a signed 32-bit integer.
getInt :: ArgumentDecoder fd Int32
getInt = fromIntegral <$> getUint

-- | Decode an unsigned 32-bit integer.
getUint :: ArgumentDecoder fd Word32
getUint = ArgumentDecoder $ \state ->
  let bytes = remainingBytes state
      available = ByteString.length bytes
   in if available < 4
        then StepNeedBytes (4 - available)
        else
          StepDone
            (decodeWord (ByteString.take 4 bytes))
            state{remainingBytes = ByteString.drop 4 bytes}

-- | Decode a signed 24.8 fixed-point value.
getFixed :: ArgumentDecoder fd Fixed
getFixed = Fixed <$> getInt

-- | Decode a non-null UTF-8 string.
getString :: ArgumentDecoder fd Text
getString = do
  value <- getNullableString
  maybe (failDecoding StringIsNull) pure value

-- | Decode a nullable UTF-8 string, preserving null versus empty.
getNullableString :: ArgumentDecoder fd (Maybe Text)
getNullableString = do
  declaredSize <- getUint
  if declaredSize == 0
    then pure Nothing
    else do
      bytes <- getLengthPrefixedValue declaredSize
      case ByteString.unsnoc bytes of
        Nothing -> failDecoding StringMissingTerminatingNul
        Just (content, terminator)
          | terminator /= 0 -> failDecoding StringMissingTerminatingNul
          | ByteString.elem 0 content -> failDecoding StringContainsInteriorNul
          | otherwise ->
              case TextEncoding.decodeUtf8' content of
                Left _ -> failDecoding InvalidStringUtf8
                Right value -> pure (Just value)

-- | Decode a non-null object ID.
getObject :: ArgumentDecoder fd ObjectId
getObject = do
  value <- getUint
  maybe (failDecoding NullObjectId) pure (mkObjectId value)

-- | Decode a nullable object ID, treating zero as null.
getNullableObject :: ArgumentDecoder fd (Maybe ObjectId)
getNullableObject = mkObjectId <$> getUint

-- | Decode a non-null object ID carried by a @new_id@ argument.
getNewId :: ArgumentDecoder fd ObjectId
getNewId = getObject

-- | Decode an opaque Wayland byte array.
getArray :: ArgumentDecoder fd ByteString
getArray = getUint >>= getLengthPrefixedValue

-- | Consume the next descriptor without consuming payload bytes.
getFd :: ArgumentDecoder fd fd
getFd = ArgumentDecoder $ \state ->
  case remainingFds state of
    [] -> StepNeedFds 1
    descriptor : descriptors -> StepDone descriptor state{remainingFds = descriptors}

getLengthPrefixedValue :: Word32 -> ArgumentDecoder fd ByteString
getLengthPrefixedValue declaredSize
  | totalSize > maxFramePayloadSize = failDecoding (ArgumentSizeTooLarge valueSize)
  | otherwise = ArgumentDecoder $ \state ->
      let bytes = remainingBytes state
          available = ByteString.length bytes
       in if available < paddedSize
            then StepNeedBytes (paddedSize - available)
            else
              let (value, withPadding) = ByteString.splitAt valueSize bytes
                  (paddingBytes, suffix) = ByteString.splitAt padding withPadding
               in if ByteString.any (/= 0) paddingBytes
                    then StepFailed NonZeroPadding
                    else StepDone value state{remainingBytes = suffix}
 where
  valueSize = fromIntegral declaredSize
  padding = paddingSize valueSize
  paddedSize = valueSize + padding
  totalSize = 4 + paddedSize

failDecoding :: ArgumentError -> ArgumentDecoder fd a
failDecoding errorValue = ArgumentDecoder (const (StepFailed errorValue))

publicResult :: DecodeStep fd a -> DecodeResult fd a
publicResult step = case step of
  StepDone value state -> Done value (remainingBytes state) (remainingFds state)
  StepNeedBytes count -> NeedBytes count
  StepNeedFds count -> NeedFds count
  StepFailed errorValue -> DecodeFailed errorValue

mapDecodeStep :: (a -> b) -> DecodeStep fd a -> DecodeStep fd b
mapDecodeStep function step = case step of
  StepDone value state -> StepDone (function value) state
  StepNeedBytes count -> StepNeedBytes count
  StepNeedFds count -> StepNeedFds count
  StepFailed errorValue -> StepFailed errorValue

decodeWord :: ByteString -> Word32
decodeWord = runGet getWord32host . LazyByteString.fromStrict

paddingSize :: Int -> Int
paddingSize size = (4 - (size .&. 3)) .&. 3
