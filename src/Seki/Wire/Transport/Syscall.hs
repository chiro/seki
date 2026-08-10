{-# LANGUAGE ForeignFunctionInterface #-}

-- | Raw, non-blocking calls through the ancillary-data C shim.
module Seki.Wire.Transport.Syscall (
  RawReceiveResult (..),
  RawReceiveError (..),
  sendRaw,
  receiveRaw,
)
where

import Control.Exception (onException)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import System.Posix.IO (closeFd)
import System.Posix.Types (CSsize (..), Fd (..))

data RawReceiveResult
  = RawReceived ByteString [Fd]
  | RawReceiveSystemError CInt
  | RawReceiveRejected RawReceiveError
  deriving (Eq, Show)

data RawReceiveError
  = RawControlTruncated
  | RawMalformedControl
  | RawUnsupportedControl
  | RawTooManyFds
  | RawCloseOnExecFailed CInt
  | RawDataTruncated
  | RawUnknownReceiveStatus CInt
  deriving (Eq, Show)

sendRaw :: Fd -> ByteString -> [Fd] -> IO (Either CInt Int)
sendRaw (Fd socketFd) bytes descriptors =
  ByteString.useAsCStringLen bytes $ \(bytePointer, byteCount) ->
    withArrayLen (map fdNumber descriptors) $ \descriptorCount descriptorPointer ->
      alloca $ \errorPointer -> do
        sent <-
          cSendMessage
            socketFd
            (castPtr bytePointer)
            (fromIntegral byteCount)
            descriptorPointer
            (fromIntegral descriptorCount)
            errorPointer
        if sent < 0
          then Left <$> peek errorPointer
          else pure (Right (fromIntegral sent))
 where
  fdNumber (Fd descriptor) = descriptor

receiveRaw :: Fd -> Int -> Int -> IO RawReceiveResult
receiveRaw (Fd socketFd) byteCapacity fdCapacity =
  allocaBytes byteCapacity $ \bytePointer ->
    allocaArray fdCapacity $ \descriptorPointer ->
      alloca $ \byteCountPointer ->
        alloca $ \fdCountPointer ->
          alloca $ \errorPointer -> do
            status <-
              cReceiveMessage
                socketFd
                bytePointer
                (fromIntegral byteCapacity)
                descriptorPointer
                (fromIntegral fdCapacity)
                byteCountPointer
                fdCountPointer
                errorPointer
            decodeResult status bytePointer descriptorPointer byteCountPointer fdCountPointer errorPointer

decodeResult :: CInt -> Ptr Word8 -> Ptr CInt -> Ptr CSsize -> Ptr CSize -> Ptr CInt -> IO RawReceiveResult
decodeResult status bytePointer descriptorPointer byteCountPointer fdCountPointer errorPointer
  | status == receiveOk = do
      byteCount <- fromIntegral <$> peek byteCountPointer
      fdCount <- fromIntegral <$> peek fdCountPointer
      descriptors <- map Fd <$> peekArray fdCount descriptorPointer
      bytes <-
        ByteString.packCStringLen (castPtr bytePointer, byteCount)
          `onException` mapM_ closeFd descriptors
      pure (RawReceived bytes descriptors)
  | status == receiveSystemError = RawReceiveSystemError <$> peek errorPointer
  | status == receiveControlTruncated = pure (RawReceiveRejected RawControlTruncated)
  | status == receiveMalformedControl = pure (RawReceiveRejected RawMalformedControl)
  | status == receiveUnsupportedControl = pure (RawReceiveRejected RawUnsupportedControl)
  | status == receiveTooManyFds = pure (RawReceiveRejected RawTooManyFds)
  | status == receiveCloseOnExecFailed = RawReceiveRejected . RawCloseOnExecFailed <$> peek errorPointer
  | status == receiveDataTruncated = pure (RawReceiveRejected RawDataTruncated)
  | otherwise = pure (RawReceiveRejected (RawUnknownReceiveStatus status))

receiveOk, receiveSystemError, receiveControlTruncated, receiveMalformedControl :: CInt
receiveOk = 0
receiveSystemError = 1
receiveControlTruncated = 2
receiveMalformedControl = 3

receiveUnsupportedControl, receiveTooManyFds, receiveCloseOnExecFailed, receiveDataTruncated :: CInt
receiveUnsupportedControl = 4
receiveTooManyFds = 5
receiveCloseOnExecFailed = 6
receiveDataTruncated = 7

foreign import ccall unsafe "seki_send_message"
  cSendMessage :: CInt -> Ptr Word8 -> CSize -> Ptr CInt -> CSize -> Ptr CInt -> IO CSsize

foreign import ccall unsafe "seki_receive_message"
  cReceiveMessage :: CInt -> Ptr Word8 -> CSize -> Ptr CInt -> CSize -> Ptr CSsize -> Ptr CSize -> Ptr CInt -> IO CInt
