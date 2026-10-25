{-# LANGUAGE TypeApplications #-}

module Seki.Wire.TransportTest (tests) where

import Control.Exception (IOException, bracket, onException, try)
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Foreign.C.Types (CInt)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  Socket,
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultProtocol,
  listen,
  socket,
  socketPair,
  withFdSocket,
 )
import Network.Socket.ByteString (recv)
import Seki.Wire.Transport (
  ConnectionDiscoveryError (..),
  ConnectionTarget (..),
  OwnedFdError (..),
  ReceivedChunk (..),
  Transport,
  TransportError (..),
  adoptFd,
  closeOwnedFd,
  closeTransport,
  maxUnixSocketPathBytes,
  openTransport,
  receiveChunk,
  releaseOwnedFd,
  resolveConnectionTarget,
  sendMessage,
  withOwnedFd,
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO (closeFd, createPipe, dup)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Types (Fd (..))
import System.Process (spawnProcess, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland Unix transport"
    [ testCase "inherited socket takes precedence" testInheritedSocket
    , testCase "rejects malformed inherited sockets" testMalformedInheritedSocket
    , testCase "uses the default display name" testDefaultDisplay
    , testCase "resolves a named display beneath the runtime directory" testNamedDisplay
    , testCase "uses an absolute display without a runtime directory" testAbsoluteDisplay
    , testCase "rejects missing or relative runtime directories" testInvalidRuntimeDirectory
    , testCase "rejects empty and overlong display paths" testInvalidDisplay
    , testCase "borrows and releases descriptor ownership once" testReleaseOwnedFd
    , testCase "closes descriptor ownership idempotently" testCloseOwnedFd
    , testCase "opens inherited Unix stream sockets" testOpenInheritedSocket
    , testCase "connects to Unix socket paths" testOpenSocketPath
    , testCase "a child spawned after connecting does not keep the connection open" testSocketPathNotInherited
    , testCase "sends and receives stream bytes" testByteTransfer
    , testCase "transfers multiple descriptors in order" testDescriptorTransfer
    , testCase "consumes descriptors rejected before sending" testRejectedDescriptorSend
    ]

testInheritedSocket :: IO ()
testInheritedSocket =
  assertEqual
    "WAYLAND_SOCKET wins"
    (Right (InheritedSocket (Fd 12)))
    (resolveConnectionTarget (Just "12") (Just "ignored") Nothing)

testMalformedInheritedSocket :: IO ()
testMalformedInheritedSocket = do
  assertEqual
    "empty"
    (Left WaylandSocketEmpty)
    (resolveConnectionTarget (Just "") Nothing Nothing)
  assertEqual
    "negative"
    (Left (WaylandSocketNotDecimal "-1"))
    (resolveConnectionTarget (Just "-1") Nothing Nothing)
  assertEqual
    "non-decimal"
    (Left (WaylandSocketNotDecimal "2x"))
    (resolveConnectionTarget (Just "2x") Nothing Nothing)
  let tooLarge = toInteger (maxBound :: CInt) + 1
  assertEqual
    "outside CInt"
    (Left (WaylandSocketOutOfRange tooLarge))
    (resolveConnectionTarget (Just (show tooLarge)) Nothing Nothing)

testDefaultDisplay :: IO ()
testDefaultDisplay =
  assertEqual
    "default"
    (Right (UnixSocketPath "/run/user/1000/wayland-0"))
    (resolveConnectionTarget Nothing Nothing (Just "/run/user/1000"))

testNamedDisplay :: IO ()
testNamedDisplay =
  assertEqual
    "named display"
    (Right (UnixSocketPath "/run/user/1000/wayland-2"))
    (resolveConnectionTarget Nothing (Just "wayland-2") (Just "/run/user/1000"))

testAbsoluteDisplay :: IO ()
testAbsoluteDisplay =
  assertEqual
    "absolute display"
    (Right (UnixSocketPath "/tmp/nested-river"))
    (resolveConnectionTarget Nothing (Just "/tmp/nested-river") Nothing)

testInvalidRuntimeDirectory :: IO ()
testInvalidRuntimeDirectory = do
  assertEqual
    "missing"
    (Left XdgRuntimeDirMissing)
    (resolveConnectionTarget Nothing Nothing Nothing)
  assertEqual
    "relative"
    (Left (XdgRuntimeDirNotAbsolute "run/user/1000"))
    (resolveConnectionTarget Nothing Nothing (Just "run/user/1000"))

testInvalidDisplay :: IO ()
testInvalidDisplay = do
  assertEqual
    "empty display"
    (Left WaylandDisplayEmpty)
    (resolveConnectionTarget Nothing (Just "") (Just "/run/user/1000"))
  let path = "/" <> replicate maxUnixSocketPathBytes 'a'
      pathBytes = length path
  assertEqual
    "overlong absolute path"
    (Left (UnixSocketPathTooLong pathBytes))
    (resolveConnectionTarget Nothing (Just path) Nothing)

testReleaseOwnedFd :: IO ()
testReleaseOwnedFd = withPipe $ \readDescriptor _ -> do
  ownedDescriptor <- dup readDescriptor
  owned <- adoptFd ownedDescriptor
  borrowed <- withOwnedFd owned pure
  assertEqual "borrowed descriptor" (Right ownedDescriptor) borrowed
  released <- releaseOwnedFd owned
  assertEqual "released descriptor" (Right ownedDescriptor) released
  releasedAgain <- releaseOwnedFd owned
  assertEqual "released only once" (Left OwnedFdAlreadyReleased) releasedAgain
  closed <- closeOwnedFd owned
  assertBool "released descriptor is not closed by the wrapper" (not closed)
  closeFd ownedDescriptor

testCloseOwnedFd :: IO ()
testCloseOwnedFd = withPipe $ \readDescriptor _ -> do
  ownedDescriptor <- dup readDescriptor
  owned <- adoptFd ownedDescriptor
  closed <- closeOwnedFd owned
  assertBool "first close owns the descriptor" closed
  closedAgain <- closeOwnedFd owned
  assertBool "second close is a no-op" (not closedAgain)
  borrowed <- withOwnedFd owned pure
  assertEqual "closed descriptor cannot be borrowed" (Left OwnedFdAlreadyReleased) borrowed
  readResult <- try @IOException (fdRead ownedDescriptor 1)
  assertBool "kernel descriptor is closed" (isLeft readResult)

testOpenInheritedSocket :: IO ()
testOpenInheritedSocket =
  withTransportPair $ \_ _ -> pure ()

testOpenSocketPath :: IO ()
testOpenSocketPath =
  withSystemTempDirectory "seki-transport" $ \temporaryDirectory ->
    bracket (socket AF_UNIX Stream defaultProtocol) close $ \listener -> do
      let socketPath = temporaryDirectory <> "/wayland-test"
      bind listener (SockAddrUnix socketPath)
      listen listener 1
      bracket
        (expectTransport =<< openTransport (UnixSocketPath socketPath))
        closeTransport
        (\_ -> bracket (accept listener) (close . fst) (const (pure ())))

testSocketPathNotInherited :: IO ()
testSocketPathNotInherited =
  withSystemTempDirectory "seki-transport" $ \temporaryDirectory ->
    bracket (socket AF_UNIX Stream defaultProtocol) close $ \listener -> do
      let socketPath = temporaryDirectory <> "/wayland-test"
      bind listener (SockAddrUnix socketPath)
      listen listener 1
      transport <- expectTransport =<< openTransport (UnixSocketPath socketPath)
      bracket (accept listener) (close . fst) $ \(server, _) ->
        bracket (spawnProcess "sleep" ["30"]) (\child -> terminateProcess child >> void (waitForProcess child)) $ \_ -> do
          closeTransport transport
          received <- timeout 2000000 (recv server 1)
          assertEqual "server reads end of stream" (Just ByteString.empty) received

testByteTransfer :: IO ()
testByteTransfer =
  withTransportPair $ \sender receiver -> do
    let payload = ByteString.pack [1, 2, 3, 4]
    assertEqual "send" (Right ()) =<< sendMessage sender payload []
    received <- receiveChunk receiver 1024
    case received of
      Right (Just chunk) -> do
        assertEqual "payload" payload (receivedBytes chunk)
        assertEqual "no descriptors" 0 (length (receivedFds chunk))
      other -> fail ("unexpected receive result: " <> showReceiveResult other)

testDescriptorTransfer :: IO ()
testDescriptorTransfer =
  withTwoPipes $ \firstRead firstWrite secondRead secondWrite ->
    withTransportPair $ \sender receiver -> do
      void (fdWrite firstWrite (ByteString.singleton 0x11))
      void (fdWrite secondWrite (ByteString.singleton 0x22))
      firstOwned <- dup firstRead >>= adoptFd
      secondOwned <- dup secondRead >>= adoptFd
      assertEqual
        "send descriptors"
        (Right ())
        =<< sendMessage sender (ByteString.singleton 0xaa) [firstOwned, secondOwned]
      assertEqual
        "sender relinquished first descriptor"
        (Left OwnedFdAlreadyReleased)
        =<< withOwnedFd firstOwned pure
      received <- receiveChunk receiver 1024
      case received of
        Right (Just chunk) -> do
          assertEqual "payload" (ByteString.singleton 0xaa) (receivedBytes chunk)
          case receivedFds chunk of
            [receivedFirst, receivedSecond] -> do
              firstValue <- withOwnedFd receivedFirst (`fdRead` 1)
              secondValue <- withOwnedFd receivedSecond (`fdRead` 1)
              assertEqual "first descriptor" (Right (ByteString.singleton 0x11)) firstValue
              assertEqual "second descriptor" (Right (ByteString.singleton 0x22)) secondValue
              void (closeOwnedFd receivedFirst)
              void (closeOwnedFd receivedSecond)
            descriptors -> fail ("unexpected descriptor count: " <> show (length descriptors))
        other -> fail ("unexpected receive result: " <> showReceiveResult other)

testRejectedDescriptorSend :: IO ()
testRejectedDescriptorSend =
  withPipe $ \readDescriptor _ ->
    withTransportPair $ \sender _ -> do
      owned <- dup readDescriptor >>= adoptFd
      assertEqual
        "empty payload"
        (Left TransportEmptyPayloadWithDescriptors)
        =<< sendMessage sender ByteString.empty [owned]
      assertEqual
        "ownership consumed"
        (Left OwnedFdAlreadyReleased)
        =<< withOwnedFd owned pure

withPipe :: (Fd -> Fd -> IO value) -> IO value
withPipe action = bracket createPipe closePipe (uncurry action)

withTwoPipes :: (Fd -> Fd -> Fd -> Fd -> IO value) -> IO value
withTwoPipes action =
  withPipe $ \firstRead firstWrite ->
    withPipe $ \secondRead secondWrite ->
      action firstRead firstWrite secondRead secondWrite

withTransportPair :: (Transport -> Transport -> IO value) -> IO value
withTransportPair action =
  bracket
    (socketPair AF_UNIX Stream defaultProtocol)
    closeSocketPair
    (\sockets -> bracket (openTransportPair sockets) closeTransportPair (uncurry action))

openTransportPair :: (Socket, Socket) -> IO (Transport, Transport)
openTransportPair (firstSocket, secondSocket) = do
  firstDescriptor <- duplicateSocket firstSocket
  secondDescriptor <- duplicateSocket secondSocket `onException` closeFd firstDescriptor
  firstTransport <- expectTransport =<< openTransport (InheritedSocket firstDescriptor)
  secondTransport <-
    (expectTransport =<< openTransport (InheritedSocket secondDescriptor))
      `onException` closeTransport firstTransport
  pure (firstTransport, secondTransport)

duplicateSocket :: Socket -> IO Fd
duplicateSocket connection = withFdSocket connection (dup . Fd)

expectTransport :: Either TransportError Transport -> IO Transport
expectTransport result = case result of
  Left errorValue -> fail ("unexpected transport-open error: " <> show errorValue)
  Right transport -> pure transport

closeSocketPair :: (Socket, Socket) -> IO ()
closeSocketPair (firstSocket, secondSocket) = do
  close firstSocket
  close secondSocket

closeTransportPair :: (Transport, Transport) -> IO ()
closeTransportPair (firstTransport, secondTransport) = do
  closeTransport firstTransport
  closeTransport secondTransport

showReceiveResult :: Either TransportError (Maybe ReceivedChunk) -> String
showReceiveResult result = case result of
  Left errorValue -> "error " <> show errorValue
  Right Nothing -> "EOF"
  Right (Just chunk) ->
    "chunk with "
      <> show (ByteString.length (receivedBytes chunk))
      <> " bytes and "
      <> show (length (receivedFds chunk))
      <> " descriptors"

closePipe :: (Fd, Fd) -> IO ()
closePipe (readDescriptor, writeDescriptor) = do
  closeIgnoringErrors readDescriptor
  closeIgnoringErrors writeDescriptor

closeIgnoringErrors :: Fd -> IO ()
closeIgnoringErrors descriptor = void (try @IOException (closeFd descriptor))
