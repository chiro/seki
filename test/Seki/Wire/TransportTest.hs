{-# LANGUAGE TypeApplications #-}

module Seki.Wire.TransportTest (tests) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.Either (isLeft)
import Foreign.C.Types (CInt)
import Seki.Wire.Transport (
  ConnectionDiscoveryError (..),
  ConnectionTarget (..),
  OwnedFdError (..),
  adoptFd,
  closeOwnedFd,
  maxUnixSocketPathBytes,
  releaseOwnedFd,
  resolveConnectionTarget,
  withOwnedFd,
 )
import System.Posix.IO (closeFd, createPipe)
import System.Posix.IO.ByteString (fdRead)
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Wayland transport foundation"
    [ testCase "inherited socket takes precedence" testInheritedSocket
    , testCase "rejects malformed inherited sockets" testMalformedInheritedSocket
    , testCase "uses the default display name" testDefaultDisplay
    , testCase "resolves a named display beneath the runtime directory" testNamedDisplay
    , testCase "uses an absolute display without a runtime directory" testAbsoluteDisplay
    , testCase "rejects missing or relative runtime directories" testInvalidRuntimeDirectory
    , testCase "rejects empty and overlong display paths" testInvalidDisplay
    , testCase "borrows and releases descriptor ownership once" testReleaseOwnedFd
    , testCase "closes descriptor ownership idempotently" testCloseOwnedFd
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
  owned <- adoptFd readDescriptor
  borrowed <- withOwnedFd owned pure
  assertEqual "borrowed descriptor" (Right readDescriptor) borrowed
  released <- releaseOwnedFd owned
  assertEqual "released descriptor" (Right readDescriptor) released
  releasedAgain <- releaseOwnedFd owned
  assertEqual "released only once" (Left OwnedFdAlreadyReleased) releasedAgain
  closed <- closeOwnedFd owned
  assertBool "released descriptor is not closed by the wrapper" (not closed)
  closeFd readDescriptor

testCloseOwnedFd :: IO ()
testCloseOwnedFd = withPipe $ \readDescriptor _ -> do
  owned <- adoptFd readDescriptor
  closed <- closeOwnedFd owned
  assertBool "first close owns the descriptor" closed
  closedAgain <- closeOwnedFd owned
  assertBool "second close is a no-op" (not closedAgain)
  borrowed <- withOwnedFd owned pure
  assertEqual "closed descriptor cannot be borrowed" (Left OwnedFdAlreadyReleased) borrowed
  readResult <- try @IOException (fdRead readDescriptor 1)
  assertBool "kernel descriptor is closed" (isLeft readResult)

withPipe :: (Fd -> Fd -> IO value) -> IO value
withPipe action = bracket createPipe closePipe (uncurry action)

closePipe :: (Fd, Fd) -> IO ()
closePipe (readDescriptor, writeDescriptor) = do
  closeIgnoringErrors readDescriptor
  closeIgnoringErrors writeDescriptor

closeIgnoringErrors :: Fd -> IO ()
closeIgnoringErrors descriptor = void (try @IOException (closeFd descriptor))
