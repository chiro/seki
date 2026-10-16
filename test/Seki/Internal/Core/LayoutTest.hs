module Seki.Internal.Core.LayoutTest (tests) where

import Seki.Internal.Core.GeometryGen (rectOf)
import Seki.Internal.Core.Layout
import Seki.Internal.Core.StackSet (Stack (..), WindowId (..), Workspace (..))
import Seki.Internal.Layout.Full (Full (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Core layout"
    [ testGroup
        "messages"
        [ testCase "fromMessage recovers the sent type" testFromMessage
        ]
    , testGroup
        "runLayout"
        [ testCase "places nothing on an empty workspace" testRunLayoutEmpty
        , testCase "lays out a workspace's stack" testRunLayoutStack
        ]
    ]

testFromMessage :: Assertion
testFromMessage = do
  let m = SomeMessage NextLayout
  assertEqual "same type" (Just NextLayout) (fromMessage m)
  assertEqual "other type" Nothing (fromMessage m :: Maybe LayoutMessages)

testRunLayoutEmpty :: Assertion
testRunLayoutEmpty = do
  rect <- rectOf 0 0 100 100
  assertEqual "placements" [] (runLayout (Workspace 'a' Full Nothing) rect)

testRunLayoutStack :: Assertion
testRunLayoutStack = do
  rect <- rectOf 0 0 100 100
  let s = Stack (WindowId 2) [WindowId 1] [WindowId 3]
  assertEqual "placements" [(WindowId 2, rect)] (runLayout (Workspace 'a' Full (Just s)) rect)
