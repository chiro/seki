module Seki.Internal.Core.LayoutTest (tests) where

import Seki.Internal.Core.GeometryGen (rectOf)
import Seki.Internal.Core.Layout
import Seki.Internal.Core.StackSet (Screen (..), Stack (..), StackSet (..), WindowId (..), Workspace (..), mapLayout, workspaces)
import Seki.Internal.Core.StackSetGen (T)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.QuickCheck (Property, conjoin, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

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
    , testGroup
        "Mirror"
        [ testCase "passes messages to the inner layout and rewraps it" testMirrorMessage
        , testCase "ignores what the inner layout ignores" testMirrorUnhandled
        ]
    , testGroup
        "Choose"
        [ testCase "NextLayout wraps around after the last layout" testWraparound
        , testCase "FirstLayout returns to the first layout" testFirstLayout
        , testCase "FirstLayout on the first layout changes nothing" testFirstLayoutAtFirst
        , testCase "left-nested choices cycle in order" testLeftNested
        , testCase "lays out and describes with the selected layout" testSelectedLayout
        , testCase "a message reaches only the selected layout" testRouting
        , testCase "switching sides hides the layout switched away from" testHide
        , testCase "nested choices hide and select as xmonad does" testNestedHide
        , testCase "an unhandled message changes nothing" testUnhandled
        ]
    , testGroup
        "messageCurrentLayout"
        [ testProperty "changes only the current workspace's layout" propMessageCurrent
        , testProperty "leaves the StackSet unchanged for an unhandled message" propMessageUnhandled
        ]
    ]

-- | A layout that counts the 'Shrink' messages it receives.
newtype Counter = Counter Int
  deriving (Eq, Show)

instance LayoutClass Counter where
  pureMessage (Counter n) m
    | Just Shrink <- fromMessage m = Just (Counter (n + 1))
    | otherwise = Nothing

propMessageCurrent :: T -> Property
propMessageCurrent x =
  let s = mapLayout Counter x
      s' = messageCurrentLayout (SomeMessage Shrink) s
      Counter n = layout (workspace (current s))
   in conjoin
        [ layout (workspace (current s')) === Counter (n + 1)
        , drop 1 (workspaces s') === drop 1 (workspaces s)
        , mapLayout (const ()) s' === mapLayout (const ()) s
        ]

propMessageUnhandled :: T -> Property
propMessageUnhandled x =
  let s = mapLayout Counter x
   in messageCurrentLayout (SomeMessage (IncMasterN 1)) s === s

{- | A layout that answers 'Hide', 'Shrink', and 'Expand' by recording them,
and ignores every other message.
-}
data Probe = Probe String [String]
  deriving (Eq, Show)

instance LayoutClass Probe where
  pureMessage (Probe name seen) m
    | Just Hide <- fromMessage m = record "Hide"
    | Just Shrink <- fromMessage m = record "Shrink"
    | Just Expand <- fromMessage m = record "Expand"
    | otherwise = Nothing
   where
    record s = Just (Probe name (seen ++ [s]))
  description (Probe name _) = name

probe :: String -> Probe
probe name = Probe name []

send :: (LayoutClass l, Message m) => m -> l -> Maybe l
send m l = pureMessage l (SomeMessage m)

-- | The descriptions along a run of messages, or 'Nothing' if one is unhandled.
walk :: (LayoutClass l) => [ChangeLayout] -> l -> Maybe [String]
walk [] l = Just [description l]
walk (m : ms) l = (description l :) <$> (send m l >>= walk ms)

tall :: Tall
tall = Tall 1 (3 / 100) (1 / 2)

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

testMirrorMessage :: Assertion
testMirrorMessage =
  assertEqual
    "layout"
    (Just (Mirror (Tall 1 (1 / 10) (2 / 5))))
    (send Shrink (Mirror (Tall 1 (1 / 10) (1 / 2))))

testMirrorUnhandled :: Assertion
testMirrorUnhandled = assertEqual "layout" Nothing (send Shrink (Mirror Full))

testWraparound :: Assertion
testWraparound =
  assertEqual
    "descriptions"
    (Just ["Tall", "Mirror Tall", "Full", "Tall", "Mirror Tall"])
    (walk (replicate 4 NextLayout) (tall ||| Mirror tall ||| Full))

testFirstLayout :: Assertion
testFirstLayout = do
  let layouts = tall ||| Mirror tall ||| Full
  assertEqual "from the second" (Just ["Tall", "Mirror Tall", "Tall"]) (walk [NextLayout, FirstLayout] layouts)
  assertEqual "from the last" (Just ["Tall", "Mirror Tall", "Full", "Tall"]) (walk [NextLayout, NextLayout, FirstLayout] layouts)

testFirstLayoutAtFirst :: Assertion
testFirstLayoutAtFirst = assertEqual "layout" Nothing (send FirstLayout (tall ||| Mirror tall ||| Full))

testLeftNested :: Assertion
testLeftNested =
  assertEqual
    "descriptions"
    (Just ["Tall", "Full", "Mirror Tall", "Tall"])
    (walk (replicate 3 NextLayout) ((tall ||| Full) ||| Mirror tall))

testSelectedLayout :: Assertion
testSelectedLayout = do
  rect <- rectOf 0 0 100 100
  let s = Stack (WindowId 2) [WindowId 1] [WindowId 3]
      full = send NextLayout (tall ||| Full)
  assertEqual "description" (Just "Full") (description <$> full)
  assertEqual "placements" (Just [(WindowId 2, rect)]) ((\l -> pureLayout l rect s) <$> full)

testRouting :: Assertion
testRouting = do
  assertEqual
    "left selected"
    (Just (Choose CL (Probe "a" ["Shrink"]) (probe "b")))
    (send Shrink (probe "a" ||| probe "b"))
  assertEqual
    "right selected"
    (Just (Choose CR (probe "a") (Probe "b" ["Expand"])))
    (send Expand (Choose CR (probe "a") (probe "b")))

testHide :: Assertion
testHide = do
  let next = send NextLayout (probe "a" ||| probe "b")
  assertEqual "to the right" (Just (Choose CR (Probe "a" ["Hide"]) (probe "b"))) next
  assertEqual
    "wrapping to the left"
    (Just (Choose CL (Probe "a" ["Hide"]) (Probe "b" ["Hide"])))
    (next >>= send NextLayout)
  assertEqual
    "first layout"
    (Just (Choose CL (probe "a") (Probe "b" ["Hide"])))
    (send FirstLayout (Choose CR (probe "a") (probe "b")))

testNestedHide :: Assertion
testNestedHide = do
  let start = probe "a" ||| probe "b" ||| probe "c"
      steps = take 5 (iterate (>>= send NextLayout) (Just start))
  assertEqual
    "layouts"
    [ Just start
    , Just (Choose CR (Probe "a" ["Hide"]) (Choose CL (probe "b") (probe "c")))
    , Just (Choose CR (Probe "a" ["Hide"]) (Choose CR (Probe "b" ["Hide"]) (probe "c")))
    , Just (Choose CL (Probe "a" ["Hide"]) (Choose CR (Probe "b" ["Hide"]) (Probe "c" ["Hide"])))
    , Just (Choose CR (Probe "a" ["Hide", "Hide"]) (Choose CL (Probe "b" ["Hide"]) (Probe "c" ["Hide", "Hide"])))
    ]
    steps

testUnhandled :: Assertion
testUnhandled = do
  assertEqual "probes" Nothing (send (IncMasterN 1) (probe "a" ||| probe "b"))
  assertEqual "full" Nothing (send Shrink (Full ||| Full))
