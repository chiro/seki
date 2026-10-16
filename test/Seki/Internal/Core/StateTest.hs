module Seki.Internal.Core.StateTest (tests) where

import Data.Either (isRight)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Seki.Internal.Core.StackSet (OutputId (..), Screen (..), StackSet (..), Workspace (..))
import Seki.Internal.Core.State (
  CoreState (..),
  HeadlessWorkspaces (..),
  WorkspaceTagError (..),
  addOutput,
  newCoreState,
 )
import Test.QuickCheck (Property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Core state"
    [ testGroup
        "construction"
        [ testCase "rejects an empty tag list" $
            newCoreState () ([] :: [Int]) @?= Left NoWorkspaceTags
        , testCase "reports the first tag that repeats an earlier one" $
            newCoreState () "abcbca" @?= Left (DuplicateWorkspaceTag 'b')
        , testCase "starts headless on the first tag with the rest hidden" $
            newCoreState 'l' "abc"
              @?= Right
                ( HeadlessState
                    ( HeadlessWorkspaces
                        (Workspace 'a' 'l' Nothing)
                        [Workspace 'b' 'l' Nothing, Workspace 'c' 'l' Nothing]
                        Map.empty
                    )
                )
        , testProperty "accepts exactly the non-empty duplicate-free tag lists" propAcceptsUnique
        ]
    , testGroup
        "adding outputs"
        [ testCase "attaches the remembered workspace to the first output" $
            addOutput o1 initial @?= AttachedState (StackSet (Screen (ws 'a') o1) [] [ws 'b', ws 'c'] Map.empty) []
        , testCase "shows the first hidden workspace after the visible screens" $
            (addOutput o3 . addOutput o2 . addOutput o1) initial
              @?= AttachedState (StackSet (Screen (ws 'a') o1) [Screen (ws 'b') o2, Screen (ws 'c') o3] [] Map.empty) []
        , testCase "leaves outputs unassigned in arrival order when no workspace is hidden" $
            (addOutput o5 . addOutput o4) full
              @?= AttachedState (stackSetOf full) [o4, o5]
        , testCase "ignores an output that already shows a workspace" $
            addOutput o2 full @?= full
        , testCase "ignores an output that is already unassigned" $
            let pending = addOutput o4 full in addOutput o4 pending @?= pending
        ]
    ]

o1, o2, o3, o4, o5 :: OutputId
o1 = OutputId 1
o2 = OutputId 2
o3 = OutputId 3
o4 = OutputId 4
o5 = OutputId 5

ws :: Char -> Workspace Char ()
ws t = Workspace t () Nothing

initial :: CoreState Char ()
initial = either (error . show) id (newCoreState () "abc")

full :: CoreState Char ()
full = (addOutput o3 . addOutput o2 . addOutput o1) initial

stackSetOf :: CoreState Char () -> StackSet Char ()
stackSetOf (AttachedState ss _) = ss
stackSetOf HeadlessState{} = error "expected an attached state"

propAcceptsUnique :: [Int] -> Property
propAcceptsUnique ts = isRight (newCoreState () ts) === (not (null ts) && nub ts == ts)
