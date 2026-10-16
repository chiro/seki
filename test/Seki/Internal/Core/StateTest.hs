module Seki.Internal.Core.StateTest (tests) where

import Data.Either (isRight)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Seki.Internal.Core.StackSet (Workspace (..))
import Seki.Internal.Core.State (
  CoreState (..),
  HeadlessWorkspaces (..),
  WorkspaceTagError (..),
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
    ]

propAcceptsUnique :: [Int] -> Property
propAcceptsUnique ts = isRight (newCoreState () ts) === (not (null ts) && nub ts == ts)
