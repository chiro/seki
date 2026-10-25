module Seki.Internal.Core.RestoreTest (tests) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Seki.Internal.Core.Layout (CLR (..), ChangeLayout (..), Choose (..), IncMasterN (..), LayoutClass (..), Mirror (..), ModifiedLayout, Resize (..), SomeMessage (..), Tolerant, tolerant)
import Seki.Internal.Core.Restore
import Seki.Internal.Core.StackSet (Stack (..))
import Seki.Internal.Core.StackSetGen (DefaultLayout)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.QuickCheck (Gen, arbitrary, chooseInt, elements, forAll, listOf, oneof, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Core restore"
    [ testProperty "a layout reached by messages reads back from its show" $
        forAll (messaged defaultLayout) $
          \l -> read (show l) === l
    , testProperty "a tolerant layout reached by messages reads back from its show" $
        forAll (messaged (tolerant defaultLayout)) $
          \l -> read (show l) === (l :: ModifiedLayout Tolerant DefaultLayout)
    , testProperty "a saved state decodes from its encoding" $ forAll restoreStates $ \s -> decodeRestoreState (encodeRestoreState s) === Right s
    , testCase "an encoding starts with the version header" $
        takeWhile (/= '\n') (encodeRestoreState emptyState) @?= "seki-restore 1"
    , testCase "an unknown version is refused by name" $
        decodeInt ("seki-restore 99\n" <> dropWhile (/= 'R') (encodeRestoreState emptyState)) @?= Left (RestoreUnknownVersion "99")
    , testCase "a file without the header is unreadable" $
        decodeInt (show emptyState) @?= Left RestoreUnreadable
    , testCase "a known version with an unreadable body is unreadable" $
        decodeInt "seki-restore 1\nnot a state" @?= Left RestoreUnreadable
    ]
 where
  emptyState = RestoreState [] [] Set.empty Set.empty :: RestoreState Int
  decodeInt = decodeRestoreState :: String -> Either RestoreError (RestoreState Int)

-- | Any saved state, including keys and tags no live session would produce.
restoreStates :: Gen (RestoreState Int)
restoreStates = RestoreState <$> listOf workspace <*> listOf ((,) <$> key <*> saved) <*> (Set.fromList <$> listOf key) <*> (Set.fromList <$> listOf command)
 where
  workspace = SavedWorkspace <$> arbitrary <*> oneof [pure Nothing, Just . OutputKey <$> arbitrary] <*> elements ["Full", "Tall 1 (3 % 100) (1 % 2)", "garbage"] <*> oneof [pure Nothing, Just <$> stack]
  stack = Stack <$> key <*> listOf key <*> listOf key
  saved = oneof [pure SavedClientSized, SavedFloatAt <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]
  command = elements ["foot", "waybar -c \"x y\"", "printf '%s\\n' \12354"]

key :: Gen WindowKey
key = WindowKey . Text.pack <$> elements ["c68415ce", "9b107380", "", "with space", "\"quoted\"", "\12354"]

defaultLayout :: DefaultLayout
defaultLayout = Choose CL tall (Choose CL (Mirror tall) Full)
 where
  tall = Tall 1 (3 / 100) (1 / 2)

messaged :: (LayoutClass l) => l -> Gen l
messaged l0 = foldl' (\l m -> fromMaybe l (pureMessage l m)) l0 <$> listOf message
 where
  message =
    oneof
      [ elements [SomeMessage NextLayout, SomeMessage FirstLayout, SomeMessage Shrink, SomeMessage Expand]
      , SomeMessage . IncMasterN <$> chooseInt (-2, 2)
      ]
