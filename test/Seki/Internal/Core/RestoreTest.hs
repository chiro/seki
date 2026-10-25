module Seki.Internal.Core.RestoreTest (tests) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Seki.Internal.Core.Layout (CLR (..), ChangeLayout (..), Choose (..), IncMasterN (..), LayoutClass (..), Mirror (..), ModifiedLayout, Resize (..), SomeMessage (..), Tolerant, tolerant)
import Seki.Internal.Core.StackSetGen (DefaultLayout)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.QuickCheck (Gen, chooseInt, elements, forAll, listOf, oneof, (===))
import Test.Tasty (TestTree, testGroup)
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
    ]

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
