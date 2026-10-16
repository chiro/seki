{- | The Full layout, with the properties of xmonad 0.18.1's
@tests/Properties/Layout/Full.hs@ under their xmonad names.
-}
module Seki.Internal.Layout.FullTest (tests) where

import Data.Maybe (fromJust, isJust, isNothing)
import Seki.Internal.Core.GeometryGen (genRect)
import Seki.Internal.Core.Layout (IncMasterN (..), LayoutClass (..), SomeMessage (..))
import Seki.Internal.Core.StackSet (Screen (..), StackSet (..), Workspace (..), peek)
import Seki.Internal.Core.StackSetGen (T)
import Seki.Internal.Layout.Full (Full (..))
import Test.QuickCheck (Gen, NonNegative (..), Property, arbitrary, forAll, suchThat, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Full layout"
    [ testProperty "purelayout_full" prop_purelayout_full
    , testProperty "sendmsg_full" prop_sendmsg_full
    , testProperty "desc_full" prop_desc_full
    ]

prop_purelayout_full :: Property
prop_purelayout_full =
  forAll genRect $ \rect ->
    forAll ((arbitrary :: Gen T) `suchThat` (isJust . peek)) $ \x ->
      let st = fromJust (stack (workspace (current x)))
       in pureLayout Full rect st === [(fromJust (peek x), rect)]

prop_sendmsg_full :: NonNegative Int -> Bool
prop_sendmsg_full (NonNegative k) = isNothing (pureMessage Full (SomeMessage (IncMasterN k)))

prop_desc_full :: Property
prop_desc_full = description Full === show Full
