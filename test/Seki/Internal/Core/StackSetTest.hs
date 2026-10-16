module Seki.Internal.Core.StackSetTest (tests) where

import Data.Foldable (toList)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isNothing)
import Seki.Internal.Core.StackSet (Stack (..), differentiate, integrate)
import Test.QuickCheck (Fun, Gen, Property, applyFun, arbitrary, forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Core StackSet"
    [ testGroup
        "Stack"
        [ testProperty "differentiate" propDifferentiate
        , testProperty "Foldable toList is integrate" propToListIntegrate
        , testProperty "Foldable methods agree with toList" propFoldableConsistent
        , testProperty "Traversable identity" propTraverseIdentity
        , testProperty "Traversable composition" propTraverseComposition
        , testProperty "Traversable effects run in integrate order" propTraverseOrder
        ]
    ]

genStack :: Gen (Stack Int)
genStack = Stack <$> arbitrary <*> arbitrary <*> arbitrary

propDifferentiate :: [Int] -> Property
propDifferentiate xs = case xs of
  [] -> isNothing (differentiate xs) === True
  x : rest -> differentiate xs === Just (Stack x [] rest)

propToListIntegrate :: Property
propToListIntegrate = forAll genStack $ \s -> toList s === integrate s

propFoldableConsistent :: Property
propFoldableConsistent = forAll genStack $ \s ->
  (foldMap pure s, foldl (flip (:)) [] s, length s, sum s)
    === (integrate s, reverse (integrate s), length (integrate s), sum (integrate s))

propTraverseIdentity :: Property
propTraverseIdentity = forAll genStack $ \s -> traverse Identity s === Identity s

propTraverseComposition :: Fun Int (Maybe Int) -> Fun Int ([Int], Int) -> Property
propTraverseComposition f g = forAll genStack $ \s ->
  traverse (Compose . fmap (applyFun g) . applyFun f) s
    === Compose (fmap (traverse (applyFun g)) (traverse (applyFun f) s))

propTraverseOrder :: Property
propTraverseOrder = forAll genStack $ \s -> traverse (\a -> ([a], a)) s === (integrate s, s)
