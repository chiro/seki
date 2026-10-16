module Seki.Internal.Core.StackSetTest (tests) where

import Data.Foldable (toList)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.List (sortOn)
import Data.Maybe (isNothing)
import Seki.Internal.Core.StackSet
import Seki.Internal.Core.StackSetGen
import Test.QuickCheck (Fun, Gen, Property, applyFun, arbitrary, chooseInt, forAll, suchThat, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "Core StackSet"
    [ testGroup
        "StackSet"
        [ localOption (QuickCheckTests 1000) $
            testGroup
              "invariants"
              [ testProperty "invariant" (invariant :: T -> Bool)
              , testProperty "empty_I" propEmptyI
              , testProperty "view_I" $ \n (x :: T) -> invariant (view n x)
              , testProperty "greedyView_I" $ \n (x :: T) -> invariant (greedyView n x)
              , testProperty "focusUp_I" $ \(SizedPositive n) (x :: T) -> invariant (applyN (Just n) focusUp x)
              , testProperty "insertUp_I" $ \n (x :: T) -> invariant (insertUp n x)
              ]
        , testProperty "empty" $ \(EmptyStackSet x) -> all (isNothing . stack) (workspaces x)
        , testProperty "empty_current" $ \(EmptyStackSet x) -> take 1 (tags x) === [currentTag x]
        , testProperty "member_empty" $ \i (EmptyStackSet x) -> not (member i x)
        , testProperty "member_peek" $ \(x :: T) -> maybe True (`member` x) (peek x)
        ]
    , testGroup
        "GreedyView"
        [ testProperty "greedyView_current" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> currentTag (greedyView n x) === n
        , testProperty "greedyView_current_id" $ \(x :: T) ->
            forAll (arbitrary `suchThat` (not . (`tagMember` x))) $ \n ->
              currentTag (greedyView n x) === currentTag x
        , testProperty "greedyView_local" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> byTag (greedyView n x) === byTag x
        , testProperty "greedyView_idem" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> greedyView n (greedyView n x) === greedyView n x
        , testCase "pulls a visible workspace onto the current output" $
            let x = attach 2 () "ab"
             in map (\sc -> (tag (workspace sc), output sc)) (screens (greedyView 'b' x))
                  @?= [('b', OutputId 1), ('a', OutputId 2)]
        , testProperty "greedyView_reversible" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> normal (greedyView (currentTag x) (greedyView n x)) === normal x
        ]
    , testGroup
        "View"
        [ testProperty "view_current" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> currentTag (view n x) === n
        , testProperty "view_local" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> byTag (view n x) === byTag x
        , testProperty "view_idem" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> view n (view n x) === view n x
        , testProperty "view_reversible" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> normal (view (currentTag x) (view n x)) === normal x
        ]
    , testGroup
        "Stack"
        [ testProperty "differentiate" propDifferentiate
        , testProperty "Foldable toList is integrate" propToListIntegrate
        , testProperty "Foldable methods agree with toList" propFoldableConsistent
        , testProperty "Traversable identity" propTraverseIdentity
        , testProperty "Traversable composition" propTraverseComposition
        , testProperty "Traversable effects run in integrate order" propTraverseOrder
        ]
    ]

byTag :: T -> [Workspace Int Int]
byTag = sortOn tag . workspaces

propEmptyI :: SizedPositive -> Int -> Property
propEmptyI (SizedPositive n) l = forAll (chooseInt (1, n)) $ \m -> invariant (attach m l [0 .. n - 1])

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
