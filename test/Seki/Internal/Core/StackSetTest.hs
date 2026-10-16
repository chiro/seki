module Seki.Internal.Core.StackSetTest (tests) where

import Data.Foldable (toList)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.List (nub, sortOn)
import Data.List qualified as L
import Data.Maybe (isNothing, listToMaybe)
import Data.Word (Word32)
import Seki.Internal.Core.StackSet
import Seki.Internal.Core.StackSetGen
import Test.QuickCheck (Fun, Gen, NonNegative (..), Property, applyFun, arbitrary, chooseInt, elements, forAll, property, suchThat, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import Prelude hiding (filter)

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
              , testProperty "focusMaster_I" $ \(SizedPositive n) (x :: T) -> invariant (applyN (Just n) focusMaster x)
              , testProperty "focusDown_I" $ \(SizedPositive n) (x :: T) -> invariant (applyN (Just n) focusDown x)
              , testProperty "focus_I" propFocusI
              , testProperty "swap_master_I" $ \(x :: T) -> invariant (swapMaster x)
              , testProperty "swap_left_I" $ \(SizedPositive n) (x :: T) -> invariant (applyN (Just n) swapUp x)
              , testProperty "swap_right_I" $ \(SizedPositive n) (x :: T) -> invariant (applyN (Just n) swapDown x)
              , testProperty "insertUp_I" $ \n (x :: T) -> invariant (insertUp n x)
              , testProperty "shift_I" $ \(x :: T) -> forAll (arbitraryTag x) $ \n -> invariant (shift n x)
              , testProperty "shift_win_I" $ \nex@(NonEmptyWindowsStackSet x) ->
                  forAll ((,) <$> arbitraryWindow nex <*> arbitraryTag x) $ \(w, n) -> invariant (shiftWin n w x)
              , testProperty "delete_I" $ \(x :: T) -> invariant (maybe x (`delete` x) (peek x))
              ]
        , testProperty "empty" $ \(EmptyStackSet x) -> all (isNothing . stack) (workspaces x)
        , testProperty "empty_current" $ \(EmptyStackSet x) -> take 1 (tags x) === [currentTag x]
        , testProperty "member_empty" $ \i (EmptyStackSet x) -> not (member i x)
        , testProperty "member_peek" $ \(x :: T) -> maybe True (`member` x) (peek x)
        ]
    , testGroup
        "Delete"
        [ testProperty "delete" $ \(x :: T) -> maybe True (\i -> not (member i (delete i x))) (peek x)
        , testProperty "delete_insert" $ \(x :: T) ->
            let y = swapMaster x in maybe (property True) (\n -> insertUp n (delete n y) === y) (peek y)
        , testProperty "delete_local" $ \(x :: T) ->
            maybe (property True) (\i -> hiddenSpaces (delete i x) === hiddenSpaces x) (peek x)
        , testProperty "delete_focus" $
            forAll (arbitrary `suchThat` ((>= 2) . length . allWindows)) $ \x ->
              forAll (arbitraryWindow (NonEmptyWindowsStackSet x) `suchThat` ((/= peek x) . Just)) $ \w ->
                peek (delete w x) === peek x
        , testProperty "delete_focus_end" $
            forAll (genT `suchThat` ((>= 2) . length . index)) $ \x ->
              let w = last (index x)
                  y = focusWindow w x
               in peek (delete w y) === peek (focusUp y)
        , testProperty "delete_focus_not_end" $
            forAll (arbitrary `suchThat` focusNotLast) $ \x ->
              maybe (property False) (\n -> peek (delete n x) === peek (focusDown x)) (peek x)
        ]
    , testGroup
        "Insert"
        [ testProperty "insert_empty" $ \i (EmptyStackSet x) -> member i (insertUp i x)
        , testProperty "insert_idem" $ \i (x :: T) -> insertUp i (insertUp i x) === insertUp i x
        , testProperty "insert_duplicate" $ \nex@(NonEmptyWindowsStackSet x) ->
            forAll (arbitraryWindow nex) $ \w -> insertUp w x === x
        , testProperty "insert_local" $ \(x :: T) ->
            forAll (arbitrary `suchThat` (not . (`member` x))) $ \i -> hiddenSpaces (insertUp i x) === hiddenSpaces x
        , testProperty "insert_peek" $ \(EmptyStackSet x) (NonEmptyNubList is) ->
            peek (foldr insertUp x is) === listToMaybe is
        , testProperty "insert_delete" $ \(x :: T) ->
            let y = swapMaster x
             in forAll (arbitrary `suchThat` (not . (`member` x))) $ \n -> delete n (insertUp n y) === y
        , testProperty "size_insert" $ \is (EmptyStackSet x) ->
            length (index (foldr insertUp x (nub is))) === length (nub is)
        ]
    , testGroup
        "Focus"
        [ testProperty "focus_left_master" $ \(SizedPositive n) (x :: T) -> index (applyN (Just n) focusUp x) === index x
        , testProperty "focus_right_master" $ \(SizedPositive n) (x :: T) -> index (applyN (Just n) focusDown x) === index x
        , testProperty "focus_master_master" $ \(SizedPositive n) (x :: T) -> index (applyN (Just n) focusMaster x) === index x
        , testProperty "focusWindow_master" $ \(NonNegative n) (x :: T) ->
            withCurrentWindow n x $ \w -> index (focusWindow w x) === index x
        , testProperty "focus_left" $ \(x :: T) -> focusUp (focusDown x) === x
        , testProperty "focus_right" $ \(x :: T) -> focusDown (focusUp x) === x
        , testProperty "focusMaster focuses the master window" $ \(x :: T) ->
            peek (focusMaster x) === listToMaybe (index x)
        , testProperty "focusMaster_idem" $ \(x :: T) -> focusMaster x === focusMaster (focusMaster x)
        , testProperty "focusWindow_works" $ \(NonNegative n) (x :: T) ->
            withCurrentWindow n x $ \w -> peek (focusWindow w x) === Just w
        , testProperty "focus_all_l" $ \(x :: T) -> foldr (const focusUp) x (index x) === x
        , testProperty "focus_all_r" $ \(x :: T) -> foldr (const focusDown) x (index x) === x
        , testProperty "focus_down_local" $ \(x :: T) -> hiddenSpaces (focusDown x) === hiddenSpaces x
        , testProperty "focus_up_local" $ \(x :: T) -> hiddenSpaces (focusUp x) === hiddenSpaces x
        , testProperty "focus_master_local" $ \(x :: T) -> hiddenSpaces (focusMaster x) === hiddenSpaces x
        , testProperty "focusWindow_local" $ \(NonNegative n) (x :: T) ->
            withCurrentWindow n x $ \w -> hiddenSpaces (focusWindow w x) === hiddenSpaces x
        , testProperty "focusWindow_identity" $ \(x :: T) ->
            forAll (arbitrary `suchThat` (not . (`member` x))) $ \w -> focusWindow w x === x
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
        "Swap"
        [ testProperty "swap_left" $ \(x :: T) -> swapUp (swapDown x) === x
        , testProperty "swap_right" $ \(x :: T) -> swapDown (swapUp x) === x
        , testProperty "swap_master_focus" $ \(x :: T) -> peek (swapMaster x) === peek x
        , testProperty "swap_left_focus" $ \(x :: T) -> peek (swapUp x) === peek x
        , testProperty "swap_right_focus" $ \(x :: T) -> peek (swapDown x) === peek x
        , testProperty "swap_master_local" $ \(x :: T) -> hiddenSpaces (swapMaster x) === hiddenSpaces x
        , testProperty "swap_left_local" $ \(x :: T) -> hiddenSpaces (swapUp x) === hiddenSpaces x
        , testProperty "swap_right_local" $ \(x :: T) -> hiddenSpaces (swapDown x) === hiddenSpaces x
        , testProperty "swap_all_l" $ \(x :: T) -> foldr (const swapUp) x (index x) === x
        , testProperty "swap_all_r" $ \(x :: T) -> foldr (const swapDown) x (index x) === x
        , testProperty "swap_master_idempotent" $ \(x :: T) -> swapMaster (swapMaster x) === swapMaster x
        , testCase "swapUp exchanges the focus with the window above" $
            onStack swapUp (Stack 2 [1] [3]) @?= Stack 2 [] [1, 3]
        , testCase "swapUp moves a top focus to the bottom" $
            onStack swapUp (Stack 1 [] [2, 3]) @?= Stack 1 [3, 2] []
        , testCase "swapDown exchanges the focus with the window below" $
            onStack swapDown (Stack 2 [1] [3]) @?= Stack 2 [3, 1] []
        , testCase "swapMaster exchanges the focus with the master" $
            onStack swapMaster (Stack 3 [2, 1] []) @?= Stack 3 [] [2, 1]
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
        "Shift"
        [ testProperty "shift_reversible" $ \(x :: T) ->
            let y = swapMaster x
                n = currentTag y
             in forAll (arbitraryTag x) $ \i ->
                  isNothing (peek y) || normal ((view n . shift n . view i . shift i) y) == normal y
        , testProperty "shift moves the focused window to the target workspace" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> case peek x of
              Nothing -> property True
              Just w -> findTag w (shift n x) === Just n
        , testProperty "shift_master_focus" $ \(x :: T) -> peek (shiftMaster x) === peek x
        , testProperty "shift_master_local" $ \(x :: T) -> hiddenSpaces (shiftMaster x) === hiddenSpaces x
        , testProperty "shift_master_idempotent" $ \(x :: T) -> shiftMaster (shiftMaster x) === shiftMaster x
        , testProperty "shift_master_ordering" $ \(x :: T) -> case peek x of
            Nothing -> property True
            Just m -> L.delete m (index (shiftMaster x)) === L.delete m (index x)
        , testCase "shiftMaster moves the focus to the top" $
            onStack shiftMaster (Stack 3 [2, 1] []) @?= Stack 3 [] [1, 2]
        , testProperty "shift_win_focus" $ \(x :: T) ->
            forAll (arbitraryTag x) $ \n -> maybe (property True) (\w -> shiftWin n w x === shift n x) (peek x)
        , testProperty "shift_win_indentity" $ \(x :: T) ->
            forAll ((,) <$> arbitraryTag x <*> (arbitrary `suchThat` (not . (`member` x)))) $ \(n, w) ->
              shiftWin n w x === x
        , testProperty "shift_win_fix_current" $
            forAll (genT `suchThat` \x -> length (tags x) >= 2 && not (null (otherWindows x))) $ \x ->
              forAll ((,) <$> (arbitraryTag x `suchThat` (/= currentTag x)) <*> elements (otherWindows x)) $ \(n, w) ->
                current (shiftWin n w x) === current x
        ]
    , testGroup
        "Stack"
        [ testProperty "filter_order" $ \(x :: T) -> case stack (workspace (current x)) of
            Nothing -> property True
            Just st -> integrate' (filter (/= focus st) st) === L.filter (/= focus st) (integrate st)
        , testProperty "differentiate" propDifferentiate
        , testProperty "Foldable toList is integrate" propToListIntegrate
        , testProperty "Foldable methods agree with toList" propFoldableConsistent
        , testProperty "Traversable identity" propTraverseIdentity
        , testProperty "Traversable composition" propTraverseComposition
        , testProperty "Traversable effects run in integrate order" propTraverseOrder
        ]
    ]

propFocusI :: SizedPositive -> T -> Bool
propFocusI (SizedPositive n) x = case peek (applyN (Just n) focusUp x) of
  Nothing -> True
  Just w -> invariant (focusWindow w x)

-- | Apply the property to the current workspace's window at index @n@, wrapped.
withCurrentWindow :: Int -> T -> (WindowId -> Property) -> Property
withCurrentWindow n x p = case index x of
  [] -> property True
  ws -> p (ws !! (n `mod` length ws))

-- | Run an operation on a one-workspace StackSet holding the given stack.
onStack :: (T -> T) -> Stack Word32 -> Stack Word32
onStack f st = case stack (workspace (current (f x))) of
  Just result -> fmap (\(WindowId w) -> w) result
  Nothing -> error "onStack: the stack disappeared"
 where
  base = attach 1 0 [1]
  x = base{current = (current base){workspace = (workspace (current base)){stack = Just (fmap WindowId st)}}}

otherWindows :: T -> [WindowId]
otherWindows x = allWindows x L.\\ index x

genT :: Gen T
genT = arbitrary

focusNotLast :: T -> Bool
focusNotLast x = length (index x) >= 2 && peek x /= listToMaybe (reverse (index x))

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
