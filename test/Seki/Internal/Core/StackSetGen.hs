{-# OPTIONS_GHC -Wno-orphans #-}

{- | StackSet generators and helpers ported from xmonad 0.18.1's
@tests/Instances.hs@ and @tests/Utils.hs@. Generated StackSets are built
through 'newCoreState' and 'addOutput', the only construction path seki has.
-}
module Seki.Internal.Core.StackSetGen (
  T,
  EmptyStackSet (..),
  NonEmptyWindowsStackSet (..),
  SizedPositive (..),
  NonEmptyNubList (..),
  attach,
  arbitraryTag,
  arbitraryWindow,
  hiddenSpaces,
  normal,
  applyN,
  tags,
  invariant,
) where

import Data.List (nub, sortOn)
import Data.Maybe (maybeToList)
import Seki.Internal.Core.StackSet
import Seki.Internal.Core.State (CoreState (..), addOutput, newCoreState)
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, elements, sized, suchThat, vectorOf)

type T = StackSet Int Int

-- | A small range, so generated windows repeat and non-members are common.
instance Arbitrary WindowId where
  arbitrary = WindowId . fromIntegral <$> chooseInt (0, 60)

instance Arbitrary T where
  arbitrary = do
    numWs <- chooseInt (1, 20)
    numScreens <- chooseInt (1, numWs)
    lay <- arbitrary
    wsIdxInFocus <- chooseInt (1, numWs)
    wsWindows <- vectorOf numWs (arbitrary :: Gen [WindowId])
    focusMoves <-
      traverse
        (\windows -> if null windows then pure Nothing else Just <$> chooseInt (0, length windows - 1))
        wsWindows
    let tagList = [1 .. numWs]
        fill (t, (moves, windows)) ss = applyN moves focusUp (foldr insertUp (view t ss) windows)
    pure
      ( view wsIdxInFocus $
          foldr fill (attach numScreens lay tagList) (zip tagList (zip focusMoves wsWindows))
      )

{- | Attach @n@ distinct outputs to a fresh core state over the given tags. @n@
must lie between 1 and the number of tags.
-}
attach :: (Eq i, Show i) => Int -> l -> [i] -> StackSet i l
attach n l ts = case foldl (flip addOutput) initial (map (OutputId . fromIntegral) [1 .. n]) of
  AttachedState ss [] -> ss
  _ -> error "attach: every output must get a workspace"
 where
  initial = either (error . show) id (newCoreState l ts)

newtype EmptyStackSet = EmptyStackSet T
  deriving (Show)

instance Arbitrary EmptyStackSet where
  arbitrary = do
    NonEmptyNubList ns <- arbitrary
    numScreens <- chooseInt (1, length ns)
    l <- arbitrary
    pure (EmptyStackSet (attach numScreens l ns))

newtype NonEmptyWindowsStackSet = NonEmptyWindowsStackSet T
  deriving (Show)

instance Arbitrary NonEmptyWindowsStackSet where
  arbitrary = NonEmptyWindowsStackSet <$> (arbitrary `suchThat` (not . null . allWindows))

newtype SizedPositive = SizedPositive Int
  deriving (Eq, Ord, Show)

instance Arbitrary SizedPositive where
  arbitrary = sized $ \s -> SizedPositive <$> chooseInt (1, max 1 s)

newtype NonEmptyNubList a = NonEmptyNubList [a]
  deriving (Eq, Ord, Show)

instance (Eq a, Arbitrary a) => Arbitrary (NonEmptyNubList a) where
  arbitrary = NonEmptyNubList <$> ((nub <$> arbitrary) `suchThat` (not . null))

arbitraryTag :: T -> Gen Int
arbitraryTag = elements . tags

arbitraryWindow :: NonEmptyWindowsStackSet -> Gen WindowId
arbitraryWindow (NonEmptyWindowsStackSet x) = elements (allWindows x)

-- | The workspaces not on the current screen.
hiddenSpaces :: StackSet i l -> [Workspace i l]
hiddenSpaces x = map workspace (visible x) ++ hidden x

-- | Sort the visible screens and hidden workspaces by tag.
normal :: (Ord i) => StackSet i l -> StackSet i l
normal s = s{hidden = sortOn tag (hidden s), visible = sortOn (tag . workspace) (visible s)}

applyN :: Maybe Int -> (a -> a) -> a -> a
applyN Nothing _ v = v
applyN (Just n) f v = iterate f v !! n

tags :: StackSet i l -> [i]
tags = map tag . workspaces

{- | No window appears twice, and no tag or screen output repeats. xmonad's
invariant checks only windows; the other two hold by construction here and
guard the hotplug paths.
-}
invariant :: T -> Bool
invariant s = distinct windows && distinct allTags && distinct outputs
 where
  allWs = workspace (current s) : map workspace (visible s) ++ hidden s
  windows = [w | ws <- allWs, t <- maybeToList (stack ws), w <- focus t : up t ++ down t]
  allTags = map tag allWs
  outputs = output (current s) : map output (visible s)
  distinct xs = nub xs == xs
