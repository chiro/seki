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
  Hotplug (..),
  Step (..),
  hotplugState,
  sortedWorkspaces,
  coreFloating,
  coreInvariant,
  DefaultLayout,
  EventStep (..),
  EventHistory (..),
  historyStart,
) where

import Data.List (nub, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Seki.Internal.Core.Action (Action)
import Seki.Internal.Core.Action qualified as Action
import Seki.Internal.Core.Event (CoreEvent (..))
import Seki.Internal.Core.Geometry (Point (..), mkRect, mkSize)
import Seki.Internal.Core.Layout (CLR (..), ChangeLayout (..), Choose (..), IncMasterN (..), Mirror (..), Resize (..))
import Seki.Internal.Core.ManageHook (noWindowProps)
import Seki.Internal.Core.StackSet
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..), addOutput, newCoreState, removeOutput)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  chooseInt,
  conjoin,
  counterexample,
  elements,
  frequency,
  listOf,
  shrinkList,
  sized,
  suchThat,
  vectorOf,
  (===),
 )

type T = StackSet Int Int

{- | Mostly a small range, so generated windows repeat. The wider tail keeps
non-members available even when a StackSet holds every small id.
-}
instance Arbitrary WindowId where
  arbitrary = WindowId . fromIntegral <$> frequency [(3, chooseInt (0, 60)), (1, chooseInt (61, 1000))]

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
  initial = either (error . show) coreWorkspaces (newCoreState l ts)

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

-- | One event in a generated hotplug history.
data Step
  = AddOutput OutputId
  | RemoveOutput OutputId
  | InsertWindow WindowId
  | FloatWindow WindowId
  | ViewTag Int
  | GreedyViewTag Int
  deriving (Show)

{- | A configured tag list and the events applied to a fresh core state. Outputs
come from a pool of six, so outputs are re-added, removed while unknown, and
left unassigned.
-}
data Hotplug = Hotplug
  { hotplugTags :: [Int]
  , hotplugSteps :: [Step]
  }
  deriving (Show)

instance Arbitrary Hotplug where
  arbitrary = do
    numWs <- chooseInt (1, 8)
    let output' = OutputId . fromIntegral <$> chooseInt (1, 6)
        tag' = chooseInt (1, numWs)
    steps <-
      listOf $
        frequency
          [ (3, AddOutput <$> output')
          , (2, RemoveOutput <$> output')
          , (3, InsertWindow <$> arbitrary)
          , (1, FloatWindow <$> arbitrary)
          , (1, ViewTag <$> tag')
          , (1, GreedyViewTag <$> tag')
          ]
    pure (Hotplug [1 .. numWs] steps)
  shrink (Hotplug ts steps) = Hotplug ts <$> shrinkList (const []) steps

hotplugState :: Hotplug -> Workspaces Int Int
hotplugState (Hotplug ts steps) = foldl (flip apply) initial steps
 where
  initial = either (error . show) coreWorkspaces (newCoreState 0 ts)
  apply (AddOutput o) = addOutput o
  apply (RemoveOutput o) = removeOutput o
  apply (InsertWindow w) = attached (insertUp w)
  apply (FloatWindow w) = attached (\ss -> if member w ss then float w floatRect ss else ss)
  apply (ViewTag t) = attached (view t)
  apply (GreedyViewTag t) = attached (greedyView t)
  attached f (AttachedState ss unassigned) = AttachedState (f ss) unassigned
  attached _ st = st
  floatRect = FloatAt (either (error . show) id (mkSize 100 100 >>= mkRect (Point 100 100)))

-- | Every workspace, sorted by tag.
sortedWorkspaces :: (Ord i) => Workspaces i l -> [Workspace i l]
sortedWorkspaces (HeadlessState (HeadlessWorkspaces c hs _)) = sortOn tag (c : hs)
sortedWorkspaces (AttachedState ss _) = sortOn tag (workspaces ss)

coreFloating :: Workspaces i l -> Map WindowId FloatGeom
coreFloating (HeadlessState hw) = headlessFloating hw
coreFloating (AttachedState ss _) = floating ss

-- | The plan's StackSet and headless-state invariants for the configured tags.
coreInvariant :: [Int] -> Workspaces Int l -> Property
coreInvariant configured st =
  conjoin
    [ counterexample "every configured tag appears exactly once" (sort (map tag allWs) === sort configured)
    , counterexample "no window appears twice" (distinct windows)
    , counterexample "screen outputs are distinct" (distinct outputs)
    , counterexample "unassigned outputs are distinct" (distinct unassigned)
    , counterexample "unassigned outputs show no workspace" (all (`notElem` outputs) unassigned)
    , counterexample "outputs wait only when no workspace is hidden" (null unassigned || null hiddenWs)
    ]
 where
  (allWs, hiddenWs, outputs, unassigned) = case st of
    HeadlessState (HeadlessWorkspaces c hs _) -> (c : hs, hs, [], [])
    AttachedState ss u -> (workspaces ss, hidden ss, map output (screens ss), u)
  windows = concatMap (integrate' . stack) allWs
  distinct xs = nub xs == xs

-- | The layout WP7 will configure by default.
type DefaultLayout = Choose Tall (Choose (Mirror Tall) Full)

-- | A core event with a description, since an 'Action' has no 'Show'.
data EventStep = EventStep String (CoreEvent Int DefaultLayout)

instance Show EventStep where
  show (EventStep d _) = d

{- | A configured tag list and the events reduced from a fresh core state.
Windows and outputs come from small pools, so windows are closed, re-created,
and interacted with while unknown, and outputs come and go.
-}
data EventHistory = EventHistory
  { historyTags :: [Int]
  , historySteps :: [EventStep]
  }
  deriving (Show)

instance Arbitrary EventHistory where
  arbitrary = do
    numWs <- chooseInt (1, 5)
    let window = WindowId . fromIntegral <$> chooseInt (1, 8)
        output' = OutputId . fromIntegral <$> chooseInt (1, 4)
        tag' = chooseInt (1, numWs)
        labelled name f x = EventStep (name ++ " " ++ show x) (f x)
    steps <-
      listOf $
        frequency
          [ (4, labelled "WindowCreated" (`WindowCreated` noWindowProps) <$> window)
          , (2, labelled "WindowClosed" WindowClosed <$> window)
          , (2, labelled "OutputCreated" OutputCreated <$> output')
          , (1, labelled "OutputRemoved" OutputRemoved <$> output')
          , (1, labelled "WindowInteraction" WindowInteraction <$> window)
          , (4, (\(d, a) -> EventStep ("RunAction (" ++ d ++ ")") (RunAction a)) <$> (tag' >>= elements . defaultActions))
          ]
    pure (EventHistory [1 .. numWs] steps)
  shrink (EventHistory ts steps) = EventHistory ts <$> shrinkList (const []) steps

historyStart :: EventHistory -> CoreState Int DefaultLayout
historyStart h = either (error . show) id (newCoreState (Choose CL tall (Choose CL (Mirror tall) Full)) (historyTags h))
 where
  tall = Tall 1 (3 / 100) (1 / 2)

-- | The actions WP7 will bind by default, with workspace actions aimed at one tag.
defaultActions :: Int -> [(String, Action Int DefaultLayout ())]
defaultActions t =
  [ ("sendMessage NextLayout", Action.sendMessage NextLayout)
  , ("sendMessage Shrink", Action.sendMessage Shrink)
  , ("sendMessage Expand", Action.sendMessage Expand)
  , ("sendMessage (IncMasterN 1)", Action.sendMessage (IncMasterN 1))
  , ("sendMessage (IncMasterN (-1))", Action.sendMessage (IncMasterN (-1)))
  , ("windows focusUp", Action.windows focusUp)
  , ("windows focusDown", Action.windows focusDown)
  , ("windows swapMaster", Action.windows swapMaster)
  , ("windows (greedyView " ++ show t ++ ")", Action.windows (greedyView t))
  , ("windows (shift " ++ show t ++ ")", Action.windows (shift t))
  , ("spawn \"foot\"", Action.spawn "foot")
  , ("kill", Action.kill)
  ]
