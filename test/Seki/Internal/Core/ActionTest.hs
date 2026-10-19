module Seki.Internal.Core.ActionTest (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Seki.Internal.Core.Action (Action, Intent (..), float, kill, runAction, sendMessage, sink, spawn, spawnOnce, windows, withFocused)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize)
import Seki.Internal.Core.Layout (CLR (..), ChangeLayout (..), Choose (..), IncMasterN (..), Mirror (..), Resize (..))
import Seki.Internal.Core.StackSet (
  FloatGeom (..),
  OutputId (..),
  Screen (..),
  Stack (..),
  StackSet (..),
  WindowId (..),
  Workspace (..),
  focusDown,
  focusUp,
  greedyView,
  shift,
  swapMaster,
 )
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..), fromWorkspaces, overWorkspaces)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type L = Choose Tall (Choose (Mirror Tall) Full)

tests :: TestTree
tests =
  testGroup
    "Core actions"
    [ testGroup
        "combinators"
        [ testCase "pure returns its value without changing state or recording intents" $
            runAction (pure 'x') attached @?= ('x', attached, [])
        , testCase "windows applies its function to the attached StackSet" $
            runAction (windows focusUp) attached @?= ((), withStack (Stack w3 [] [w2, w1]), [ReclaimFocus])
        , testCase "windows changes nothing while headless" $
            runAction (windows (greedyView 2)) headless @?= ((), headless, [ReclaimFocus])
        , testCase "sendMessage changes the current layout" $
            runAction (sendMessage Shrink) attached @?= ((), withLayout (Choose CL tall{tallRatio = 47 / 100} rest), [])
        , testCase "sendMessage changes nothing while headless" $
            runAction (sendMessage Shrink) headless @?= ((), headless, [])
        , testCase "spawn records its command while headless" $
            runAction (spawn "foot") headless @?= ((), headless, [Spawn "foot"])
        , testCase "kill records nothing while headless" $
            runAction kill headless @?= ((), headless, [])
        , testCase "kill records nothing without a focused window" $
            runAction kill emptyAttached @?= ((), emptyAttached, [])
        , testCase "intents accumulate in order across binds" $
            runAction (spawn "a" >> kill >>= \() -> spawn "b") attached
              @?= ((), attached, [Spawn "a", CloseWindow w2, Spawn "b"])
        , testCase "withFocused passes the focused window" $
            intentsOf (withFocused (\w -> spawn (show w))) attached @?= [Spawn (show w2)]
        , testCase "withFocused does nothing without a focused window" $
            runAction (withFocused (const (spawn "x"))) emptyAttached @?= ((), emptyAttached, [])
        , testCase "kill closes the window focused by an earlier step" $
            runAction (windows focusDown >> kill) attached
              @?= ((), withStack (Stack w1 [w2, w3] []), [ReclaimFocus, CloseWindow w1])
        ]
    , testGroup
        "default actions"
        [ testCase "sendMessage NextLayout selects Mirror Tall" $
            runAction (sendMessage NextLayout) attached
              @?= ((), withLayout (Choose CR tall (Choose CL (Mirror tall) Full)), [])
        , testCase "sendMessage Shrink narrows the master column" $
            runAction (sendMessage Shrink) attached @?= ((), withLayout (Choose CL tall{tallRatio = 47 / 100} rest), [])
        , testCase "sendMessage Expand widens the master column" $
            runAction (sendMessage Expand) attached @?= ((), withLayout (Choose CL tall{tallRatio = 53 / 100} rest), [])
        , testCase "sendMessage (IncMasterN 1) adds a master" $
            runAction (sendMessage (IncMasterN 1)) attached @?= ((), withLayout (Choose CL tall{tallNMaster = 2} rest), [])
        , testCase "sendMessage (IncMasterN (-1)) removes a master" $
            runAction (sendMessage (IncMasterN (-1))) attached @?= ((), withLayout (Choose CL tall{tallNMaster = 0} rest), [])
        , testCase "windows focusUp focuses the window above" $
            runAction (windows focusUp) attached @?= ((), withStack (Stack w3 [] [w2, w1]), [ReclaimFocus])
        , testCase "windows focusDown focuses the window below" $
            runAction (windows focusDown) attached @?= ((), withStack (Stack w1 [w2, w3] []), [ReclaimFocus])
        , testCase "windows swapMaster swaps the focused window with the master" $
            runAction (windows swapMaster) attached @?= ((), withStack (Stack w2 [] [w3, w1]), [ReclaimFocus])
        , testCase "windows (greedyView 2) shows workspace 2" $
            runAction (windows (greedyView 2)) attached
              @?= ( ()
                  , fromWorkspaces (AttachedState (StackSet (Screen (Workspace 2 layouts Nothing) o1) [] [ws1, Workspace 3 layouts Nothing] Map.empty) [])
                  , [ReclaimFocus]
                  )
        , testCase "windows (shift 2) moves the focused window to workspace 2" $
            runAction (windows (shift 2)) attached
              @?= ( ()
                  , fromWorkspaces
                      ( AttachedState
                          ( StackSet
                              (Screen (Workspace 1 layouts (Just (Stack w1 [w3] []))) o1)
                              []
                              [Workspace 2 layouts (Just (Stack w2 [] [])), Workspace 3 layouts Nothing]
                              Map.empty
                          )
                          []
                      )
                  , [ReclaimFocus]
                  )
        , testCase "spawn \"foot\" records a launch" $
            runAction (spawn "foot") attached @?= ((), attached, [Spawn "foot"])
        , testCase "kill records a close for the focused window" $
            runAction kill attached @?= ((), attached, [CloseWindow w2])
        , testCase "float floats a tiled window with no stored box" $
            runAction (float w2) attached @?= ((), withFloating (Map.singleton w2 ClientSized), [ReclaimFocus])
        , testCase "float keeps a floating window's stored box" $
            runAction (float w2) (withFloating (Map.singleton w2 stored)) @?= ((), withFloating (Map.singleton w2 stored), [ReclaimFocus])
        , testCase "float ignores a window it does not manage" $
            runAction (float (WindowId 9)) attached @?= ((), attached, [ReclaimFocus])
        , testCase "sink tiles a floating window again" $
            runAction (sink w2) (withFloating (Map.singleton w2 stored)) @?= ((), attached, [ReclaimFocus])
        , testCase "spawnOnce launches the same command once" $
            intentsOf (spawnOnce "bar" >> spawnOnce "bar") attached @?= [Spawn "bar"]
        , testCase "spawnOnce launches each different command" $
            intentsOf (spawnOnce "bar" >> spawnOnce "dunst" >> spawnOnce "bar") attached @?= [Spawn "bar", Spawn "dunst"]
        , testCase "spawnOnce remembers its commands across actions, headless included" $ do
            let ((), st, first) = runAction (spawnOnce "bar") headless
            first @?= [Spawn "bar"]
            coreSpawnedOnce st @?= Set.singleton "bar"
            intentsOf (spawnOnce "bar") st @?= []
            intentsOf (spawn "bar") st @?= [Spawn "bar"]
        ]
    ]

intentsOf :: Action Int L () -> CoreState Int L -> [Intent]
intentsOf action st = let ((), _, intents) = runAction action st in intents

tall :: Tall
tall = Tall 1 (3 / 100) (1 / 2)

rest :: Choose (Mirror Tall) Full
rest = Choose CL (Mirror tall) Full

layouts :: L
layouts = Choose CL tall rest

o1 :: OutputId
o1 = OutputId 1

w1, w2, w3 :: WindowId
w1 = WindowId 1
w2 = WindowId 2
w3 = WindowId 3

ws1 :: Workspace Int L
ws1 = Workspace 1 layouts (Just (Stack w2 [w3] [w1]))

-- | One output showing workspace 1, whose windows are w3, w2, w1 with w2 focused.
attached :: CoreState Int L
attached = fromWorkspaces (AttachedState (StackSet (Screen ws1 o1) [] [Workspace 2 layouts Nothing, Workspace 3 layouts Nothing] Map.empty) [])

emptyAttached :: CoreState Int L
emptyAttached = fromWorkspaces (AttachedState (StackSet (Screen (Workspace 1 layouts Nothing) o1) [] [Workspace 2 layouts Nothing] Map.empty) [])

headless :: CoreState Int L
headless = fromWorkspaces (HeadlessState (HeadlessWorkspaces ws1 [Workspace 2 layouts Nothing, Workspace 3 layouts Nothing] Map.empty))

withStack :: Stack WindowId -> CoreState Int L
withStack s = onCurrent (\w -> w{stack = Just s})

withFloating :: Map.Map WindowId FloatGeom -> CoreState Int L
withFloating fl = overWorkspaces onAttached attached
 where
  onAttached (AttachedState ss u) = AttachedState ss{floating = fl} u
  onAttached HeadlessState{} = error "attached is attached"

stored :: FloatGeom
stored = FloatAt (OutputRect (either (error . show) id (mkSize 300 200 >>= mkRect (Point 40 30))))

withLayout :: L -> CoreState Int L
withLayout l = onCurrent (\w -> w{layout = l})

onCurrent :: (Workspace Int L -> Workspace Int L) -> CoreState Int L
onCurrent f = overWorkspaces onAttached attached
 where
  onAttached (AttachedState ss u) = AttachedState ss{current = (current ss){workspace = f (workspace (current ss))}} u
  onAttached HeadlessState{} = error "attached is attached"
