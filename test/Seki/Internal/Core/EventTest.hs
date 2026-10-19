module Seki.Internal.Core.EventTest (tests) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Seki.Internal.Core.Action (Intent (..), sink, spawn, windows)
import Seki.Internal.Core.Event (CoreEvent (..), drainCore, stepCore)
import Seki.Internal.Core.ManageHook (ManageHook, WindowProps (..), appId, askWindow, doFloat, doShift, idHook, isDialog, noWindowProps, (-->), (=?))
import Seki.Internal.Core.StackSet (
  OutputId (..),
  Screen (..),
  Stack (..),
  StackSet (..),
  WindowId (..),
  Workspace (..),
  focusWindow,
  integrate',
  peek,
  view,
 )
import Seki.Internal.Core.StackSetGen (EventHistory (..), EventStep (..), coreInvariant, historyStart, sortedWorkspaces)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..), fromWorkspaces, newCoreState)
import Test.QuickCheck (Property, checkCoverage, conjoin, counterexample, cover, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "Core events"
    [ reducerTests
    , localOption (QuickCheckTests 1000) $
        testGroup
          "generated event sequences"
          [ testProperty "every prefix keeps the core invariant" propInvariant
          , testProperty "managed windows are those created and not yet closed" propManagedWindows
          , testProperty "draining equals stepping one event at a time" propDrainSteps
          , testProperty "the raise order holds each float once, with a focused float on top" propRaiseOrder
          ]
    ]

reducerTests :: TestTree
reducerTests =
  testGroup
    "reducer"
    [ testCase "starts headless with nothing to do" $
        drainCore idHook [] initial @?= (initial, [])
    , testCase "attaches the first output to the remembered workspace" $
        stepCore idHook (OutputCreated o1) initial
          @?= (fromWorkspaces (AttachedState (StackSet (Screen (ws 'a') o1) [] [ws 'b', ws 'c'] Map.empty) []), [])
    , testCase "shows a hidden workspace on a second output" $
        drainCore idHook [OutputCreated o1, OutputCreated o2] initial
          @?= (twoOutputs, [])
    , testCase "hides a removed second output's workspace first" $
        stepCore idHook (OutputRemoved o2) twoOutputs
          @?= (fromWorkspaces (AttachedState (StackSet (Screen (ws 'a') o1) [] [ws 'b', ws 'c'] Map.empty) []), [])
    , testCase "goes headless when the last output is removed" $
        drainCore idHook [OutputCreated o1, OutputRemoved o1] initial
          @?= (initial, [])
    , testCase "manages and unmanages windows while attached" $
        drainCore idHook [OutputCreated o1, created w1, created w2, WindowClosed w2] initial
          @?= (fromWorkspaces (AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [] [])) o1) [] [ws 'b', ws 'c'] Map.empty) []), [])
    , testCase "manages and unmanages windows while headless" $
        drainCore idHook [created w1, created w2, WindowClosed w1] initial
          @?= (fromWorkspaces (HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w2 [] [])) [ws 'b', ws 'c'] Map.empty)), [])
    , testCase "shows a window created while headless on the first output" $
        drainCore idHook [created w1, OutputCreated o1] initial
          @?= (fromWorkspaces (AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [] [])) o1) [] [ws 'b', ws 'c'] Map.empty) []), [])
    , testCase "focuses an interacted window" $
        drainCore idHook [OutputCreated o1, created w1, created w2, WindowInteraction w1] initial
          @?= (fromWorkspaces (AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [w2] [])) o1) [] [ws 'b', ws 'c'] Map.empty) []), [])
    , testCase "makes the output of an interacted window current" $
        drainCore idHook (onTwoOutputs ++ [WindowInteraction w2]) initial
          @?= ( fromWorkspaces
                  ( AttachedState
                      (StackSet (Screen (wsWith 'b' (Stack w2 [] [])) o2) [Screen (wsWith 'a' (Stack w1 [] [])) o1] [ws 'c'] Map.empty)
                      []
                  )
              , [ReclaimFocus, ReclaimFocus]
              )
    , testCase "ignores interaction with an unknown window" $
        let (st, _) = drainCore idHook onTwoOutputs initial
         in stepCore idHook (WindowInteraction w3) st @?= (st, [])
    , testCase "runs the manage hook on each created window's properties" $
        drainCore (appId =? "mpv" --> doShift 'b') [OutputCreated o1, WindowCreated w1 foot, WindowCreated w2 mpv] initial
          @?= (fromWorkspaces (AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [] [])) o1) [] [wsWith 'b' (Stack w2 [] []), ws 'c'] Map.empty) []), [])
    , testCase "a new floating dialog goes on top of the raise order" $
        coreRaiseOrder (fst (drainCore dialogs [OutputCreated o1, created w1, dialog w2 w1, dialog w3 w1] initial)) @?= [w2, w3]
    , testCase "focusing a float raises it and focusing a tile leaves the order alone" $ do
        let (st, _) = drainCore dialogs [OutputCreated o1, created w1, dialog w2 w1, dialog w3 w1] initial
            raised = fst (stepCore dialogs (WindowInteraction w2) st)
        coreRaiseOrder raised @?= [w3, w2]
        coreRaiseOrder (fst (stepCore dialogs (WindowInteraction w1) raised)) @?= [w3, w2]
        coreRaiseOrder (fst (stepCore dialogs (RunAction (windows (focusWindow w1))) raised)) @?= [w3, w2]
    , testCase "closing or sinking a float drops it from the raise order" $ do
        let (st, _) = drainCore dialogs [OutputCreated o1, created w1, dialog w2 w1, dialog w3 w1] initial
        coreRaiseOrder (fst (stepCore dialogs (WindowClosed w3) st)) @?= [w2]
        coreRaiseOrder (fst (stepCore dialogs (RunAction (sink w2)) st)) @?= [w3]
    , testCase "runs actions and keeps their intents in arrival order" $
        drainCore idHook [RunAction (spawn "x"), created w1, RunAction (spawn "y")] initial
          @?= (fromWorkspaces (HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w1 [] [])) [ws 'b', ws 'c'] Map.empty)), [Spawn "x", Spawn "y"])
    ]

-- | w1 on workspace a (output 1, current) and w2 on workspace b (output 2).
onTwoOutputs :: [CoreEvent Char ()]
onTwoOutputs =
  [ OutputCreated o1
  , OutputCreated o2
  , created w1
  , RunAction (windows (view 'b'))
  , created w2
  , RunAction (windows (view 'a'))
  ]

twoOutputs :: CoreState Char ()
twoOutputs = fromWorkspaces (AttachedState (StackSet (Screen (ws 'a') o1) [Screen (ws 'b') o2] [ws 'c'] Map.empty) [])

initial :: CoreState Char ()
initial = either (error . show) id (newCoreState () "abc")

ws :: Char -> Workspace Char ()
ws t = Workspace t () Nothing

wsWith :: Char -> Stack WindowId -> Workspace Char ()
wsWith t s = Workspace t () (Just s)

o1, o2 :: OutputId
o1 = OutputId 1
o2 = OutputId 2

foot, mpv :: WindowProps
foot = noWindowProps{propAppId = Just (Text.pack "foot")}
mpv = noWindowProps{propAppId = Just (Text.pack "mpv")}

dialogs :: ManageHook Char ()
dialogs = isDialog --> doFloat

dialog :: WindowId -> WindowId -> CoreEvent i l
dialog w parent = WindowCreated w noWindowProps{propParent = Just parent}

created :: WindowId -> CoreEvent i l
created w = WindowCreated w noWindowProps

w1, w2, w3 :: WindowId
w1 = WindowId 1
w2 = WindowId 2
w3 = WindowId 3

propInvariant :: EventHistory -> Property
propInvariant h =
  checkCoverage
    . cover 10 (any isHeadless prefixes && not (all isHeadless prefixes)) "attached and headless"
    . cover 20 (not (all (null . managed) prefixes)) "managed windows"
    $ conjoin
      [ counterexample ("after " ++ show n ++ " events") (coreInvariant (historyTags h) (coreWorkspaces st))
      | (n, st) <- zip [0 :: Int ..] prefixes
      ]
 where
  prefixes = scanl (\st (EventStep _ e) -> fst (stepCore idHook e st)) (historyStart h) (historySteps h)
  isHeadless st = case coreWorkspaces st of
    HeadlessState{} -> True
    AttachedState{} -> False

propManagedWindows :: EventHistory -> Property
propManagedWindows h = managed final === Set.toList expected
 where
  (final, _) = drainCore idHook [e | EventStep _ e <- historySteps h] (historyStart h)
  expected = foldl track Set.empty (historySteps h)
  track live (EventStep _ (WindowCreated w _)) = Set.insert w live
  track live (EventStep _ (WindowClosed w)) = Set.delete w live
  track live _ = live

propDrainSteps :: EventHistory -> Property
propDrainSteps h = drainCore idHook events st0 === stepAll events st0
 where
  events = [e | EventStep _ e <- historySteps h]
  st0 = historyStart h
  stepAll [] st = (st, [])
  stepAll (e : es) st =
    let (st1, is1) = stepCore idHook e st
        (st2, is2) = stepAll es st1
     in (st2, is1 ++ is2)

-- | Every managed window, sorted, with any duplicate kept.
managed :: CoreState Int l -> [WindowId]
managed = sort . concatMap (integrate' . stack) . sortedWorkspaces . coreWorkspaces

propRaiseOrder :: EventHistory -> Property
propRaiseOrder h =
  checkCoverage
    . cover 20 (any (\st -> length (coreRaiseOrder st) > 1) prefixes) "two or more floats"
    $ conjoin
      [ counterexample ("after " ++ show n ++ " events") $
        conjoin
          [ sort (coreRaiseOrder st) === Map.keys floats
          , counterexample "a focused float is on top" (all (\w -> Map.notMember w floats || lastMaybe (coreRaiseOrder st) == Just w) focused)
          ]
      | (n, st) <- zip [0 :: Int ..] prefixes
      , let (floats, focused) = case coreWorkspaces st of
              AttachedState ss _ -> (floating ss, peek ss)
              HeadlessState hw -> (headlessFloating hw, Nothing)
      ]
 where
  prefixes = scanl (\st (EventStep _ e) -> fst (stepCore oddFloat e st)) (historyStart h) (historySteps h)
  oddFloat = fmap (\(WindowId n) -> odd n) askWindow --> doFloat
  lastMaybe xs = if null xs then Nothing else Just (last xs)
