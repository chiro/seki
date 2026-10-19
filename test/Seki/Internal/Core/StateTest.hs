module Seki.Internal.Core.StateTest (tests) where

import Data.Either (isRight)
import Data.List (find, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Seki.Internal.Core.Geometry (Point (..), mkRect, mkSize)
import Seki.Internal.Core.StackSet (
  FloatGeom (..),
  OutputId (..),
  Screen (..),
  Stack (..),
  StackSet (..),
  WindowId (..),
  Workspace (..),
  integrate',
  screens,
  shiftWin,
  tag,
  view,
 )
import Seki.Internal.Core.StackSetGen (Hotplug (..), coreFloating, coreInvariant, hotplugState, sortedWorkspaces)
import Seki.Internal.Core.State (
  CoreState (..),
  HeadlessWorkspaces (..),
  WorkspaceTagError (..),
  Workspaces (..),
  addOutput,
  focusWindowIn,
  fromWorkspaces,
  manageWindow,
  newCoreState,
  removeOutput,
  unmanageWindow,
 )
import Test.QuickCheck (Gen, Property, arbitrary, checkCoverage, conjoin, cover, elements, forAll, property, suchThatMap, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "Core state"
    [ testGroup
        "construction"
        [ testCase "rejects an empty tag list" $
            newCoreState () ([] :: [Int]) @?= Left NoWorkspaceTags
        , testCase "reports the first tag that repeats an earlier one" $
            newCoreState () "abcbca" @?= Left (DuplicateWorkspaceTag 'b')
        , testCase "starts headless on the first tag with the rest hidden" $
            newCoreState 'l' "abc"
              @?= Right
                ( fromWorkspaces
                    ( HeadlessState
                        ( HeadlessWorkspaces
                            (Workspace 'a' 'l' Nothing)
                            [Workspace 'b' 'l' Nothing, Workspace 'c' 'l' Nothing]
                            Map.empty
                        )
                    )
                )
        , testProperty "accepts exactly the non-empty duplicate-free tag lists" propAcceptsUnique
        ]
    , testGroup
        "adding outputs"
        [ testCase "attaches the remembered workspace to the first output" $
            addOutput o1 initial @?= AttachedState (StackSet (Screen (ws 'a') o1) [] [ws 'b', ws 'c'] Map.empty) []
        , testCase "shows the first hidden workspace after the visible screens" $
            (addOutput o3 . addOutput o2 . addOutput o1) initial
              @?= AttachedState (StackSet (Screen (ws 'a') o1) [Screen (ws 'b') o2, Screen (ws 'c') o3] [] Map.empty) []
        , testCase "leaves outputs unassigned in arrival order when no workspace is hidden" $
            (addOutput o5 . addOutput o4) full
              @?= AttachedState (stackSetOf full) [o4, o5]
        , testCase "ignores an output that already shows a workspace" $
            addOutput o2 full @?= full
        , testCase "ignores an output that is already unassigned" $
            let pending = addOutput o4 full in addOutput o4 pending @?= pending
        ]
    , testGroup
        "removing outputs"
        [ testCase "drops an unassigned output" $
            (removeOutput o4 . addOutput o5 . addOutput o4) full
              @?= AttachedState (stackSetOf full) [o5]
        , testCase "ignores an unknown output while attached" $
            removeOutput o5 full @?= full
        , testCase "ignores an output while headless" $
            removeOutput o1 initial @?= initial
        , testCase "hides a visible screen's workspace first" $
            removeOutput o2 full
              @?= AttachedState (StackSet (Screen (ws 'a') o1) [Screen (ws 'c') o3] [ws 'b'] Map.empty) []
        , testCase "passes focus from a removed current screen to the first visible one" $
            removeOutput o1 full
              @?= AttachedState (StackSet (Screen (ws 'b') o2) [Screen (ws 'c') o3] [ws 'a'] Map.empty) []
        , testCase "gives a freed workspace to the first unassigned output" $
            (removeOutput o2 . addOutput o5 . addOutput o4) full
              @?= AttachedState (StackSet (Screen (ws 'a') o1) [Screen (ws 'c') o3, Screen (ws 'b') o4] [] Map.empty) [o5]
        , testCase "moves the current workspace to an unassigned output when no screen survives" $
            (removeOutput o1 . addOutput o2 . addOutput o1) single
              @?= AttachedState (StackSet (Screen (ws 'a') o2) [] [] Map.empty) []
        , testCase "goes headless on the last output, remembering its workspace and the hidden order" $
            (removeOutput o2 . removeOutput o1 . addOutput o2 . addOutput o1) initial
              @?= HeadlessState (HeadlessWorkspaces (ws 'b') [ws 'a', ws 'c'] Map.empty)
        ]
    , testGroup
        "managing windows"
        [ testCase "focuses a new window above the current focus while attached" $
            (manageWindow id w2 . manageWindow id w1) full
              @?= AttachedState (StackSet (Screen (wsWith 'a' (Stack w2 [] [w1])) o1) [Screen (ws 'b') o2, Screen (ws 'c') o3] [] Map.empty) []
        , testCase "puts a window managed while headless on the remembered workspace" $
            (manageWindow id w2 . manageWindow id w1) initial
              @?= HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w2 [] [w1])) [ws 'b', ws 'c'] Map.empty)
        , testCase "shows a window managed while headless on the first output" $
            addOutput o1 (manageWindow id w1 initial)
              @?= AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [] [])) o1) [] [ws 'b', ws 'c'] Map.empty) []
        , testCase "ignores a window already on a hidden workspace while attached" $
            let st = AttachedState (StackSet (Screen (ws 'a') o1) [] [wsWith 'b' (Stack w1 [] []), ws 'c'] Map.empty) []
             in manageWindow id w1 st @?= st
        , testCase "ignores a window already on a hidden workspace while headless" $
            let st = HeadlessState (HeadlessWorkspaces (ws 'a') [wsWith 'b' (Stack w1 [] []), ws 'c'] Map.empty)
             in manageWindow id w1 st @?= st
        , testCase "applies the hook after inserting, so a shift to a hidden workspace hides the window and keeps focus" $
            (manageWindow (shiftWin 'b' w2) w2 . manageWindow id w1) (addOutput o1 initial)
              @?= AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [] [])) o1) [] [wsWith 'b' (Stack w2 [] []), ws 'c'] Map.empty) []
        , testCase "skips the hook for a window already managed" $
            let st = AttachedState (StackSet (Screen (ws 'a') o1) [] [wsWith 'b' (Stack w1 [] []), ws 'c'] Map.empty) []
             in manageWindow (shiftWin 'c' w1) w1 st @?= st
        , testCase "runs the hook while headless, shifting to a hidden workspace without changing the remembered one" $
            (manageWindow (shiftWin 'b' w2) w2 . manageWindow id w1) initial
              @?= HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w1 [] [])) [wsWith 'b' (Stack w2 [] []), ws 'c'] Map.empty)
        , testCase "a hook that views a workspace while headless changes the one the first output shows" $
            addOutput o1 (manageWindow (view 'b') w1 initial)
              @?= AttachedState (StackSet (Screen (ws 'b') o1) [] [wsWith 'a' (Stack w1 [] []), ws 'c'] Map.empty) []
        , testProperty "a hook shifting a new window keeps the invariant, puts it on the tag, and keeps focus" propManageShift
        ]
    , testGroup
        "unmanaging windows"
        [ testCase "removes a window and its floating rectangle while attached" $
            unmanageWindow w2 (AttachedState (StackSet (Screen (wsWith 'a' (Stack w2 [] [w1])) o1) [] [ws 'b', ws 'c'] (Map.singleton w2 floatRect)) [])
              @?= AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [] [])) o1) [] [ws 'b', ws 'c'] Map.empty) []
        , testCase "removes a window and its floating rectangle while headless" $
            unmanageWindow w2 (HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w2 [] [w1])) [wsWith 'b' (Stack w3 [] [])] (Map.singleton w2 floatRect)))
              @?= HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w1 [] [])) [wsWith 'b' (Stack w3 [] [])] Map.empty)
        , testCase "removes a window from a hidden workspace while headless" $
            unmanageWindow w3 (HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w1 [] [])) [wsWith 'b' (Stack w3 [] [])] Map.empty))
              @?= HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w1 [] [])) [ws 'b'] Map.empty)
        , testCase "ignores an unknown window" $
            let st = manageWindow id w1 full in unmanageWindow w2 st @?= st
        ]
    , testGroup
        "focusing a window"
        [ testCase "makes the output showing the window current" $
            focusWindowIn w2 (AttachedState (StackSet (Screen (wsWith 'a' (Stack w1 [] [])) o1) [Screen (wsWith 'b' (Stack w3 [] [w2])) o2] [] Map.empty) [])
              @?= AttachedState (StackSet (Screen (wsWith 'b' (Stack w2 [w3] [])) o2) [Screen (wsWith 'a' (Stack w1 [] [])) o1] [] Map.empty) []
        , testCase "ignores an unknown window" $
            let st = manageWindow id w1 full in focusWindowIn w2 st @?= st
        , testCase "changes nothing while headless" $
            let st = HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w2 [] [w1])) [ws 'b'] Map.empty)
             in focusWindowIn w1 st @?= st
        ]
    , testGroup
        "hotplug properties"
        [ localOption (QuickCheckTests 1000) $
            testProperty "every hotplug history keeps the core invariant" $ \h ->
              let st = hotplugState h
               in checkCoverage
                    . cover 10 (null (knownOutputs st)) "headless"
                    . cover 10 (unassignedCount st > 0) "an unassigned output"
                    . cover 20 (length (knownOutputs st) - unassignedCount st >= 2) "two or more screens"
                    $ coreInvariant (hotplugTags h) st
        , testProperty "adding and removing keep every workspace and floating window" $ \h ->
            let st = hotplugState h
             in forAll anyOutput $ \o ->
                  conjoin
                    [ (sortedWorkspaces (f st), coreFloating (f st)) === (sortedWorkspaces st, coreFloating st)
                    | f <- [addOutput o, removeOutput o]
                    ]
        , testProperty "removing a freshly added output undoes the addition" $ \h ->
            let st = hotplugState h
             in forAll (elements (filter (`notElem` knownOutputs st) (map OutputId [1 .. 12]))) $ \o ->
                  removeOutput o (addOutput o st) === st
        , testProperty "adding is idempotent" $ \h -> forAll anyOutput $ \o ->
            let once = addOutput o (hotplugState h) in addOutput o once === once
        , testProperty "removing is idempotent" $ \h -> forAll anyOutput $ \o ->
            let once = removeOutput o (hotplugState h) in removeOutput o once === once
        , testProperty "re-adding a removed output restores its workspace when none waits" $
            forAll (attachedWith (null . snd)) $ \(ss, _) ->
              forAll (elements (screens ss)) $ \sc ->
                fmap tag (shownOn (output sc) (addOutput (output sc) (removeOutput (output sc) (AttachedState ss []))))
                  === Just (tag (workspace sc))
        , testProperty "the last output's workspace and the hidden order return with a new output" $
            forAll (attachedWith (\(ss, u) -> null (visible ss) && null u)) $ \(ss, _) ->
              addOutput o9 (removeOutput (output (current ss)) (AttachedState ss []))
                === AttachedState ss{current = (current ss){output = o9}} []
        , testProperty "removing the current output focuses the first visible screen" $
            forAll (attachedWith (not . null . visible . fst)) $ \(ss, u) ->
              case removeOutput (output (current ss)) (AttachedState ss u) of
                AttachedState ss' _ -> Just (current ss') === listToMaybe (visible ss)
                HeadlessState{} -> property False
        , testProperty "a freed workspace goes to the first unassigned output" $
            forAll (attachedWith (not . null . snd)) $ \(ss, u) ->
              forAll (elements (screens ss)) $ \sc ->
                (listToMaybe u >>= \first -> tag <$> shownOn first (removeOutput (output sc) (AttachedState ss u)))
                  === Just (tag (workspace sc))
        ]
    ]

o9 :: OutputId
o9 = OutputId 9

propManageShift :: Hotplug -> Property
propManageShift h =
  forAll (elements (hotplugTags h)) $ \t ->
    let st' = manageWindow (shiftWin t fresh) fresh st
        (currentTag, currentFocus) = focused st
     in checkCoverage
          . cover 10 (null (knownOutputs st)) "headless"
          . cover 10 (t /= currentTag) "shifted away from the current workspace"
          $ conjoin
            [ coreInvariant (hotplugTags h) st'
            , [tag ws' | ws' <- sortedWorkspaces st', fresh `elem` integrate' (stack ws')] === [t]
            , if t == currentTag then focused st' === (t, Just fresh) else focused st' === (currentTag, currentFocus)
            ]
 where
  st = hotplugState h
  fresh = WindowId 9999
  focused (HeadlessState hw) = (tag (headlessCurrent hw), focus <$> stack (headlessCurrent hw))
  focused (AttachedState ss _) = (tag (workspace (current ss)), focus <$> stack (workspace (current ss)))

anyOutput :: Gen OutputId
anyOutput = OutputId <$> elements [1 .. 8]

unassignedCount :: Workspaces i l -> Int
unassignedCount HeadlessState{} = 0
unassignedCount (AttachedState _ u) = length u

knownOutputs :: Workspaces i l -> [OutputId]
knownOutputs HeadlessState{} = []
knownOutputs (AttachedState ss u) = map output (screens ss) ++ u

-- | An attached hotplug state, as its StackSet and unassigned outputs, that satisfies the predicate.
attachedWith :: ((StackSet Int Int, [OutputId]) -> Bool) -> Gen (StackSet Int Int, [OutputId])
attachedWith p = (hotplugState <$> arbitrary) `suchThatMap` matching
 where
  matching (AttachedState ss u) | p (ss, u) = Just (ss, u)
  matching _ = Nothing

-- | The workspace shown on an output.
shownOn :: OutputId -> Workspaces i l -> Maybe (Workspace i l)
shownOn _ HeadlessState{} = Nothing
shownOn o (AttachedState ss _) = workspace <$> find ((== o) . output) (screens ss)

o1, o2, o3, o4, o5 :: OutputId
o1 = OutputId 1
o2 = OutputId 2
o3 = OutputId 3
o4 = OutputId 4
o5 = OutputId 5

ws :: Char -> Workspace Char ()
ws t = Workspace t () Nothing

wsWith :: Char -> Stack WindowId -> Workspace Char ()
wsWith t s = Workspace t () (Just s)

w1, w2, w3 :: WindowId
w1 = WindowId 1
w2 = WindowId 2
w3 = WindowId 3

floatRect :: FloatGeom
floatRect = FloatAt (either (error . show) id (mkSize 100 100 >>= mkRect (Point 10 10)))

initial :: Workspaces Char ()
initial = either (error . show) coreWorkspaces (newCoreState () "abc")

single :: Workspaces Char ()
single = either (error . show) coreWorkspaces (newCoreState () "a")

full :: Workspaces Char ()
full = (addOutput o3 . addOutput o2 . addOutput o1) initial

stackSetOf :: Workspaces Char () -> StackSet Char ()
stackSetOf (AttachedState ss _) = ss
stackSetOf HeadlessState{} = error "expected an attached state"

propAcceptsUnique :: [Int] -> Property
propAcceptsUnique ts = isRight (newCoreState () ts) === (not (null ts) && nub ts == ts)
