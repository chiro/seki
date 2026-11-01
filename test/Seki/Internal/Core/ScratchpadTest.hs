{-# LANGUAGE OverloadedStrings #-}

module Seki.Internal.Core.ScratchpadTest (tests) where

import Data.Function ((&))
import Data.Int (Int32)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Seki.Internal.Core.Action
import Seki.Internal.Core.Event
import Seki.Internal.Core.Geometry
import Seki.Internal.Core.ManageHook
import Seki.Internal.Core.Restore
import Seki.Internal.Core.Scratchpad.Types
import Seki.Internal.Core.StackSet qualified as W
import Seki.Internal.Core.State
import Seki.Internal.Layout.Full
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

box :: OutputRect
box = either (error . show) OutputRect (mkSize 500 400 >>= mkRect (Point 30 20))
definitions :: [Scratchpad]
definitions = [Scratchpad "term" "foot -a pad" (appId =? "pad") box, Scratchpad "other" "foot -a other" (appId =? "other") box]
initial :: CoreState String Full
initial =
  (either (error . show) id (newCoreState Full ["1", "2", "3"]))
    { coreScratchpadDefinitions = M.fromList [(scratchpadName p, (scratchpadCommand p, W.FloatAt (scratchpadGeometry p))) | p <- definitions]
    }
run :: [CoreEvent String Full] -> CoreState String Full -> (CoreState String Full, [Intent])
run = drainCoreWithScratchpads definitions idHook
attached :: CoreState String Full
attached = fst (run [OutputCreated (W.OutputId 1), OutputCreated (W.OutputId 2), WindowCreated ordinary noWindowProps] initial)
ordinary, pad, extra :: W.WindowId
ordinary = W.WindowId 1
pad = W.WindowId 2
extra = W.WindowId 3
props :: WindowProps
props = noWindowProps{propAppId = Just "pad"}
shown :: CoreState String Full
shown = fst (run [WindowCreated pad props] attached)
slot :: CoreState i l -> Maybe ScratchpadState
slot = M.lookup "term" . coreScratchpads
act :: Action String Full () -> CoreEvent String Full
act = RunAction
ssOf :: CoreState i l -> W.StackSet i l
ssOf st = case coreWorkspaces st of AttachedState ss _ -> ss; _ -> error "not attached"
inv :: LiveInventory
inv = LiveInventory (M.fromList [(WindowKey "ordinary", ordinary), (WindowKey "stable-pad", pad), (WindowKey "extra", extra)]) (M.fromList [(OutputKey 10, W.OutputId 1), (OutputKey 20, W.OutputId 2)])

tests :: TestTree
tests =
  testGroup
    "Scratchpads"
    [ testCase "validation rejects duplicate definitions" $ assertBool "duplicate" (case validateScratchpads (definitions ++ definitions) of Left _ -> True; _ -> False)
    , testCase "validation rejects blank name/command" $ do
        let p = Scratchpad " " "foot" (pure True) box
        assertBool "blank name" (case validateScratchpads [p] of Left _ -> True; _ -> False)
        assertBool "blank command" (case validateScratchpads [p{scratchpadName = "term", scratchpadCommand = ""}] of Left _ -> True; _ -> False)
    , testCase "absent launch coalesces a pending burst" $ do
        let (st, effects) = run (replicate 100 (act (toggleScratchpad "term"))) attached
        slot st @?= Just (LaunchPending 1)
        length [() | LaunchScratchpad{} <- effects] @?= 1
    , testCase "timeout latches, repeats do not launch, late map resolves" $ do
        let (st, effects) = run [act (toggleScratchpad "term"), act (scratchpadTimeout "term" 1), act (toggleScratchpad "term")] attached
        slot st @?= Just (LaunchTimedOut 1)
        length [() | LaunchScratchpad{} <- effects] @?= 1
        slot (fst (run [WindowCreated pad props] st)) @?= Just (ShownPad pad (Just ordinary))
    , testCase "reset explicitly permits a fresh generation; stale deadline ignored" $ do
        let (st, effects) = run [act (toggleScratchpad "term"), act (resetScratchpad "term"), act (toggleScratchpad "term"), act (scratchpadTimeout "term" 1)] attached
        slot st @?= Just (LaunchPending 2)
        length [() | LaunchScratchpad{} <- effects] @?= 2
        assertBool "reset warns" (any (\e -> case e of ScratchpadNotice _ -> True; _ -> False) effects)
    , testCase "hide detaches pad and restores prior eligible focus" $ do
        let (st, _) = run [act (toggleScratchpad "term")] shown
        slot st @?= Just (HiddenPad pad (W.FloatAt box))
        W.member pad (ssOf st) @?= False
        W.peek (ssOf st) @?= Just ordinary
        M.member pad (W.floating (ssOf st)) @?= False
    , testProperty "any finite existing-pad toggles never spawn or lose ordinary window" $ \(NonNegative n) ->
        let (st, effects) = run (replicate (n `mod` 1000) (act (toggleScratchpad "term"))) shown
         in W.member ordinary (ssOf st) .&&. null [() | LaunchScratchpad{} <- effects] .&&. (slot st /= Nothing)
    , testCase "duplicate remains ordinary, warning once, ownership unchanged" $ do
        let (st, effects) = run [WindowCreated extra props, WindowCreated (W.WindowId 4) props] shown
        slot st @?= slot shown
        assertBool "extra ordinary" (W.member extra (ssOf st))
        length [() | ScratchpadNotice{} <- effects] @?= 1
    , testCase "first matching definition wins even when occupied" $ do
        let defs = definitions ++ [Scratchpad "overlap" "foot" (pure True) box]
            (st, _) = drainCoreWithScratchpads defs idHook [WindowCreated extra props] shown
        M.lookup "overlap" (coreScratchpads st) @?= Nothing
        W.member extra (ssOf st) @?= True
    , testCase "owned matching bypasses hook; duplicate takes hook" $ do
        let (st, _) = drainCoreWithScratchpads definitions (doShift "3") [WindowCreated pad props, WindowCreated extra props] attached
        W.findTag pad (ssOf st) @?= Just "1"
        W.findTag extra (ssOf st) @?= Just "3"
    , testCase "bring uses current output and retains ordinary source" $ do
        let (st, _) = run [act (windows (W.view "2")), act (toggleScratchpad "term")] shown
        W.findTag pad (ssOf st) @?= Just "2"
        W.findTag ordinary (ssOf st) @?= Just "1"
        W.output (W.current (ssOf st)) @?= W.OutputId 2
    , testCase "fullscreen hide exits; close clears ownership and allows new launch" $ do
        let (hidden, _) = run [act (toggleFullscreen pad), act (toggleScratchpad "term")] shown
        S.member pad (coreFullscreen hidden) @?= False
        let (st, effects) = run [WindowClosed pad, act (toggleScratchpad "term")] hidden
        slot st @?= Just (LaunchPending 1)
        length [() | LaunchScratchpad{} <- effects] @?= 1
    , testCase "independent pads retain separate pending latches" $ do
        let (st, effects) = run [act (toggleScratchpad "term"), act (toggleScratchpad "other"), act (scratchpadTimeout "term" 1)] attached
        slot st @?= Just (LaunchTimedOut 1)
        M.lookup "other" (coreScratchpads st) @?= Just (LaunchPending 2)
        length [() | LaunchScratchpad{} <- effects] @?= 2
    , testCase "v2 hidden identity roundtrip does not rematch title" $ do
        let hidden = fst (run [act (toggleScratchpad "term")] shown)
            saved = saveCore inv hidden
            restored = fst (restoreCore inv saved initial)
        decodeRestoreState (encodeRestoreState saved) @?= Right saved
        slot restored @?= Just (HiddenPad pad (W.FloatAt box))
        let replayed = fst (run [WindowCreated pad noWindowProps] restored)
        slot replayed @?= slot restored
    , testCase "removed definition releases hidden survivor with missing output" $ do
        let hidden = fst (run [act (toggleScratchpad "term")] shown)
            saved = saveCore inv hidden
            restored = fst (restoreCore inv{liveOutputs = M.empty} saved initial{coreScratchpadDefinitions = M.empty})
        M.null (coreScratchpads restored) @?= True
        let withOutput = fst (run [OutputCreated (W.OutputId 9)] restored)
        W.member pad (ssOf withOutput) @?= True
    , testCase "shown restore recovers geometry without redundant floats" $ do
        let saved = (saveCore inv shown){savedFloats = []}
            restored = fst (restoreCore inv saved initial)
            headless = fst (restoreCore inv{liveOutputs = M.empty} saved initial)
        decodeRestoreState (encodeRestoreState saved) @?= Right saved
        slot restored @?= slot shown
        M.lookup pad (W.floating (ssOf restored)) @?= Just (W.FloatAt box)
        case coreWorkspaces headless of
          HeadlessState h -> M.lookup pad (headlessFloating h) @?= Just (W.FloatAt box)
          _ -> assertFailure "missing outputs should restore headless"
    , testCase "shown restore uses its own geometry over a conflicting float copy" $ do
        let saved = (saveCore inv shown){savedFloats = [(WindowKey "stable-pad", SavedClientSized)]}
            restored = fst (restoreCore inv saved initial)
        slot restored @?= slot shown
        M.lookup pad (W.floating (ssOf restored)) @?= Just (W.FloatAt box)
    , testCase "unresolved launches are not restored" $ do
        let pending = fst (run [act (toggleScratchpad "term")] attached)
            restored = fst (restoreCore inv (saveCore inv pending) initial)
        M.null (coreScratchpads restored) @?= True
    , testCase "v1 migration accepts exact old schema" $ do
        let old = "seki-restore 1\nRestoreState {savedWorkspaces = [], savedFloats = [], savedFullscreen = fromList [], savedSpawnedOnce = fromList []}"
        fmap savedScratchpads (decodeRestoreState old :: Either RestoreError (RestoreState String)) @?= Right []
    , testCase "empty rectangle is rejected before use" $ do
        let emptyBox = either (error . show) OutputRect (mkSize 0 10 >>= mkRect (Point 0 0))
        assertBool "empty" (case validateScratchpads [Scratchpad "bad" "foot" (pure True) emptyBox] of Left _ -> True; _ -> False)
    , testProperty "malformed duplicate restore claims preserve accessible window union" $ \entries ->
        let padKey n = [WindowKey "ordinary", WindowKey "stable-pad", WindowKey "extra", WindowKey "gone"] !! (abs (toInteger n) `mod` 4 & fromInteger)
            make (n, hidden, width) = SavedScratchpad (if even n then "term" else "other") (padKey n) hidden (SavedFloatAt 0 0 (fromIntegral width :: Int32) 100) Nothing
            saved = (saveCore inv shown){savedScratchpads = map make (take 40 (entries :: [(Int, Bool, Int)]))}
            restored = fst (restoreCore inv saved initial)
            work = case coreWorkspaces restored of
              AttachedState ss _ -> W.allWindows ss
              HeadlessState h -> concatMap (W.integrate' . W.stack) (headlessCurrent h : headlessHidden h)
            hiddenOwned = [w | HiddenPad w _ <- M.elems (coreScratchpads restored)]
            owned = [w | s <- M.elems (coreScratchpads restored), w <- case s of HiddenPad x _ -> [x]; ShownPad x _ -> [x]; _ -> []]
         in counterexample (show restored) (ordinary `elem` (work ++ hiddenOwned) && pad `elem` (work ++ hiddenOwned) && length owned == S.size (S.fromList owned) && all (`notElem` work) hiddenOwned)
    , testCase "malformed v2 and malformed v1 are rejected" $ do
        (decodeRestoreState "seki-restore 2\nnot-state" :: Either RestoreError (RestoreState String)) @?= Left RestoreUnreadable
        (decodeRestoreState "seki-restore 1\nnot-state" :: Either RestoreError (RestoreState String)) @?= Left RestoreUnreadable
    , testCase "automatic timeout updates status without reclaiming input" $ do
        let pending = fst (run [act (toggleScratchpad "term")] attached)
            (_, effects) = run [act (scratchpadTimeout "term" 1)] pending
        assertBool "no automatic focus claim" (ReclaimFocus `notElem` effects)
    , testCase "public insertion cannot expose an owned hidden identity" $ do
        let (st, _) = run [act (toggleScratchpad "term" >> windows (W.insertUp pad))] shown
        slot st @?= Just (HiddenPad pad (W.FloatAt box))
        W.member pad (ssOf st) @?= False
        W.peek (ssOf st) @?= Just ordinary
    , testProperty "hidden pad stays outside arbitrary user workspace navigation" $ \(NonNegative n) ->
        let hiddenState = fst (run [act (toggleScratchpad "term")] shown)
            actions = take (n `mod` 100) (cycle [windows (W.insertUp pad), windows (W.view "2"), windows W.focusDown, windows (W.view "1")])
            (st, _) = run (map act actions) hiddenState
         in not (W.member pad (ssOf st)) .&&. (slot st == Just (HiddenPad pad (W.FloatAt box)))
    , testCase "public deletion hides ownership and preserves hidden restore" $ do
        let st = fst (run [act (windows (W.delete pad))] shown)
            restored = fst (restoreCore inv (saveCore inv st) initial)
        slot st @?= Just (HiddenPad pad (W.FloatAt box))
        slot restored @?= slot st
        W.member pad (ssOf st) @?= False
    ]
