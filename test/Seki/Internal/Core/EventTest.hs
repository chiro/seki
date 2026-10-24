module Seki.Internal.Core.EventTest (tests) where

import Data.Int (Int32)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Seki.Internal.Core.Action (Intent (..), sink, spawn, toggleFullscreen, windows)
import Seki.Internal.Core.Event (CoreEvent (..), drainCore, stepCore)
import Seki.Internal.Core.Geometry (OutputRect (..))
import Seki.Internal.Core.GeometryGen (rectAt)
import Seki.Internal.Core.ManageHook (ManageHook, WindowProps (..), appId, askWindow, doFloat, doShift, idHook, isDialog, noWindowProps, (-->), (=?))
import Seki.Internal.Core.PointerOp (HEdge (..), OpId, OpInput (..), OpKind (..), OpOrigin (..), OpRequest (..), PointerOp (..), ResizeEdges (..), SeatId (..), SizeBounds (..), VEdge (..), firstOpId, nextOpId, noSizeBounds)
import Seki.Internal.Core.StackSet (
  FloatGeom (..),
  OutputId (..),
  Screen (..),
  Stack (..),
  StackSet (..),
  WindowId (..),
  Workspace (..),
  focusDown,
  focusWindow,
  greedyView,
  integrate',
  peek,
  view,
 )
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Internal.Core.StackSetGen (EventHistory (..), EventStep (..), coreInvariant, historyStart, sortedWorkspaces)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..), fromWorkspaces, newCoreState, syncPointerOp)
import Test.QuickCheck (Property, checkCoverage, conjoin, counterexample, cover, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "Core events"
    [ reducerTests
    , pointerOpTests
    , localOption (QuickCheckTests 1000) $
        testGroup
          "generated event sequences"
          [ testProperty "every prefix keeps the core invariant" propInvariant
          , testProperty "managed windows are those created and not yet closed" propManagedWindows
          , testProperty "draining equals stepping one event at a time" propDrainSteps
          , testProperty "the raise order holds each float once, with a focused float on top" propRaiseOrder
          , testProperty "the fullscreen set holds only managed windows that focus their own workspace" propFullscreen
          , testProperty "a pointer op's window is managed, not fullscreen, and floating or not yet moved" propPointerOp
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
    , testCase "a fullscreen request from its workspace's focus enters the fullscreen set" $
        coreFullscreen (fst (drainCore idHook [OutputCreated o1, created w1, created w2, FullscreenRequested w2] initial)) @?= Set.singleton w2
    , testCase "a fullscreen request from a window that is not its workspace's focus is dropped" $ do
        let (st, _) = drainCore idHook [OutputCreated o1, created w1, created w2, FullscreenRequested w1] initial
        coreFullscreen st @?= Set.empty
        peekCurrent st @?= Just w2
    , testCase "a fullscreen request from an unmanaged window is dropped" $
        coreFullscreen (fst (drainCore idHook [OutputCreated o1, created w1, FullscreenRequested w3] initial)) @?= Set.empty
    , testCase "an exit request and closing the window each leave the fullscreen set" $ do
        let (st, _) = drainCore idHook [OutputCreated o1, created w1, FullscreenRequested w1] initial
        coreFullscreen (fst (stepCore idHook (ExitFullscreenRequested w1) st)) @?= Set.empty
        coreFullscreen (fst (stepCore idHook (WindowClosed w1) st)) @?= Set.empty
    , testCase "toggleFullscreen enters and then exits" $ do
        let (st, _) = drainCore idHook [OutputCreated o1, created w1, RunAction (toggleFullscreen w1)] initial
        coreFullscreen st @?= Set.singleton w1
        coreFullscreen (fst (stepCore idHook (RunAction (toggleFullscreen w1)) st)) @?= Set.empty
    , testCase "M2-D03: a focus change on the same output exits fullscreen while a workspace switch does not" $ do
        let (st, _) = drainCore idHook [OutputCreated o1, created w1, created w2, FullscreenRequested w2] initial
            exits event = coreFullscreen (fst (stepCore idHook event st)) @?= Set.empty
        exits (RunAction (windows focusDown))
        exits (WindowInteraction w1)
        exits (created w3)
        let (away, _) = stepCore idHook (RunAction (windows (greedyView 'b'))) st
        coreFullscreen away @?= Set.singleton w2
        coreFullscreen (fst (stepCore idHook (RunAction (windows (greedyView 'a'))) away)) @?= Set.singleton w2
    , testCase "a focus change on another output keeps the fullscreen window" $ do
        let (st, _) = drainCore idHook (onTwoOutputs ++ [FullscreenRequested w1]) initial
        coreFullscreen st @?= Set.singleton w1
        coreFullscreen (fst (stepCore idHook (WindowInteraction w2) st)) @?= Set.singleton w1
    , testCase "a new window shifted to another workspace keeps the fullscreen window" $ do
        let (st, _) = drainCore (appId =? "mpv" --> doShift 'b') [OutputCreated o1, created w1, FullscreenRequested w1, WindowCreated w2 mpv] initial
        coreFullscreen st @?= Set.singleton w1
    , testCase "runs actions and keeps their intents in arrival order" $
        drainCore idHook [RunAction (spawn "x"), created w1, RunAction (spawn "y")] initial
          @?= (fromWorkspaces (HeadlessState (HeadlessWorkspaces (wsWith 'a' (Stack w1 [] [])) [ws 'b', ws 'c'] Map.empty)), [Spawn "x", Spawn "y"])
    ]

pointerOpTests :: TestTree
pointerOpTests =
  testGroup
    "pointer ops"
    [ testCase "a binding drag focuses a tile, floats it at its shown box moved by the offset, and the release ends the op" $ do
        let (started, _) = drainCore idHook (twoTiles ++ [StartOp (request Move FromBinding w1 tileBox)]) initial
        peekCurrent started @?= Just w1
        floatsOf started @?= Map.empty
        opWindow <$> corePointerOp started @?= Just w1
        let (moved, _) = drainCore idHook [OpInput seat op0 (OpMoved 30 10)] started
        floatsOf moved @?= Map.singleton w1 (FloatAt (box 32 12 496 596))
        let (released, _) = drainCore idHook [OpInput seat op0 OpReleased] moved
        corePointerOp released @?= Nothing
        floatsOf released @?= Map.singleton w1 (FloatAt (box 32 12 496 596))
    , testCase "a binding press with no motion leaves a tile tiled" $ do
        let (st, _) = drainCore idHook (twoTiles ++ [StartOp (request Move FromBinding w1 tileBox), OpInput seat op0 OpReleased]) initial
        floatsOf st @?= Map.empty
        corePointerOp st @?= Nothing
        peekCurrent st @?= Just w1
    , testCase "a bottom-right resize keeps the top-left and stops at the minimum hint" $ do
        let (st, _) = drainCore idHook (floatAt w1 (box 100 100 400 300) ++ [StartOp (request (Resize (Corner RightEdge BottomEdge)) FromBinding w1 (box 100 100 400 300)){reqBounds = SizeBounds 200 150 0 0}, OpInput seat op0 (OpMoved (-300) (-200)), OpInput seat op0 OpReleased]) initial
        floatsOf st @?= Map.singleton w1 (FloatAt (box 100 100 200 150))
        corePointerOp st @?= Nothing
    , testCase "a top-left resize keeps the bottom-right edges fixed" $ do
        let (st, _) = drainCore idHook (floatAt w1 (box 100 100 400 300) ++ [StartOp (request (Resize (Corner LeftEdge TopEdge)) FromClient w1 (box 100 100 400 300)), OpInput seat op0 (OpMoved (-20) (-10)), OpInput seat op0 (OpMoved (-40) 30)]) initial
        floatsOf st @?= Map.singleton w1 (FloatAt (box 60 130 440 270))
    , testCase "a release after the window closed changes nothing" $ do
        let setup = twoTiles ++ [StartOp (request Move FromBinding w1 tileBox), OpInput seat op0 (OpMoved 5 5), WindowClosed w1]
            (closed, _) = drainCore idHook setup initial
        corePointerOp closed @?= Nothing
        drainCore idHook (setup ++ [OpInput seat op0 OpReleased]) initial @?= drainCore idHook setup initial
    , testCase "a press while an op runs is ignored" $ do
        let (st, _) = drainCore idHook (twoTiles ++ [StartOp (request Move FromBinding w1 tileBox)]) initial
        stepCore idHook (StartOp (request Move FromBinding w2 tileBox)) st @?= (st, [])
    , testCase "M2-D05: a client request on a tiled window is ignored and the window stays tiled" $ do
        let (st, _) = drainCore idHook twoTiles initial
        stepCore idHook (StartOp (request Move FromClient w1 tileBox)) st @?= (st, [])
    , testCase "M2-D05: a client request on a float starts the op without changing focus" $ do
        let (st, _) = drainCore idHook (floatAt w1 (box 0 0 100 100) ++ [created w2, StartOp (request Move FromClient w1 (box 0 0 100 100)), OpInput seat op0 (OpMoved 10 20)]) initial
        peekCurrent st @?= Just w2
        Map.lookup w1 (floatsOf st) @?= Just (FloatAt (box 10 20 100 100))
    , testCase "a window that is not shown or is fullscreen starts no op" $ do
        let (st, _) = drainCore idHook twoTiles initial
        stepCore idHook (StartOp (request Move FromBinding w1 tileBox){reqShownBox = Nothing}) st @?= (st, [])
        let (full, _) = drainCore idHook [FullscreenRequested w2] st
        stepCore idHook (StartOp (request Move FromBinding w2 tileBox)) full @?= (full, [])
    , testCase "input with no river op, an older op, or another seat is ignored" $ do
        let (st, _) = drainCore idHook (twoTiles ++ [StartOp (request Move FromBinding w1 tileBox), OpInput seat op0 OpReleased, StartOp (request Move FromBinding w1 tileBox)]) initial
        mapM_
          (\event -> stepCore idHook event st @?= (st, []))
          [OpInput seat Nothing (OpMoved 9 9), OpInput seat op0 (OpMoved 9 9), OpInput seat op0 OpReleased, OpInput (SeatId 8) op1 OpReleased]
        corePointerOp (fst (stepCore idHook (OpInput seat op1 OpReleased) st)) @?= Nothing
    , testCase "removing the seat, sinking after a move, or entering fullscreen ends the op" $ do
        let (st, _) = drainCore idHook (twoTiles ++ [StartOp (request Move FromBinding w1 tileBox), OpInput seat op0 (OpMoved 1 1)]) initial
            ends event = corePointerOp (fst (stepCore idHook event st)) @?= Nothing
        ends (SeatRemoved seat)
        ends (RunAction (sink w1))
        ends (FullscreenRequested w1)
        opWindow <$> corePointerOp (fst (stepCore idHook (SeatRemoved (SeatId 8)) st)) @?= Just w1
    ]
 where
  twoTiles = [OutputCreated o1, created w1, created w2]
  tileBox = box 2 2 496 596
  floatAt w b = [OutputCreated o1, created w, RunAction (windows (StackSet.float w (FloatAt b)))]

seat :: SeatId
seat = SeatId 7

op0, op1 :: Maybe OpId
op0 = Just firstOpId
op1 = Just (nextOpId firstOpId)

request :: OpKind -> OpOrigin -> WindowId -> OutputRect -> OpRequest
request kind origin w shown = OpRequest seat w kind origin (Just shown) noSizeBounds

box :: Int32 -> Int32 -> Int32 -> Int32 -> OutputRect
box x y w h = OutputRect (rectAt x y w h)

floatsOf :: CoreState i l -> Map.Map WindowId FloatGeom
floatsOf st = case coreWorkspaces st of
  AttachedState ss _ -> floating ss
  HeadlessState hw -> headlessFloating hw

peekCurrent :: CoreState i l -> Maybe WindowId
peekCurrent st = case coreWorkspaces st of
  AttachedState ss _ -> peek ss
  HeadlessState{} -> Nothing

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

propFullscreen :: EventHistory -> Property
propFullscreen h =
  checkCoverage
    . cover 10 (any (not . Set.null . coreFullscreen) prefixes) "a fullscreen window"
    $ conjoin
      [ counterexample ("after " ++ show n ++ " events") $
        counterexample "every fullscreen window is managed and focuses its own workspace" (coreFullscreen st `Set.isSubsetOf` focuses)
      | (n, st) <- zip [0 :: Int ..] prefixes
      , let focuses = Set.fromList [focus s | Just s <- map stack (sortedWorkspaces (coreWorkspaces st))]
      ]
 where
  prefixes = scanl (\st (EventStep _ e) -> fst (stepCore idHook e st)) (historyStart h) (historySteps h)

propPointerOp :: EventHistory -> Property
propPointerOp h =
  checkCoverage
    . cover 5 (any (any opMoved . corePointerOp) prefixes) "a moved op"
    . cover 10 (any ((== Just False) . fmap opMoved . corePointerOp) prefixes) "an op not yet moved"
    $ conjoin
      [ counterexample ("after " ++ show n ++ " events") $
        conjoin
          [ counterexample "syncing again changes nothing" (syncPointerOp st === st)
          , counterexample "the op's window is managed, not fullscreen, and floating or not yet moved" (all valid (corePointerOp st))
          ]
      | (n, st) <- zip [0 :: Int ..] prefixes
      , let valid op = case coreWorkspaces st of
              AttachedState ss _ -> StackSet.member (opWindow op) ss && opWindow op `Set.notMember` coreFullscreen st && (Map.member (opWindow op) (floating ss) || not (opMoved op))
              HeadlessState _ -> False
      ]
 where
  prefixes = scanl (\st (EventStep _ e) -> fst (stepCore idHook e st)) (historyStart h) (historySteps h)
