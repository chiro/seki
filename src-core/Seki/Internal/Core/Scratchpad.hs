module Seki.Internal.Core.Scratchpad (
  togglePad,
  resetPad,
  managePad,
  closePad,
  ownedWindow,
) where

import Data.Map.Strict qualified as Map
import Data.Monoid (Endo (..))
import Data.Set qualified as Set
import Seki.Internal.Core.ManageHook (ManageHook, WindowProps, runQuery)
import Seki.Internal.Core.Scratchpad.Types
import Seki.Internal.Core.StackSet (FloatGeom (..), WindowId)
import Seki.Internal.Core.StackSet qualified as W
import Seki.Internal.Core.State

ownedWindow :: ScratchpadState -> Maybe WindowId
ownedWindow (HiddenPad w _) = Just w
ownedWindow (ShownPad w _) = Just w
ownedWindow _ = Nothing

currentFocus :: CoreState i l -> Maybe WindowId
currentFocus st = case coreWorkspaces st of
  AttachedState ss _ -> W.peek ss
  HeadlessState _ -> Nothing

onStack :: (W.StackSet i l -> W.StackSet i l) -> CoreState i l -> CoreState i l
onStack f = overWorkspaces $ \ws -> case ws of
  AttachedState ss waiting -> AttachedState (f ss) waiting
  HeadlessState h ->
    let ss = f (W.StackSet (W.Screen (headlessCurrent h) (W.OutputId 0)) [] (headlessHidden h) (headlessFloating h))
     in HeadlessState (HeadlessWorkspaces (W.workspace (W.current ss)) (map W.workspace (W.visible ss) ++ W.hidden ss) (W.floating ss))

putPad :: String -> ScratchpadState -> CoreState i l -> CoreState i l
putPad name slot st = st{coreScratchpads = Map.insert name slot (coreScratchpads st)}

padGeometry :: String -> CoreState i l -> FloatGeom
padGeometry name st = maybe ClientSized snd (Map.lookup name (coreScratchpadDefinitions st))

showPad :: String -> WindowId -> FloatGeom -> CoreState i l -> CoreState i l
showPad name w geom st =
  putPad name (ShownPad w (currentFocus st)) $
    (onStack (W.float w geom . W.insertUp w . W.delete w) st){coreFullscreen = Set.delete w (coreFullscreen st)}

togglePad :: (Eq i) => String -> CoreState i l -> (CoreState i l, [ScratchpadEffect])
togglePad name st = case Map.lookup name (coreScratchpadDefinitions st) of
  Nothing -> (st, [PadNotice ("unknown scratchpad: " <> name)])
  Just (command, _) -> case Map.lookup name (coreScratchpads st) of
    Nothing ->
      let generation = coreScratchpadGeneration st + 1
       in (putPad name (LaunchPending generation) st{coreScratchpadGeneration = generation}, [LaunchPad name generation command])
    Just LaunchPending{} -> (st, [])
    Just (HiddenPad w geom) -> (showPad name w geom st, [])
    Just (ShownPad w prior) -> case coreWorkspaces st of
      AttachedState ss _
        | W.findTag w ss == Just (W.currentTag ss) ->
            let geom = Map.findWithDefault (padGeometry name st) w (W.floating ss)
                removed = onStack (W.delete w) st
                restoreFocus x = case prior of
                  Just old | W.findTag old x == Just (W.currentTag x) -> W.focusWindow old x
                  _ -> x
             in (putPad name (HiddenPad w geom) ((onStack restoreFocus removed){coreFullscreen = Set.delete w (coreFullscreen st)}), [])
      _ -> (showPad name w (geometry w st) st, [])
 where
  geometry w s = case coreWorkspaces s of
    AttachedState ss _ -> Map.findWithDefault (padGeometry name s) w (W.floating ss)
    HeadlessState h -> Map.findWithDefault (padGeometry name s) w (headlessFloating h)

resetPad :: String -> CoreState i l -> (CoreState i l, [ScratchpadEffect])
resetPad name st = case Map.lookup name (coreScratchpads st) of
  Just LaunchPending{} -> reset
  _ -> (st, [])
 where
  reset = (st{coreScratchpads = Map.delete name (coreScratchpads st)}, [PadNotice ("scratchpad reset: " <> name <> "; operator is responsible for cleaning up the previous detached command; reset cannot prove it has exited")])

managePad :: [Scratchpad] -> ManageHook i l -> WindowId -> WindowProps -> CoreState i l -> (CoreState i l, [ScratchpadEffect])
managePad definitions hook w props st
  | any ((== Just w) . ownedWindow) (Map.elems (coreScratchpads st)) || inWorkspace = (st, [])
  | p : _ <- filter (\p -> runQuery (scratchpadQuery p) w props) definitions =
      let name = scratchpadName p
       in case Map.lookup name (coreScratchpads st) >>= ownedWindow of
            Nothing -> (showPad name w (FloatAt (scratchpadGeometry p)) st, [])
            Just _ ->
              let firstWarning = name `Set.notMember` coreScratchpadWarnings st
               in (ordinary{coreScratchpadWarnings = Set.insert name (coreScratchpadWarnings st)}, [PadNotice ("scratchpad ambiguous extra match for " <> name <> "; keeping ordinary window") | firstWarning])
  | otherwise = (ordinary, [])
 where
  ordinary = overWorkspaces (manageWindow (appEndo (runQuery hook w props)) w) st
  inWorkspace = case coreWorkspaces st of
    AttachedState ss _ -> W.member w ss
    HeadlessState h -> any ((w `elem`) . W.integrate' . W.stack) (headlessCurrent h : headlessHidden h)

closePad :: WindowId -> CoreState i l -> CoreState i l
closePad w st = (overWorkspaces (unmanageWindow w) st){coreScratchpads = Map.filter ((/= Just w) . ownedWindow) (coreScratchpads st)}
