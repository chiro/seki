{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Seki.Latency.Pure (runPure, forceResult, initialState, planParents, planFor, operationCodes, eventFor, BenchLayout, layout, validateResult) where

import Control.Exception (evaluate)
import Control.Monad (foldM, unless)
import Data.Aeson (encode, object, (.=))
import Data.Bits (shiftR, xor)
import Data.ByteString.Lazy qualified as BL
import Data.List (foldl', sort)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Seki.Internal.Core.Action
import Seki.Internal.Core.Event
import Seki.Internal.Core.Geometry
import Seki.Internal.Core.Layout
import Seki.Internal.Core.ManageHook (idHook)
import Seki.Internal.Core.PointerOp (HEdge (..), OpId, OpKind (..), PointerOp (..), ResizeEdges (..), SeatId (..), SizeBounds (..), VEdge (..))
import Seki.Internal.Core.Scene
import Seki.Internal.Core.StackSet qualified as S
import Seki.Internal.Core.State
import Seki.Internal.Layout.Full
import Seki.Internal.Layout.Tall
import Seki.Latency.Workload (WorkloadName (..), workloadText)

type BenchLayout = Choose Tall (Choose (Mirror Tall) Full)
layout :: BenchLayout
layout = Tall 1 (1 / 10) (1 / 2) ||| Mirror (Tall 1 (1 / 10) (1 / 2)) ||| Full

{- | Every window managed on workspace 1, the newest focused. In M2 the two
newest windows are client-sized floats, as dialogs of window 1 are.
-}
initialState :: WorkloadName -> Int -> CoreState String BenchLayout
initialState name count = fst (drainCore idHook [RunAction (windows (S.float w S.ClientSized)) | w <- Map.keys (planParents name count)] attached)
 where
  attached = overWorkspaces (\ws -> foldl' (flip (manageWindow id)) (addOutput (S.OutputId 1) ws) [S.WindowId (fromIntegral i) | i <- [1 .. count]]) (either (error . show) id (newCoreState layout ["1", "2", "3"]))

planParents :: WorkloadName -> Int -> Map.Map S.WindowId (Maybe S.WindowId)
planParents M1 _ = Map.empty
planParents M2 count = Map.fromList [(S.WindowId (fromIntegral w), Just (S.WindowId 1)) | w <- [count - 1, count]]

planFor :: WorkloadName -> Int -> CoreState String BenchLayout -> DesiredScene
planFor name count st = planScene defaultSceneStyle st (Map.singleton (S.OutputId 1) (OutputArea area area)) (planParents name count)

area :: Rect
area = either (error . show) id (mkSize 1920 1080 >>= mkRect (Point 0 0))

operationCodes :: WorkloadName -> Word64 -> Int -> [Int]
operationCodes name seed count = take count [fromIntegral ((x `shiftR` 32) `mod` codes) | x <- tail (iterate next seed)]
 where
  next x = x * 6364136223846793005 + 1442695040888963407
  codes = case name of M1 -> 16; M2 -> 20

{- | Codes 0 to 15 are the M1 operations. M2 adds a fullscreen toggle on the
focus, a client fullscreen request that the core drops unless window 2 is
focused, a click that focuses and raises a dialog, and floating window 3.
-}
eventFor :: WorkloadName -> Int -> Int -> CoreEvent String BenchLayout
eventFor M2 _ 16 = RunAction (withFocused toggleFullscreen)
eventFor M2 _ 17 = FullscreenRequested (S.WindowId 2)
eventFor M2 count 18 = WindowInteraction (S.WindowId (fromIntegral (count - 1)))
eventFor M2 _ 19 = RunAction (windows (S.float (S.WindowId 3) S.ClientSized))
eventFor _ _ code = RunAction $ case code of
  0 -> windows S.focusUp
  1 -> windows S.focusDown
  2 -> windows S.swapMaster
  3 -> sendMessage Shrink
  4 -> sendMessage Expand
  5 -> sendMessage NextLayout
  6 -> sendMessage (IncMasterN 1)
  7 -> sendMessage (IncMasterN (-1))
  8 -> windows (S.greedyView "1")
  9 -> windows (S.greedyView "2")
  10 -> windows (S.shift "2")
  11 -> windows (S.shift "1")
  12 -> windows (S.float (S.WindowId 1) (S.FloatAt (OutputRect area)))
  13 -> windows (S.sink (S.WindowId 1))
  14 -> spawn "diagnostic-intent"
  _ -> kill

mix :: Word64 -> Word64 -> Word64
mix a b = (a `xor` b) * 1099511628211
many :: (a -> Word64) -> [a] -> Word64
many f = foldl' (\a b -> mix a (f b)) 14695981039346656037
optional :: (a -> Word64) -> Maybe a -> Word64
optional _ Nothing = 0
optional f (Just x) = mix 14695981039346656037 (f x)
window :: S.WindowId -> Word64
window (S.WindowId w) = fromIntegral w
output :: S.OutputId -> Word64
output (S.OutputId o) = fromIntegral o
rect :: Rect -> Word64
rect r = many fromIntegral [rectX r, rectY r, rectWidth r, rectHeight r]
ratio :: Rational -> Word64
ratio r = mix (fromInteger (numerator r)) (fromInteger (denominator r))
tall :: Tall -> Word64
tall (Tall n d r) = many id [fromIntegral n, ratio d, ratio r]
forceLayout :: BenchLayout -> Word64
forceLayout (Choose c a (Choose d (Mirror b) Full)) = many id [choice c, tall a, choice d, tall b, 7]
 where
  choice CL = 1; choice CR = 2
workspace :: S.Workspace String BenchLayout -> Word64
workspace (S.Workspace tag l stack) = many id [many (fromIntegral . fromEnum) tag, forceLayout l, optional zipper stack]
 where
  zipper (S.Stack f up down) = many id [window f, many window up, many window down]
screen :: S.Screen String BenchLayout -> Word64
screen (S.Screen ws o) = mix (workspace ws) (output o)
floats :: Map.Map S.WindowId S.FloatGeom -> Word64
floats = Map.foldlWithKey' (\a w g -> mix a (mix (window w) (geometry g))) 0
 where
  geometry S.ClientSized = 0
  geometry (S.FloatAt (OutputRect r)) = mix 1 (rect r)
state :: CoreState String BenchLayout -> Word64
state st =
  many
    id
    [ many command (Set.toList (coreSpawnedOnce st))
    , many window (coreRaiseOrder st)
    , many window (Set.toList (coreFullscreen st))
    , optional pointerOp (corePointerOp st)
    , opNumber (coreNextOpId st)
    , case coreWorkspaces st of
        HeadlessState (HeadlessWorkspaces c hs fs) -> many id [0, workspace c, many workspace hs, floats fs]
        AttachedState (S.StackSet c vs hs fs) os -> many id [1, screen c, many screen vs, many workspace hs, floats fs, many output os]
    ]

-- | 'OpId' is abstract, so its rendering is the only way to reach the counter.
opNumber :: OpId -> Word64
opNumber = command . show

seat :: SeatId -> Word64
seat (SeatId i) = fromIntegral i
pointerOp :: PointerOp -> Word64
pointerOp (PointerOp i s w k (OutputRect r) (SizeBounds a b c d) moved) = many id [opNumber i, seat s, window w, kind k, rect r, many fromIntegral [a, b, c, d], fromIntegral (fromEnum moved)]
 where
  kind Move = 1
  kind (Resize edges) = mix 2 $ case edges of
    Horizontal h -> mix 1 (hedge h)
    Vertical v -> mix 2 (vedge v)
    Corner h v -> mix 3 (mix (hedge h) (vedge v))
  hedge LeftEdge = 1; hedge RightEdge = 2
  vedge TopEdge = 1; vedge BottomEdge = 2
command :: String -> Word64
command = many (fromIntegral . fromEnum)
intent :: Intent -> Word64
intent (Spawn cmd) = mix 1 (command cmd)
intent (CloseWindow w) = mix 2 (window w)
intent ReclaimFocus = 3
intent Restart = 4
scene :: DesiredScene -> Word64
scene (DesiredScene ws order raised stacking focus out op) =
  many
    id
    [ Map.foldlWithKey' (\a w p -> mix a (mix (window w) (placement p))) 0 ws
    , many window order
    , many window raised
    , many window stacking
    , optional window focus
    , optional output out
    , optional (\(s, i) -> mix (seat s) (opNumber i)) op
    ]
 where
  policyCode Crop = 1; policyCode Tolerate = 2
  placement Omitted = 0; placement (Placed policy r) = mix (mix 1 (policyCode policy)) (rect r); placement (Floating r) = mix 2 (rect r); placement (Centered r) = mix 3 (rect r); placement (Resizing _ r) = mix 6 (rect r); placement (Fullscreen o) = mix 4 (output o); placement HiddenFullscreen = 5

forceResult :: CoreState String BenchLayout -> [Intent] -> DesiredScene -> Word64
forceResult st intents desired = many id [state st, many intent intents, scene desired]

validateResult :: Int -> CoreState String BenchLayout -> DesiredScene -> Bool
validateResult count st desired = case coreWorkspaces st of
  HeadlessState _ -> False
  AttachedState ss os ->
    null os
      && sort (S.allWindows ss) == expected
      && sort (map S.tag (S.workspaces ss)) == ["1", "2", "3"]
      && sort (sceneWindowOrder desired) == expected
      && sort (sceneStacking desired) == expected
      && Map.keys (sceneWindows desired) == expected
      && all validPlacement (Map.elems (sceneWindows desired))
      && maybe True (\w -> maybe False placementShown (Map.lookup w (sceneWindows desired))) (sceneFocus desired)
      && all (\w -> any (\ws -> fmap S.focus (S.stack ws) == Just w) (S.workspaces ss)) (coreFullscreen st)
 where
  expected = [S.WindowId (fromIntegral i) | i <- [1 .. count]]
  validPlacement Omitted = True
  validPlacement (Placed _ r) = inScreen r
  validPlacement (Floating r) = inScreen r
  validPlacement (Centered r) = inScreen r
  validPlacement (Resizing _ r) = inScreen r
  validPlacement (Fullscreen _) = True
  validPlacement HiddenFullscreen = True
  inScreen r = rectWidth r > 0 && rectHeight r > 0 && rectX r >= 0 && rectY r >= 0 && rectX r + rectWidth r <= 1920 && rectY r + rectHeight r <= 1080

runPure :: FilePath -> WorkloadName -> Word64 -> Int -> Int -> IO ()
runPure path name seed windowCount steps = do
  unless (windowCount > 1 && windowCount <= 128 && steps > 0 && steps <= 1000000) (fail "pure workload bounds exceeded")
  let codes = operationCodes name seed (steps + 128)
      events = map (eventFor name windowCount) codes
      st0 = initialState name windowCount
  _ <- evaluate (many fromIntegral codes)
  _ <- evaluate (foldl' (\n event -> case event of RunAction action -> action `seq` n + 1; WindowInteraction w -> w `seq` n + 1; FullscreenRequested w -> w `seq` n + 1; _ -> event `seq` n + 1) (0 :: Int) events)
  _ <- evaluate (forceResult st0 [] (plan st0))
  (warmed, _, _) <- foldM (step False) (st0, 0, []) (take 128 events)
  (final, checksum, rows) <- foldM (step True) (warmed, 0, []) (drop 128 events)
  unless (validateResult windowCount final (plan final)) (fail "pure final invariants failed")
  let replay = foldl' (\(!st, !hash) event -> let (next, intents) = drainCore idHook [event] st; h = forceResult next intents (plan next) in (next, mix hash h)) (warmed, 0) (drop 128 events)
  unless (snd replay == checksum) (fail "pure deterministic replay checksum mismatch")
  BL.writeFile path (encode (object ["mode" .= ("pure" :: String), "workload" .= workloadText name, "seed" .= seed, "windows" .= windowCount, "steps" .= steps, "warmup" .= (128 :: Int), "checksum" .= checksum, "samples_ns" .= reverse rows, "valid" .= True]))
 where
  plan = planFor name windowCount
  step measured (!st, !checksum, rows) event = do
    start <- getMonotonicTimeNSec
    let (next, intents) = drainCore idHook [event] st
        desired = plan next
    digest <- evaluate (forceResult next intents desired)
    end <- getMonotonicTimeNSec
    unless (validateResult windowCount next desired) (fail "pure per-step invariants failed")
    pure (next, mix checksum digest, if measured then end - start : rows else rows)
