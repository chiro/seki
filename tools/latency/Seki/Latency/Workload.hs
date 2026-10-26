{- | The native latency workloads as a pure model: which windows the driver
opens, which steps it takes, what the probe observes after each step, and
what the output shows. The live driver interprets it, and tests check it
without river.

Every window is named by its stack index: 0 is the newest window, which
the WM focuses and places first.
-}
module Seki.Latency.Workload (
  WorkloadName (..),
  parseWorkloadName,
  workloadText,
  WindowRole (..),
  windowRoles,
  Ratio (..),
  Model (..),
  initialModel,
  Step (..),
  stepCode,
  stepModel,
  plan,
  restoreSteps,
  Observation (..),
  expectedObservation,
  Layer (..),
  expectedLayers,
  pictureMismatch,
  tileBoxes,
  Exercise (..),
  exercised,
  exerciseCounts,
) where

import Data.Bits (shiftR, testBit)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Word (Word64, Word8)
import Seki.Fixture.Probe (Color (..), Image (..))

data WorkloadName = M1 | M2 deriving (Eq, Show, Enum, Bounded)

parseWorkloadName :: String -> Maybe WorkloadName
parseWorkloadName text = find ((== text) . workloadText) [minBound .. maxBound]

workloadText :: WorkloadName -> String
workloadText M1 = "m1"
workloadText M2 = "m2"

{- | A window in open order. A tile commits the size the WM proposes. A
dialog names its parent by open order and commits a fixed size, so the
default manage hook floats it and it never allocates another buffer.
-}
data WindowRole
  = TileRole Color
  | DialogRole Color (Int32, Int32) Int
  deriving (Eq, Show)

windowRoles :: WorkloadName -> [WindowRole]
windowRoles M1 = map TileRole (take 8 (cycle [Red, Green, Blue]))
windowRoles M2 = map TileRole [Red, Green, Blue, Red, Green, Blue] ++ [DialogRole Red (300, 200) 0, DialogRole Green (200, 300) 0]

{- | The Tall master ratio. The plan only flips between these two, so the
geometry stays a small table independent of the production layout.
-}
data Ratio = Half | TwoFifths deriving (Eq, Show)

data Model = Model
  { modelWorkload :: WorkloadName
  , modelFocus :: Int
  -- ^ The stack index of the focused window.
  , modelShown :: Bool
  -- ^ Workspace 1 is viewed.
  , modelRatio :: Ratio
  , modelFullscreen :: Maybe Int
  -- ^ Always the focused window (M2-D03).
  , modelRaise :: [Int]
  -- ^ The dialogs, bottom to top.
  }
  deriving (Eq, Show)

initialModel :: WorkloadName -> Model
initialModel name = Model name 0 True Half Nothing (reverse (filter (isDialog name) [0 .. length (windowRoles name) - 1]))

roleAt :: WorkloadName -> Int -> WindowRole
roleAt name i = let roles = windowRoles name in roles !! (length roles - 1 - i)

isDialog :: WorkloadName -> Int -> Bool
isDialog name i = case roleAt name i of DialogRole{} -> True; TileRole _ -> False

-- | Codes 0 to 3 keep the meaning of the retained M1 action codes.
data Step
  = FocusDown
  | FocusUp
  | ViewEmpty
  | ViewPopulated
  | ShrinkMaster
  | ExpandMaster
  | EnterFullscreen
  | ExitFullscreen
  deriving (Eq, Show, Enum, Bounded)

stepCode :: Step -> Int
stepCode = fromEnum

{- | A focus change exits fullscreen and raises a focused dialog, and a view
keeps fullscreen hidden until the workspace returns.
-}
stepModel :: Model -> Step -> Model
stepModel m step = case step of
  FocusDown -> focus ((modelFocus m + 1) `mod` count)
  FocusUp -> focus ((modelFocus m - 1) `mod` count)
  ViewEmpty -> m{modelShown = False}
  ViewPopulated -> m{modelShown = True}
  ShrinkMaster -> m{modelRatio = TwoFifths}
  ExpandMaster -> m{modelRatio = Half}
  EnterFullscreen -> m{modelFullscreen = Just (modelFocus m)}
  ExitFullscreen -> m{modelFullscreen = Nothing}
 where
  count = length (windowRoles (modelWorkload m))
  focus i = m{modelFocus = i, modelFullscreen = Nothing, modelRaise = if isDialog (modelWorkload m) i then filter (/= i) (modelRaise m) ++ [i] else modelRaise m}

{- | The infinite seeded plan from a model, one group per generator value.
An M1 group changes focus, then views the empty workspace and back. An M2
group changes focus, flips the master ratio so every tile resizes, may
request fullscreen on a focused tile, views the empty workspace and back,
and may then request the fullscreen exit.
-}
plan :: WorkloadName -> Word64 -> Model -> [Step]
plan name seed = go (map (`shiftR` 32) (drop 1 (iterate next seed)))
 where
  next x = x * 6364136223846793005 + 1442695040888963407
  go [] _ = []
  go (x : xs) m = let steps = group x m in steps ++ go xs (foldl' stepModel m steps)
  group x m = case name of
    M1 -> [focusStep, ViewEmpty, ViewPopulated]
    M2 ->
      let focused = stepModel m focusStep
          resize = if modelRatio focused == Half then ShrinkMaster else ExpandMaster
          enter = [EnterFullscreen | testBit x 1, not (isDialog name (modelFocus focused))]
          exit = [ExitFullscreen | testBit x 2, not (null enter)]
       in [focusStep, resize] ++ enter ++ [ViewEmpty, ViewPopulated] ++ exit
   where
    focusStep = if testBit x 0 then FocusUp else FocusDown

{- | Steps after the measured interval that view the populated workspace and
exit fullscreen, so the last picture shows every window.
-}
restoreSteps :: Model -> [Step]
restoreSteps m = [ViewPopulated | not (modelShown m)] ++ [ExitFullscreen | isJust (modelFullscreen m)]

{- | What the probe sees: the keyboard owner and the size each window last
committed.
-}
data Observation = Observation
  { observedOwner :: Maybe Int
  , observedSizes :: Map Int (Int32, Int32)
  }
  deriving (Eq, Show)

{- | A tile commits its content box, or the whole output while fullscreen. A
dialog commits its fixed size. A hidden workspace keeps every size.
-}
expectedObservation :: Model -> Observation
expectedObservation m =
  Observation
    (if modelShown m then Just (modelFocus m) else Nothing)
    (Map.fromList [(i, size i) | i <- [0 .. length (windowRoles name) - 1]])
 where
  name = modelWorkload m
  size i = case roleAt name i of
    DialogRole _ fixed _ -> fixed
    TileRole _
      | modelFullscreen m == Just i -> (800, 600)
      | otherwise -> let (_, _, w, h) = tileBox m i in (fromIntegral w, fromIntegral h)

tiles :: WorkloadName -> [Int]
tiles name = filter (not . isDialog name) [0 .. length (windowRoles name) - 1]

tileBox :: Model -> Int -> (Int, Int, Int, Int)
tileBox m i = Map.fromList (zip ts (tileBoxes (modelRatio m) (length ts))) Map.! i
 where
  ts = tiles (modelWorkload m)

{- | Tall content boxes on the 800x600 output, master first, inset by the
2 px border. This is xmonad's arithmetic written out independently of the
production layout: the master takes the ratio of the width rounded down,
and the stack rows split the height by repeated integer division.
-}
tileBoxes :: Ratio -> Int -> [(Int, Int, Int, Int)]
tileBoxes ratio n
  | n <= 1 = [inset (0, 0, 800, 600)]
  | otherwise = map inset ((0, 0, master, 600) : rows (n - 1) 0 600)
 where
  master = case ratio of Half -> 400; TwoFifths -> 320
  rows k top height
    | k <= 0 = []
    | otherwise = let small = height `div` k in (master, top, 800 - master, small) : rows (k - 1) (top + small) (height - small)
  inset (x, y, w, h) = (x + 2, y + 2, w - 4, h - 4)

{- | One window in painter's order: its content box and the border band
around it.
-}
data Layer = Layer
  { layerColor :: Color
  , layerContent :: (Int, Int, Int, Int)
  , layerBorder :: Int
  }
  deriving (Eq, Show)

{- | Bottom to top: the tiles, then the dialogs in raise order, each centered
on the output, then the fullscreen window over the whole output. Empty
while the empty workspace is viewed.
-}
expectedLayers :: Model -> [Layer]
expectedLayers m
  | not (modelShown m) = []
  | otherwise =
      [Layer (colorOf i) (tileBox m i) 2 | i <- tiles name, Just i /= modelFullscreen m]
        ++ [Layer c (centered w h) 2 | i <- modelRaise m, DialogRole c (w, h) _ <- [roleAt name i]]
        ++ [Layer (colorOf f) (0, 0, 800, 600) 0 | Just f <- [modelFullscreen m]]
 where
  name = modelWorkload m
  colorOf i = case roleAt name i of TileRole c -> c; DialogRole c _ _ -> c
  centered w h = (2 + (796 - fromIntegral w) `div` 2, 2 + (596 - fromIntegral h) `div` 2, fromIntegral w, fromIntegral h)

{- | Check every pixel of an 800x600 capture: content shows its window's
exact color, and a border band or uncovered pixel shows no probe color.
-}
pictureMismatch :: [Layer] -> Image -> Maybe String
pictureMismatch layers image
  | (imageWidth image, imageHeight image, BS.length (imageRgb image)) /= (800, 600, 800 * 600 * 3) = Just "native output dimensions wrong"
  | otherwise = listToMaybe [describe x y e | y <- [0 .. 599], x <- [0 .. 799], let e = expected x y, not (matches e (rgb x y))]
 where
  top = reverse layers
  rgb x y = let o = (y * 800 + x) * 3 in (BS.index (imageRgb image) o, BS.index (imageRgb image) (o + 1), BS.index (imageRgb image) (o + 2))
  expected x y = case find (\l -> inside (outer l) x y) top of
    Just l | inside (layerContent l) x y -> Just l
    _ -> Nothing
  matches (Just l) actual = actual == probeRgb (layerColor l)
  matches Nothing actual = actual `notElem` map probeRgb [Red, Green, Blue]
  describe x y e = "pixel " <> show (x, y) <> " shows " <> show (rgb x y) <> " where " <> maybe "no probe color" (\l -> show (layerColor l) <> " content of " <> show (layerContent l)) e <> " belongs"
  outer l = let (x, y, w, h) = layerContent l; b = layerBorder l in (x - b, y - b, w + 2 * b, h + 2 * b)
  inside (bx, by, bw, bh) x y = x >= bx && x < bx + bw && y >= by && y < by + bh

probeRgb :: Color -> (Word8, Word8, Word8)
probeRgb Red = (255, 0, 0)
probeRgb Green = (0, 255, 0)
probeRgb Blue = (0, 0, 255)

-- | What a measured run exercised, counted from the model's transitions.
data Exercise = Exercise
  { clipResizes :: Int
  -- ^ Steps that changed every tile's clip box.
  , fullscreenEnters :: Int
  , fullscreenExitsByFocus :: Int
  , fullscreenExitsByRequest :: Int
  , hiddenFullscreenViews :: Int
  -- ^ Views of the empty workspace while a window is fullscreen.
  , floatRaises :: Int
  -- ^ Steps that changed the topmost dialog.
  }
  deriving (Eq, Show)

exercised :: Model -> [Step] -> Exercise
exercised m0 = snd . foldl' count (m0, Exercise 0 0 0 0 0 0)
 where
  count (m, e) step =
    let next = stepModel m step
        full = isJust (modelFullscreen m)
        add b = if b then 1 else 0
     in ( next
        , Exercise
            { clipResizes = clipResizes e + add (step `elem` [ShrinkMaster, ExpandMaster])
            , fullscreenEnters = fullscreenEnters e + add (step == EnterFullscreen && not full)
            , fullscreenExitsByFocus = fullscreenExitsByFocus e + add (step `elem` [FocusDown, FocusUp] && full)
            , fullscreenExitsByRequest = fullscreenExitsByRequest e + add (step == ExitFullscreen && full)
            , hiddenFullscreenViews = hiddenFullscreenViews e + add (step == ViewEmpty && full)
            , floatRaises = floatRaises e + add (topmost next /= topmost m)
            }
        )
  topmost = listToMaybe . reverse . modelRaise

-- | The counts under the names the report uses.
exerciseCounts :: Exercise -> [(String, Int)]
exerciseCounts e =
  [ ("clip_resizes", clipResizes e)
  , ("fullscreen_enters", fullscreenEnters e)
  , ("fullscreen_exits_by_focus", fullscreenExitsByFocus e)
  , ("fullscreen_exits_by_request", fullscreenExitsByRequest e)
  , ("hidden_fullscreen_views", hiddenFullscreenViews e)
  , ("float_raises", floatRaises e)
  ]
