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
) where

import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Word (Word64, Word8)
import Seki.Fixture.Probe (Color (..), Image (..))

data WorkloadName = M1 | M2 deriving (Eq, Show, Enum, Bounded)

parseWorkloadName :: String -> Maybe WorkloadName
parseWorkloadName text = find ((== text) . workloadText) [minBound .. maxBound]

workloadText :: WorkloadName -> String
workloadText M1 = "m1"
workloadText M2 = "m2"

-- | A window in open order. A tile commits the size the WM proposes.
newtype WindowRole = TileRole Color deriving (Eq, Show)

windowRoles :: WorkloadName -> [WindowRole]
windowRoles _ = map TileRole (take 8 (cycle [Red, Green, Blue]))

data Model = Model
  { modelWorkload :: WorkloadName
  , modelFocus :: Int
  -- ^ The stack index of the focused window.
  , modelShown :: Bool
  -- ^ Workspace 1 is viewed.
  }
  deriving (Eq, Show)

initialModel :: WorkloadName -> Model
initialModel name = Model name 0 True

-- | Codes keep the meaning of the retained M1 action codes.
data Step = FocusDown | FocusUp | ViewEmpty | ViewPopulated
  deriving (Eq, Show, Enum, Bounded)

stepCode :: Step -> Int
stepCode = fromEnum

stepModel :: Model -> Step -> Model
stepModel m step = case step of
  FocusDown -> m{modelFocus = (modelFocus m + 1) `mod` count}
  FocusUp -> m{modelFocus = (modelFocus m - 1) `mod` count}
  ViewEmpty -> m{modelShown = False}
  ViewPopulated -> m{modelShown = True}
 where
  count = length (windowRoles (modelWorkload m))

{- | The infinite seeded plan from a model: groups of a focus change, then a
view of the empty workspace and back.
-}
plan :: WorkloadName -> Word64 -> Model -> [Step]
plan _ seed _ = concatMap group (drop 1 (iterate next seed))
 where
  group x = [if (x `shiftR` 32) `mod` 2 == 0 then FocusDown else FocusUp, ViewEmpty, ViewPopulated]
  next x = x * 6364136223846793005 + 1442695040888963407

-- | Steps after the measured interval that view the populated workspace.
restoreSteps :: Model -> [Step]
restoreSteps m = [ViewPopulated | not (modelShown m)]

{- | What the probe sees: the keyboard owner and the size each window last
committed.
-}
data Observation = Observation
  { observedOwner :: Maybe Int
  , observedSizes :: Map Int (Int32, Int32)
  }
  deriving (Eq, Show)

expectedObservation :: Model -> Observation
expectedObservation m =
  Observation
    (if modelShown m then Just (modelFocus m) else Nothing)
    (Map.fromList [(i, (fromIntegral w, fromIntegral h)) | (i, (_, _, w, h)) <- zip [0 ..] (tileBoxes (length (windowRoles (modelWorkload m))))])

{- | Tall content boxes on the 800x600 output, master first, inset by the
2 px border. This is xmonad's arithmetic written out independently of the
production layout: the master takes half the width, and the stack rows
split the height by repeated integer division.
-}
tileBoxes :: Int -> [(Int, Int, Int, Int)]
tileBoxes n = map inset ((0, 0, 400, 600) : rows (n - 1) 0 600)
 where
  rows k top height
    | k <= 0 = []
    | otherwise = let small = height `div` k in (400, top, 400, small) : rows (k - 1) (top + small) (height - small)
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

-- | Bottom to top. Empty while the empty workspace is viewed.
expectedLayers :: Model -> [Layer]
expectedLayers m
  | not (modelShown m) = []
  | otherwise = [Layer (colorAt i) box 2 | (i, box) <- zip [0 ..] (tileBoxes (length roles))]
 where
  roles = windowRoles (modelWorkload m)
  colorAt i = case roles !! (length roles - 1 - i) of TileRole c -> c

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
