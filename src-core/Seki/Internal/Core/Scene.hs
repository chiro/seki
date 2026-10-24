{- | Pure layout selections and complete bottom-to-top window order.

A 'Placement' selects a content box in global logical coordinates, with the
border drawn outside it. Protocol readiness determines whether a selection can
actually be shown, and belongs to the runtime reconciler.

Floating windows never reach a layout. The planner never rewrites a stored
float box; it only clamps the placement, so a float returns to its stored
position when its workspace is shown on a larger output again (spec 3.5).

A fullscreen window still reaches the layout if it is tiled, so it keeps its
tile slot, and only its placement changes (M2-D03). On a hidden workspace it
stays fullscreen but covers no output.
-}
module Seki.Internal.Core.Scene (
  Placement (..),
  placementShown,
  OutputArea (..),
  DesiredScene (..),
  SceneStyle (..),
  BorderColor (..),
  defaultSceneStyle,
  planScene,
  stackWindows,
) where

import Data.Int (Int32)
import Data.List (elemIndex, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word32)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), Rect, mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.Layout (LayoutClass (..), SizePolicy, runLayout)
import Seki.Internal.Core.StackSet (FloatGeom (..), OutputId, Screen (..), StackSet (..), WindowId, Workspace (..), integrate', peek, screens)
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..))

data Placement
  = Omitted
  | -- | A tile's content box, inset from the layout's tile by the border, and
    -- what to show when the client commits another size.
    Placed SizePolicy Rect
  | -- | A float's stored content box moved onto its output and clamped inside it.
    Floating Rect
  | -- | A float with no stored box: center whatever size the client commits
    -- in this box, the output less its border, keeping the top-left inside it.
    Centered Rect
  | -- | A fullscreen window, which river places to cover this output.
    Fullscreen OutputId
  | -- | A fullscreen window whose workspace is hidden: hidden like 'Omitted',
    -- and still fullscreen, so its workspace returns to an output fullscreen.
    HiddenFullscreen
  deriving (Eq, Show)

placementShown :: Placement -> Bool
placementShown placement = case placement of
  Omitted -> False
  HiddenFullscreen -> False
  Placed _ _ -> True
  Floating _ -> True
  Centered _ -> True
  Fullscreen _ -> True

-- | An output's whole rectangle, where floats go, and the usable part layouts tile.
data OutputArea = OutputArea
  { outputBounds :: Rect
  , outputUsable :: Rect
  }
  deriving (Eq, Show)

data BorderColor = BorderColor Word32 Word32 Word32 Word32 deriving (Eq, Show)

data SceneStyle = SceneStyle
  { sceneBorderWidth :: Int32
  , sceneFocusedColor :: BorderColor
  , sceneUnfocusedColor :: BorderColor
  }
  deriving (Eq, Show)

defaultSceneStyle :: SceneStyle
defaultSceneStyle = SceneStyle 2 (BorderColor 0 0x80000000 0xffffffff 0xffffffff) (BorderColor 0x40000000 0x40000000 0x40000000 0xffffffff)

data DesiredScene = DesiredScene
  { sceneWindows :: Map WindowId Placement
  , sceneWindowOrder :: [WindowId]
  , sceneRaiseOrder :: [WindowId]
  -- ^ The floating windows bottom to top.
  , sceneStacking :: [WindowId]
  , sceneFocus :: Maybe WindowId
  , sceneOutput :: Maybe OutputId
  }
  deriving (Eq, Show)

{- | Lay out each shown workspace's tiles in its output's usable area and place
its floats on the whole output. A float keeps its stored size unless the
output, less the border, is smaller, and moves only as far as it takes to keep
the content and border on the output.
-}
planScene :: (LayoutClass l) => SceneStyle -> CoreState i l -> Map OutputId OutputArea -> Map WindowId (Maybe WindowId) -> DesiredScene
planScene style st areas parents = DesiredScene placements stable raiseOrder ordering (preferred >>= \w -> if shown w then Just w else Nothing) activeOutput
 where
  (allWorkspaces, assigned, floats, preferred, activeOutput) = case coreWorkspaces st of
    HeadlessState hw -> (headlessCurrent hw : headlessHidden hw, [], headlessFloating hw, Nothing, Nothing)
    AttachedState ss _ -> (map workspace (screens ss) ++ hidden ss, screens ss, floating ss, peek ss, Just (output (current ss)))
  raiseOrder = coreRaiseOrder st
  stable = concatMap (integrate' . stack) allWorkspaces
  placements = Map.union (Map.fromList placed) (Map.fromList [(w, if w `Set.member` coreFullscreen st then HiddenFullscreen else Omitted) | w <- stable])
  placed =
    concat
      [ [(w, Placed (sizePolicy (layout tiled)) r) | (w, tile) <- runLayout tiled (outputUsable area), Just r <- [inset tile]]
        ++ [(w, p) | w <- integrate' (stack (workspace s)), Just g <- [Map.lookup w floats], Just p <- [floatPlacement (outputBounds area) g]]
        ++ [(w, Fullscreen (output s)) | w <- integrate' (stack (workspace s)), w `Set.member` coreFullscreen st]
      | s <- assigned
      , Just area <- [Map.lookup (output s) areas]
      , let tiled = (workspace s){stack = stack (workspace s) >>= StackSet.filter (`Map.notMember` floats)}
      ]
  inset tile
    | toInteger (rectWidth tile) <= 2 * b || toInteger (rectHeight tile) <= 2 * b = Nothing
    | otherwise = either (const Nothing) Just $ mkSize (rectWidth tile - 2 * border) (rectHeight tile - 2 * border) >>= mkRect (Point (rectX tile + border) (rectY tile + border))
  floatPlacement bounds g =
    inset bounds >>= \inner -> case g of
      ClientSized -> Just (Centered inner)
      FloatAt (OutputRect r) -> either (const Nothing) (Just . Floating) $ do
        let width = min (rectWidth r) (rectWidth inner)
            height = min (rectHeight r) (rectHeight inner)
            clampTo lo extent len v = max (toInteger lo) (min (toInteger lo + toInteger extent - toInteger len) v)
            x = clampTo (rectX inner) (rectWidth inner) width (toInteger (rectX bounds) + toInteger (rectX r))
            y = clampTo (rectY inner) (rectHeight inner) height (toInteger (rectY bounds) + toInteger (rectY r))
        mkSize width height >>= mkRect (Point (fromInteger x) (fromInteger y))
  border = max 0 (sceneBorderWidth style)
  b = toInteger border
  shown w = maybe False placementShown (Map.lookup w placements)
  fullscreen = [w | w <- stable, Just (Fullscreen _) <- [Map.lookup w placements]]
  ordering = stackWindows stable raiseOrder fullscreen (filter shown stable) preferred parents

{- | The complete bottom-to-top order: hidden windows, then the tiled tier, then
the floating tier (M2-D02), then the shown fullscreen windows (M2-D03). Parent
links group a tier's shown windows into families, each parent directly below
its children and the branch holding the focus on top among siblings (M1-D03);
a parent in another tier or not shown starts no family. Tiled families follow StackSet order and floating families
the raise order of their most recently raised member. Within each tier the
focused window's family is on top.
-}
stackWindows :: [WindowId] -> [WindowId] -> [WindowId] -> [WindowId] -> Maybe WindowId -> Map WindowId (Maybe WindowId) -> [WindowId]
stackWindows stable raiseOrder fullscreen visibleWindows preferred parents =
  filter (`notElem` visibleWindows) stable ++ tier id tiles ++ tier (sortOn latestRaise) floats ++ filter (`elem` visibleWindows) fullscreen
 where
  floats = filter (\w -> w `elem` visibleWindows && w `notElem` fullscreen) raiseOrder
  tiles = filter (\w -> w `elem` visibleWindows && w `notElem` floats && w `notElem` fullscreen) stable
  latestRaise = maximum . map (`elemIndex` floats)
  tier arrange ws = concat (lastMatching (\f -> maybe False (`elem` f) preferred) (arrange (map (descend []) roots)))
   where
    parent w = case Map.lookup w parents >>= id of Just p | p /= w && p `elem` ws -> Just p; _ -> Nothing
    canonicalRoot w = walk [] w
    walk seen w
      | w `elem` seen = minimum (w : takeWhile (/= w) seen)
      | otherwise = maybe w (walk (w : seen)) (parent w)
    roots = filter (\w -> canonicalRoot w == w) ws
    descend seen w
      | w `elem` seen = []
      | otherwise = w : concatMap (descend (w : seen)) (lastMatching containsFocus children)
     where
      children = [c | c <- ws, c /= canonicalRoot c, parent c == Just w]
      containsFocus c = maybe False (\f -> ancestor [] f c) preferred
    ancestor seen w target
      | w == target = True
      | w `elem` seen = False
      | otherwise = maybe False (\p -> ancestor (w : seen) p target) (parent w)
  lastMatching predicate xs = filter (not . predicate) xs ++ filter predicate xs
