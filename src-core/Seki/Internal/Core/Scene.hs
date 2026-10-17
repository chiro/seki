{- | Pure layout selections and complete bottom-to-top window order.

'Placed' selects a content rectangle. Protocol readiness determines whether
that selection can actually be shown, and belongs to the runtime reconciler.
-}
module Seki.Internal.Core.Scene (
  Placement (..),
  DesiredScene (..),
  SceneStyle (..),
  BorderColor (..),
  defaultSceneStyle,
  planScene,
  stackWindows,
) where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Seki.Internal.Core.Geometry (Point (..), Rect, mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.Layout (LayoutClass, runLayout)
import Seki.Internal.Core.StackSet (OutputId, Screen (..), StackSet (..), WindowId, Workspace (..), integrate', peek, screens)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..))

data Placement = Omitted | Placed Rect deriving (Eq, Show)

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
  , sceneStacking :: [WindowId]
  , sceneFocus :: Maybe WindowId
  , sceneOutput :: Maybe OutputId
  }
  deriving (Eq, Show)

planScene :: (LayoutClass l) => SceneStyle -> CoreState i l -> Map OutputId Rect -> Map WindowId (Maybe WindowId) -> DesiredScene
planScene style st areas parents = DesiredScene placements stable ordering (preferred >>= \w -> if shown w then Just w else Nothing) activeOutput
 where
  (allWorkspaces, assigned, preferred, activeOutput) = case st of
    HeadlessState hw -> (headlessCurrent hw : headlessHidden hw, [], Nothing, Nothing)
    AttachedState ss _ -> (map workspace (screens ss) ++ hidden ss, screens ss, peek ss, Just (output (current ss)))
  stable = concatMap (integrate' . stack) allWorkspaces
  placements = Map.union (Map.fromList placed) (Map.fromList [(w, Omitted) | w <- stable])
  placed = [(w, Placed r) | s <- assigned, Just area <- [Map.lookup (output s) areas], (w, tile) <- runLayout (workspace s) area, Just r <- [inset tile]]
  inset tile
    | toInteger (rectWidth tile) <= 2 * b || toInteger (rectHeight tile) <= 2 * b = Nothing
    | otherwise = either (const Nothing) Just $ mkSize (rectWidth tile - 2 * border) (rectHeight tile - 2 * border) >>= mkRect (Point (rectX tile + border) (rectY tile + border))
  border = max 0 (sceneBorderWidth style)
  b = toInteger border
  shown w = case Map.lookup w placements of Just Placed{} -> True; _ -> False
  ordering = stackWindows stable (filter shown stable) preferred parents

stackWindows :: [WindowId] -> [WindowId] -> Maybe WindowId -> Map WindowId (Maybe WindowId) -> [WindowId]
stackWindows stable visibleWindows preferred parents = hiddenWindows ++ concatMap family (lastMatching (\r -> Just r == focusedRoot) roots)
 where
  hiddenWindows = filter (`notElem` visibleWindows) stable
  parent w = case Map.lookup w parents >>= id of Just p | p /= w && p `elem` visibleWindows -> Just p; _ -> Nothing
  canonicalRoot w = walk [] w
  walk seen w
    | w `elem` seen = minimum (w : takeWhile (/= w) seen)
    | otherwise = maybe w (walk (w : seen)) (parent w)
  roots = filter (\w -> canonicalRoot w == w) visibleWindows
  focusedRoot = canonicalRoot <$> (preferred >>= \w -> if w `elem` visibleWindows then Just w else Nothing)
  lastMatching predicate xs = filter (not . predicate) xs ++ filter predicate xs
  family r = descend [] r
  descend seen w
    | w `elem` seen = []
    | otherwise = w : concatMap (descend (w : seen)) (lastMatching containsFocus children)
   where
    children = [c | c <- visibleWindows, c /= canonicalRoot c, parent c == Just w]
    containsFocus c = maybe False (\f -> ancestor [] f c) preferred
  ancestor seen w target
    | w == target = True
    | w `elem` seen = False
    | otherwise = maybe False (\p -> ancestor (w : seen) p target) (parent w)
