{- | Interactive pointer operations: the op in progress and its geometry.

An op moves or resizes a float's output-relative content box (M2-D01) by the
cumulative pointer offset river reports since the op started.
-}
module Seki.Internal.Core.PointerOp (
  SeatId (..),
  OpId,
  firstOpId,
  nextOpId,
  HEdge (..),
  VEdge (..),
  ResizeEdges (..),
  OpKind (..),
  OpOrigin (..),
  SizeBounds (..),
  noSizeBounds,
  OpRequest (..),
  OpInput (..),
  PointerOp (..),
  opBox,
  moveBox,
  resizeBox,
) where

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.StackSet (WindowId)

-- | A river seat's object id. The core only compares and forwards it.
newtype SeatId = SeatId Word32 deriving (Eq, Ord, Show)

{- | Which op this is, counted per process. Two ops on the same window with the
same box must still differ, so the reconciler can tell an end and a start
from nothing.
-}
newtype OpId = OpId Word64 deriving (Eq, Ord, Show)

firstOpId :: OpId
firstOpId = OpId 0

nextOpId :: OpId -> OpId
nextOpId (OpId n) = OpId (n + 1)

data HEdge = LeftEdge | RightEdge deriving (Eq, Show)

data VEdge = TopEdge | BottomEdge deriving (Eq, Show)

-- | River's resize edges, which are never none and never both opposite edges.
data ResizeEdges = Horizontal HEdge | Vertical VEdge | Corner HEdge VEdge deriving (Eq, Show)

data OpKind = Move | Resize ResizeEdges deriving (Eq, Show)

-- | A binding floats a tiled window. A client request on a tiled window is ignored (M2-D05).
data OpOrigin = FromBinding | FromClient deriving (Eq, Show)

-- | River's @dimensions_hint@ in logical pixels, where 0 is no preference.
data SizeBounds = SizeBounds
  { boundsMinWidth :: Int32
  , boundsMinHeight :: Int32
  , boundsMaxWidth :: Int32
  , boundsMaxHeight :: Int32
  }
  deriving (Eq, Show)

noSizeBounds :: SizeBounds
noSizeBounds = SizeBounds 0 0 0 0

-- | Everything an op needs at start. The runtime resolves it when it drains its queue.
data OpRequest = OpRequest
  { reqSeat :: SeatId
  , reqWindow :: WindowId
  , reqKind :: OpKind
  , reqOrigin :: OpOrigin
  , reqShownBox :: Maybe OutputRect
  -- ^ The content box last rendered, relative to the window's output, or
  -- 'Nothing' when the window is not shown, and then no op starts.
  , reqBounds :: SizeBounds
  }
  deriving (Eq, Show)

data OpInput = OpMoved Int32 Int32 | OpReleased deriving (Eq, Show)

{- | The op in progress. The StackSet's float box is the only geometry: each
delta recomputes it from 'opStart'.
-}
data PointerOp = PointerOp
  { opId :: OpId
  , opSeat :: SeatId
  , opWindow :: WindowId
  , opKind :: OpKind
  , opStart :: OutputRect
  , opBounds :: SizeBounds
  , opMoved :: Bool
  -- ^ Whether a delta has arrived, which is when a tiled window floats.
  }
  deriving (Eq, Show)

-- | The box for a cumulative pointer offset.
opBox :: PointerOp -> Int32 -> Int32 -> OutputRect
opBox op = case opKind op of
  Move -> moveBox (opStart op)
  Resize edges -> resizeBox (opBounds op) edges (opStart op)

-- | Shift the box by the offset, stopping where its far edge would leave 'Int32'.
moveBox :: OutputRect -> Int32 -> Int32 -> OutputRect
moveBox (OutputRect r) dx dy =
  boxOr
    (OutputRect r)
    (fit (toInteger (rectX r) + toInteger dx) (toInteger (rectWidth r)))
    (fit (toInteger (rectY r) + toInteger dy) (toInteger (rectHeight r)))

{- | Drag the given edges by the offset. A dragged length stays at least
@max 1 minimum@, and at most a positive maximum raised to that floor. The
edge opposite a dragged edge stays fixed unless 'Int32' cannot hold the
result, and an axis that is not dragged is unchanged.
-}
resizeBox :: SizeBounds -> ResizeEdges -> OutputRect -> Int32 -> Int32 -> OutputRect
resizeBox bounds edges (OutputRect r) dx dy =
  boxOr
    (OutputRect r)
    (axis (boundsMinWidth bounds) (boundsMaxWidth bounds) horizontal (rectX r) (rectWidth r) dx)
    (axis (boundsMinHeight bounds) (boundsMaxHeight bounds) vertical (rectY r) (rectHeight r) dy)
 where
  (horizontal, vertical) = case edges of
    Horizontal h -> (Just (nearH h), Nothing)
    Vertical v -> (Nothing, Just (nearV v))
    Corner h v -> (Just (nearH h), Just (nearV v))
  nearH h = if h == LeftEdge then NearEdge else FarEdge
  nearV v = if v == TopEdge then NearEdge else FarEdge

data AxisEdge = NearEdge | FarEdge

-- | One axis's origin and length, dragged at one edge or not at all.
axis :: Int32 -> Int32 -> Maybe AxisEdge -> Int32 -> Int32 -> Int32 -> (Integer, Integer)
axis minHint maxHint dragged origin32 len32 d32 = case dragged of
  Nothing -> (origin, len)
  Just FarEdge -> fit origin (min (clampLen (len + d)) (max lo (maxI32 - origin)))
  Just NearEdge ->
    let far = origin + len
        len' = min (clampLen (len - d)) (max lo (far - minI32))
     in fit (far - len') len'
 where
  origin = toInteger origin32
  len = toInteger len32
  d = toInteger d32
  lo = max 1 (toInteger minHint)
  hi = if maxHint > 0 then max lo (toInteger maxHint) else maxI32
  clampLen = max lo . min hi

-- | Keep a length in 'Int32' and move the origin only as far as the far edge needs.
fit :: Integer -> Integer -> (Integer, Integer)
fit origin len = (max minI32 (min (maxI32 - len') origin), len')
 where
  len' = min maxI32 len

boxOr :: OutputRect -> (Integer, Integer) -> (Integer, Integer) -> OutputRect
boxOr fallback (x, w) (y, h) = either (const fallback) OutputRect (mkSize (fromInteger w) (fromInteger h) >>= mkRect (Point (fromInteger x) (fromInteger y)))

minI32, maxI32 :: Integer
minI32 = toInteger (minBound :: Int32)
maxI32 = toInteger (maxBound :: Int32)
