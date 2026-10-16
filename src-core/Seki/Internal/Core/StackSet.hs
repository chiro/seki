module Seki.Internal.Core.StackSet (
  StackSet (..),
  Screen (..),
  Workspace (..),
  Stack (..),
  WindowId (..),
  OutputId (..),
  FloatGeom,
  integrate,
  differentiate,
) where

import Control.Applicative.Backwards (Backwards (..))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Word (Word32)
import Seki.Internal.Core.Geometry (Rect)

-- | A river window's identity.
newtype WindowId = WindowId Word32
  deriving (Eq, Ord, Show)

-- | A river output's identity.
newtype OutputId = OutputId Word32
  deriving (Eq, Ord, Show)

-- | A floating window's rectangle in global logical coordinates.
type FloatGeom = Rect

{- | Every workspace exactly once: the focused screen's, the other screens',
and the hidden ones, plus the floating windows' rectangles.
-}
data StackSet i l = StackSet
  { current :: !(Screen i l)
  , visible :: [Screen i l]
  , hidden :: [Workspace i l]
  , floating :: Map WindowId FloatGeom
  }
  deriving (Eq, Show)

-- | A workspace shown on an output.
data Screen i l = Screen
  { workspace :: !(Workspace i l)
  , output :: !OutputId
  }
  deriving (Eq, Show)

-- | A tagged workspace with its layout and its windows, if any.
data Workspace i l = Workspace
  { tag :: !i
  , layout :: l
  , stack :: Maybe (Stack WindowId)
  }
  deriving (Eq, Show)

{- | A non-empty list zipper. @up@ holds the elements above 'focus', nearest
first; @down@ holds those below it, in order.
-}
data Stack a = Stack
  { focus :: !a
  , up :: [a]
  , down :: [a]
  }
  deriving (Eq, Show, Functor)

-- | Folds in 'integrate' order.
instance Foldable Stack where
  toList = integrate
  foldr f z = foldr f z . toList

-- | Runs effects in 'integrate' order, so @up@ is traversed back to front.
instance Traversable Stack where
  traverse f s =
    flip Stack
      <$> forwards (traverse (Backwards . f) (up s))
      <*> f (focus s)
      <*> traverse f (down s)

-- | The elements from top to bottom, ignoring focus.
integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r

-- | A stack focused on the first element, or 'Nothing' for an empty list.
differentiate :: [a] -> Maybe (Stack a)
differentiate [] = Nothing
differentiate (x : xs) = Just (Stack x [] xs)
