module Seki.Internal.Core.StackSet (
  Stack (..),
  integrate,
  differentiate,
) where

import Control.Applicative.Backwards (Backwards (..))
import Data.Foldable (toList)

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
