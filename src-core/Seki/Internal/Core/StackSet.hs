{- | The workspace, screen, and window zipper behind seki's tiling state.

Closely derived from @XMonad.StackSet@ in xmonad 0.18.1 (BSD-3-Clause, see
@THIRD_PARTY_NOTICES.md@). The operations keep xmonad's semantics. Deliberate
differences:

* Windows are 'WindowId' and screens are 'OutputId', river object
  identities, rather than type parameters.
* A 'Screen' carries no geometry. The scene planner reads each output's
  usable rectangle separately.
* 'FloatGeom' is an absolute 'Rect', not a @RationalRect@.
* There is no @new@ and no @abort@. Construction is total and lives in
  "Seki.Internal.Core.State", which returns typed errors.
* The workspace set is fixed once validated, so @renameTag@ and
  @ensureTags@ are not ported.
-}
module Seki.Internal.Core.StackSet (
  StackSet (..),
  Screen (..),
  Workspace (..),
  Stack (..),
  WindowId (..),
  OutputId (..),
  FloatGeom,

  -- * Observation
  peek,
  index,
  integrate,
  integrate',
  differentiate,
  currentTag,
  member,
  findTag,
  screens,
  workspaces,
  allWindows,

  -- * Focus
  focusUp,
  focusDown,
  focusMaster,
  focusWindow,

  -- * Ordering
  swapUp,
  swapDown,
  swapMaster,
  shiftMaster,

  -- * Membership
  insertUp,
  delete,
  filter,
  shift,
  shiftWin,
  tagMember,
  lookupWorkspace,

  -- * Workspaces
  view,
  greedyView,
  mapWorkspace,
  mapLayout,
) where

import Prelude hiding (filter)

import Control.Applicative.Backwards (Backwards (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Maybe (isJust, listToMaybe)
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

-- | The stack's elements, or none.
integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate

{- | Focus workspace @i@. A visible workspace swaps screens with the current
one; a hidden workspace replaces the current one on its screen. An unknown tag
changes nothing.
-}
view :: (Eq i) => i -> StackSet i l -> StackSet i l
view i s
  | i == currentTag s = s
  | Just x <- L.find ((i ==) . tag . workspace) (visible s) =
      s{current = x, visible = current s : L.deleteBy (equating output) x (visible s)}
  | Just x <- L.find ((i ==) . tag) (hidden s) =
      s
        { current = (current s){workspace = x}
        , hidden = workspace (current s) : L.deleteBy (equating tag) x (hidden s)
        }
  | otherwise = s
 where
  equating f x y = f x == f y

with :: b -> (Stack WindowId -> b) -> StackSet i l -> b
with dflt f = maybe dflt f . stack . workspace . current

modify :: Maybe (Stack WindowId) -> (Stack WindowId -> Maybe (Stack WindowId)) -> StackSet i l -> StackSet i l
modify d f s = s{current = (current s){workspace = (workspace (current s)){stack = with d f s}}}

modify' :: (Stack WindowId -> Stack WindowId) -> StackSet i l -> StackSet i l
modify' f = modify Nothing (Just . f)

-- | The focused window of the current workspace.
peek :: StackSet i l -> Maybe WindowId
peek = with Nothing (Just . focus)

-- | The current workspace's windows from top to bottom.
index :: StackSet i l -> [WindowId]
index = with [] integrate

-- | Move focus up the current stack, wrapping from the top to the bottom.
focusUp :: StackSet i l -> StackSet i l
focusUp = modify' focusUp'

focusUp' :: Stack a -> Stack a
focusUp' (Stack t (l : ls) rs) = Stack l ls (t : rs)
focusUp' (Stack t [] rs) = Stack x xs []
 where
  x :| xs = NE.reverse (t :| rs)

-- | The current screen's workspace, then the visible ones, then the hidden ones.
workspaces :: StackSet i l -> [Workspace i l]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

-- | Every window on every workspace, without duplicates.
allWindows :: StackSet i l -> [WindowId]
allWindows = nubOrd . concatMap (integrate' . stack) . workspaces

currentTag :: StackSet i l -> i
currentTag = tag . workspace . current

member :: WindowId -> StackSet i l -> Bool
member a s = isJust (findTag a s)

-- | The tag of the workspace holding the window.
findTag :: WindowId -> StackSet i l -> Maybe i
findTag a s = listToMaybe [tag w | w <- workspaces s, a `elem` integrate' (stack w)]

{- | Insert a window above the focus of the current workspace and focus it.
A window already in the StackSet changes nothing.
-}
insertUp :: WindowId -> StackSet i l -> StackSet i l
insertUp a s
  | member a s = s
  | otherwise = modify (Just (Stack a [] [])) (\(Stack t l r) -> Just (Stack a l (t : r))) s

{- | Like 'view', but a visible workspace moves onto the current screen and
the current workspace takes its place.
-}
greedyView :: (Eq i) => i -> StackSet i l -> StackSet i l
greedyView w ws
  | any wTag (hidden ws) = view w ws
  | Just s <- L.find (wTag . workspace) (visible ws) =
      ws
        { current = (current ws){workspace = workspace s}
        , visible = s{workspace = workspace (current ws)} : L.filter (not . wTag . workspace) (visible ws)
        }
  | otherwise = ws
 where
  wTag = (w ==) . tag

-- | The current screen, then the visible ones.
screens :: StackSet i l -> [Screen i l]
screens s = current s : visible s

-- | Whether a workspace has the tag.
tagMember :: (Eq i) => i -> StackSet i l -> Bool
tagMember t = elem t . map tag . workspaces

-- | Move focus down the current stack, wrapping from the bottom to the top.
focusDown :: StackSet i l -> StackSet i l
focusDown = modify' focusDown'

focusDown' :: Stack a -> Stack a
focusDown' = reverseStack . focusUp' . reverseStack

reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls

-- | Focus the top window of the current stack, keeping the window order.
focusMaster :: StackSet i l -> StackSet i l
focusMaster = modify' $ \c -> case c of
  Stack _ [] _ -> c
  Stack t (l : ls) rs -> Stack x [] (xs ++ t : rs)
   where
    x :| xs = NE.reverse (l :| ls)

{- | Focus a window, viewing its workspace first. A window outside the
StackSet changes nothing.
-}
focusWindow :: (Eq i) => WindowId -> StackSet i l -> StackSet i l
focusWindow w s
  | Just w == peek s = s
  | otherwise = maybe s (\n -> until ((Just w ==) . peek) focusUp (view n s)) (findTag w s)

-- | Swap the focused window with the one above it, wrapping at the top.
swapUp :: StackSet i l -> StackSet i l
swapUp = modify' swapUp'

-- | Swap the focused window with the one below it, wrapping at the bottom.
swapDown :: StackSet i l -> StackSet i l
swapDown = modify' (reverseStack . swapUp' . reverseStack)

swapUp' :: Stack a -> Stack a
swapUp' (Stack t (l : ls) rs) = Stack t ls (l : rs)
swapUp' (Stack t [] rs) = Stack t (reverse rs) []

-- | Swap the focused window with the master window, keeping focus.
swapMaster :: StackSet i l -> StackSet i l
swapMaster = modify' $ \c -> case c of
  Stack _ [] _ -> c
  Stack t (l : ls) rs -> Stack t [] (xs ++ x : rs)
   where
    x :| xs = NE.reverse (l :| ls)

{- | Remove a window from every workspace. When it had focus, focus moves to
the window below it, or above it when it was at the bottom.
-}
delete :: WindowId -> StackSet i l -> StackSet i l
delete = delete'

delete' :: WindowId -> StackSet i l -> StackSet i l
delete' w s =
  s
    { current = removeFromScreen (current s)
    , visible = map removeFromScreen (visible s)
    , hidden = map removeFromWorkspace (hidden s)
    }
 where
  removeFromWorkspace ws = ws{stack = stack ws >>= filter (/= w)}
  removeFromScreen scr = scr{workspace = removeFromWorkspace (workspace scr)}

{- | Keep the elements that satisfy the predicate, in order. Focus stays if it
survives, else moves to the next element below, else the nearest above.
-}
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack f ls rs) = case L.filter p (f : rs) of
  f' : rs' -> Just (Stack f' (L.filter p ls) rs')
  [] -> case L.filter p ls of
    f' : ls' -> Just (Stack f' ls' [])
    [] -> Nothing

-- | Move the focused window to the top of the stack, keeping focus.
shiftMaster :: StackSet i l -> StackSet i l
shiftMaster = modify' $ \c -> case c of
  Stack _ [] _ -> c
  Stack t ls rs -> Stack t [] (reverse ls ++ rs)

-- | Move the focused window to workspace @n@, keeping the current workspace.
shift :: (Eq i) => i -> StackSet i l -> StackSet i l
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

{- | Move a window to workspace @n@ and focus it there. An unknown window or
tag, or a window already on @n@, changes nothing.
-}
shiftWin :: (Eq i) => i -> WindowId -> StackSet i l -> StackSet i l
shiftWin n w s = case findTag w s of
  Just from | n `tagMember` s && n /= from -> go from s
  _ -> s
 where
  go from = onWorkspace n (insertUp w) . onWorkspace from (delete' w)

onWorkspace :: (Eq i) => i -> (StackSet i l -> StackSet i l) -> StackSet i l -> StackSet i l
onWorkspace n f s = view (currentTag s) . f . view n $ s

-- | The tag of the workspace shown on an output.
lookupWorkspace :: OutputId -> StackSet i l -> Maybe i
lookupWorkspace o s = listToMaybe [tag w | Screen w o' <- screens s, o' == o]

-- | Apply a function to every workspace.
mapWorkspace :: (Workspace i l -> Workspace i l) -> StackSet i l -> StackSet i l
mapWorkspace f s =
  s
    { current = updScr (current s)
    , visible = map updScr (visible s)
    , hidden = map f (hidden s)
    }
 where
  updScr scr = scr{workspace = f (workspace scr)}

-- | Apply a function to every workspace's layout.
mapLayout :: (l -> l') -> StackSet i l -> StackSet i l'
mapLayout f (StackSet v vs hs m) = StackSet (fScreen v) (map fScreen vs) (map fWorkspace hs) m
 where
  fScreen (Screen ws o) = Screen (fWorkspace ws) o
  fWorkspace (Workspace t l s) = Workspace t (f l) s
