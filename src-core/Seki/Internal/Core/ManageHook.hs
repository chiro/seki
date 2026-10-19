{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Pure manage hooks over a new window's properties.

Ported from @XMonad.ManageHook@ in xmonad 0.18.1 and 'doSink' from
@XMonad.Hooks.ManageHelpers@ in xmonad-contrib 0.18.1 (BSD-3-Clause, see
@THIRD_PARTY_NOTICES.md@). A 'Query' reads the managed window and its
'WindowProps' and nothing else: it has no @liftX@ and runs no IO.

The runtime reads 'WindowProps' from its inventory at @manage_start@. River
sends a window's @app_id@, @title@, @parent@, and @dimensions_hint@ before the
@manage_start@ that follows the window's creation, so a hook sees what the
client set before its first commit. A later change does not rerun the hook.

Deliberate differences:

* 'appId' stands in for @className@. Wayland clients report an @app_id@ and
  Xwayland clients report their class through the same field. A missing
  @app_id@ reads as the empty string, as @className@ reads a missing
  @WM_CLASS@, so a hook keyed on a non-empty name does not match it.
* 'isDialog' means the window has a parent. River reports no window type, so
  contrib's @_NET_WM_WINDOW_TYPE_DIALOG@ test has no equivalent.
-}
module Seki.Internal.Core.ManageHook (
  WindowProps (..),
  DimensionsHint (..),
  noWindowProps,
  Query,
  ManageHook,
  runQuery,
  askWindow,
  appId,
  title,
  isDialog,
  (=?),
  (-->),
  composeAll,
  idHook,
  doF,
  doShift,
  doSink,
  doFloat,
) where

import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Seki.Internal.Core.StackSet (FloatGeom (..), StackSet, WindowId, float, shiftWin, sink)

-- | A new window's properties, each absent until the client sets it.
data WindowProps = WindowProps
  { propAppId :: Maybe Text
  , propTitle :: Maybe Text
  , propParent :: Maybe WindowId
  , propDimensionsHint :: Maybe DimensionsHint
  }
  deriving (Eq, Show)

-- | River's @dimensions_hint@ in logical pixels, where 0 means no preference.
data DimensionsHint = DimensionsHint
  { hintMinWidth :: Int32
  , hintMinHeight :: Int32
  , hintMaxWidth :: Int32
  , hintMaxHeight :: Int32
  }
  deriving (Eq, Show)

noWindowProps :: WindowProps
noWindowProps = WindowProps Nothing Nothing Nothing Nothing

-- | A pure question about the window being managed.
newtype Query a = Query ((WindowId, WindowProps) -> a)
  deriving newtype (Functor, Applicative, Monad, Semigroup, Monoid)

-- | A change to apply to the StackSet once the window is inserted.
type ManageHook i l = Query (Endo (StackSet i l))

runQuery :: Query a -> WindowId -> WindowProps -> a
runQuery (Query f) = curry f

-- | The window being managed, xmonad's @ask@.
askWindow :: Query WindowId
askWindow = Query fst

fromProps :: (WindowProps -> a) -> Query a
fromProps f = Query (f . snd)

-- | The window's @app_id@, or the empty string when it set none.
appId :: Query String
appId = fromProps (maybe "" Text.unpack . propAppId)

-- | The window's title, or the empty string when it set none.
title :: Query String
title = fromProps (maybe "" Text.unpack . propTitle)

-- | Whether the window has a parent window.
isDialog :: Query Bool
isDialog = fromProps (isJust . propParent)

(=?) :: (Eq a) => Query a -> a -> Query Bool
q =? x = fmap (== x) q

infix 0 -->

-- | Run the hook only when the predicate holds.
(-->) :: (Monad m, Monoid a) => m Bool -> m a -> m a
p --> f = p >>= \b -> if b then f else pure mempty

-- | Combine hooks. Their 'Endo's compose, so the last hook in the list applies first, as in xmonad.
composeAll :: (Monoid m) => [m] -> m
composeAll = mconcat

idHook :: (Monoid m) => m
idHook = mempty

doF :: (s -> s) -> Query (Endo s)
doF = pure . Endo

-- | Move the window to the workspace with this tag, keeping focus where it was.
doShift :: (Eq i) => i -> ManageHook i l
doShift i = doF . shiftWin i =<< askWindow

-- | Tile the window, dropping any floating rectangle.
doSink :: ManageHook i l
doSink = doF . sink =<< askWindow

{- | Float the window with no stored box, so it is proposed no size, keeps the
size its client commits, and is centered on its output. xmonad's @doFloat@
keeps the X11 window's own geometry instead, which a new river window does not
have yet.
-}
doFloat :: ManageHook i l
doFloat = doF . (`float` ClientSized) =<< askWindow
