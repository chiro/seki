{- | Layouts and the messages that change them.

Closely derived from @XMonad.Core@ and @XMonad.Layout@ in xmonad 0.18.1
(BSD-3-Clause, see @THIRD_PARTY_NOTICES.md@). Deliberate differences:

* The core is pure and has no @X@ monad, so xmonad's @runLayout@ method,
  @doLayout@, @emptyLayout@, and @handleMessage@ collapse into 'pureLayout'
  and 'pureMessage'. No M1 layout changes state while laying out; a later
  stateful layout can add a defaulted method without breaking instances.
* Layouts place 'WindowId's in 'Rect's rather than taking a window type
  parameter.
* There is no @sizePolicy@ method yet; @Crop@ and @Tolerate@ arrive in M2.
* @ReleaseResources@, @JumpToLayout@, and X11 event messages are not ported,
  since seki has no restart or name-based layout selection in M1.
-}
module Seki.Internal.Core.Layout (
  -- * Messages
  Message,
  SomeMessage (..),
  fromMessage,
  Resize (..),
  IncMasterN (..),
  ChangeLayout (..),
  LayoutMessages (..),

  -- * Layouts
  LayoutClass (..),
  runLayout,
) where

import Data.Typeable (Typeable, cast)
import Seki.Internal.Core.Geometry (Rect)
import Seki.Internal.Core.StackSet (Stack (..), WindowId, Workspace (..))

-- | A value a layout can receive through 'SomeMessage'.
class (Typeable a) => Message a

-- | Any 'Message', to be recovered with 'fromMessage'.
data SomeMessage = forall a. (Message a) => SomeMessage a

fromMessage :: (Message m) => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- | Shrink or expand the master area.
data Resize = Shrink | Expand

instance Message Resize

-- | Change the number of master windows by the given amount.
newtype IncMasterN = IncMasterN Int

instance Message IncMasterN

-- | Select the first layout of a 'Seki.Internal.Core.Layout.Choose', or the next one.
data ChangeLayout = FirstLayout | NextLayout
  deriving (Eq, Show)

instance Message ChangeLayout

-- | Messages every layout may receive.
data LayoutMessages
  = -- | The layout is no longer selected.
    Hide
  deriving (Eq, Show)

instance Message LayoutMessages

class (Show l) => LayoutClass l where
  -- | Place windows of a non-empty stack. Windows left out are hidden.
  pureLayout :: l -> Rect -> Stack WindowId -> [(WindowId, Rect)]
  pureLayout _ r s = [(focus s, r)]

  -- | The changed layout, or 'Nothing' when the message does not apply.
  pureMessage :: l -> SomeMessage -> Maybe l
  pureMessage _ _ = Nothing

  -- | A human-readable name.
  description :: l -> String
  description = show

-- | Lay out a workspace's windows; a workspace without windows places none.
runLayout :: (LayoutClass l) => Workspace i l -> Rect -> [(WindowId, Rect)]
runLayout (Workspace _ l ms) r = maybe [] (pureLayout l r) ms
