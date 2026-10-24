{- | Layouts and the messages that change them.

Closely derived from @XMonad.Core@ and @XMonad.Layout@ in xmonad 0.18.1
(BSD-3-Clause, see @THIRD_PARTY_NOTICES.md@). Deliberate differences:

* The core is pure and has no @X@ monad, so xmonad's @runLayout@ method,
  @doLayout@, @emptyLayout@, and @handleMessage@ collapse into 'pureLayout'
  and 'pureMessage'. No M1 layout changes state while laying out; a later
  stateful layout can add a defaulted method without breaking instances.
* Layouts place 'WindowId's in 'Rect's rather than taking a window type
  parameter.
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
  SizePolicy (..),
  LayoutClass (..),
  runLayout,
  Mirror (..),
  CLR (..),
  Choose (..),
  (|||),

  -- * Layout modifiers
  LayoutModifier (..),
  ModifiedLayout (..),
  Tolerant (..),
  tolerant,

  -- * Workspace layout state
  messageCurrentLayout,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)
import Seki.Internal.Core.Geometry (Rect, mirror)
import Seki.Internal.Core.StackSet (Screen (..), Stack (..), StackSet (..), WindowId, Workspace (..))

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

{- | What a tile shows when its client commits a size other than the tile:
'Crop' clips the content to the tile, and 'Tolerate' shows the committed size
at the tile's top-left and proposes the tile clamped to the client's size hints.
-}
data SizePolicy = Crop | Tolerate
  deriving (Eq, Show)

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

  sizePolicy :: l -> SizePolicy
  sizePolicy _ = Crop

-- | Lay out a workspace's windows; a workspace without windows places none.
runLayout :: (LayoutClass l) => Workspace i l -> Rect -> [(WindowId, Rect)]
runLayout (Workspace _ l ms) r = maybe [] (pureLayout l r) ms

-- | A layout with its x and y axes exchanged.
newtype Mirror l = Mirror l
  deriving (Eq, Show)

instance (LayoutClass l) => LayoutClass (Mirror l) where
  pureLayout (Mirror l) r s = map (second mirror) (pureLayout l (mirror r) s)
  pureMessage (Mirror l) m = Mirror <$> pureMessage l m
  description (Mirror l) = "Mirror " ++ description l
  sizePolicy (Mirror l) = sizePolicy l

-- | Which layout of a 'Choose' is selected.
data CLR = CL | CR
  deriving (Eq, Show)

-- | Two layouts with one selected; 'NextLayout' and 'FirstLayout' switch.
data Choose l r = Choose CLR l r
  deriving (Eq, Show)

-- | Choose between two layouts, starting with the left one.
(|||) :: l -> r -> Choose l r
(|||) = Choose CL

infixr 5 |||

-- | Selects the next layout without wrapping; 'NextLayout' wraps with it.
data NextNoWrap = NextNoWrap
  deriving (Eq, Show)

instance Message NextNoWrap

instance (LayoutClass l, LayoutClass r) => LayoutClass (Choose l r) where
  pureLayout (Choose CL l _) = pureLayout l
  pureLayout (Choose CR _ r) = pureLayout r

  description (Choose CL l _) = description l
  description (Choose CR _ r) = description r

  sizePolicy (Choose CL l _) = sizePolicy l
  sizePolicy (Choose CR _ r) = sizePolicy r

  pureMessage lr m
    | Just NextLayout <- fromMessage m = handle lr NextNoWrap <|> handle lr FirstLayout
  pureMessage c@(Choose d l r) m
    | Just NextNoWrap <- fromMessage m = case d of
        CL -> case handle l NextNoWrap of
          ml@(Just _) -> choose c CL ml Nothing
          Nothing -> choose c CR Nothing (handle r FirstLayout)
        CR -> choose c CR Nothing (handle r NextNoWrap)
  pureMessage c@(Choose _ l _) m
    | Just FirstLayout <- fromMessage m = choose c CL (handle l FirstLayout) Nothing
  pureMessage c@(Choose CL l _) m = choose c CL (pureMessage l m) Nothing
  pureMessage c@(Choose CR _ r) m = choose c CR Nothing (pureMessage r m)

handle :: (LayoutClass l, Message m) => l -> m -> Maybe l
handle l m = pureMessage l (SomeMessage m)

{- | Select side @d'@ given the sides' answers to a message. Switching sides
sends 'Hide' to the side switched away from.
-}
choose :: (LayoutClass l, LayoutClass r) => Choose l r -> CLR -> Maybe l -> Maybe r -> Maybe (Choose l r)
choose (Choose d _ _) d' Nothing Nothing | d == d' = Nothing
choose (Choose d l r) d' ml mr =
  Just
    ( case (d, d') of
        (CL, CR) -> Choose d' (hide l') r'
        (CR, CL) -> Choose d' l' (hide r')
        _ -> Choose d' l' r'
    )
 where
  l' = fromMaybe l ml
  r' = fromMaybe r mr
  hide x = fromMaybe x (handle x Hide)

-- | A layout changed by a modifier @m@.
class (Show m) => LayoutModifier m where
  modifySizePolicy :: m -> SizePolicy -> SizePolicy
  modifySizePolicy _ = id

-- | A layout with a modifier, which keeps the layout's placements and messages.
data ModifiedLayout m l = ModifiedLayout m l
  deriving (Eq, Show)

instance (LayoutModifier m, LayoutClass l) => LayoutClass (ModifiedLayout m l) where
  pureLayout (ModifiedLayout _ l) = pureLayout l
  pureMessage (ModifiedLayout m l) msg = ModifiedLayout m <$> pureMessage l msg
  description (ModifiedLayout _ l) = description l
  sizePolicy (ModifiedLayout m l) = modifySizePolicy m (sizePolicy l)

-- | The modifier behind 'tolerant', named apart from the 'Tolerate' policy.
data Tolerant = Tolerant
  deriving (Eq, Show)

instance LayoutModifier Tolerant where
  modifySizePolicy _ _ = Tolerate

-- | Tolerate client sizes in a layout, like xmonad's @layoutHints@.
tolerant :: l -> ModifiedLayout Tolerant l
tolerant = ModifiedLayout Tolerant

{- | Send a message to the current workspace's layout and keep the result.
Other workspaces keep their own layout state, and a message the layout
ignores leaves the StackSet unchanged.
-}
messageCurrentLayout :: (LayoutClass l) => SomeMessage -> StackSet i l -> StackSet i l
messageCurrentLayout m s = case pureMessage (layout ws) m of
  Nothing -> s
  Just l -> s{current = (current s){workspace = ws{layout = l}}}
 where
  ws = workspace (current s)
