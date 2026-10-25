{- | The state a restart carries to the next seki process (M2-D06).

The saved form names windows by their river identifier and outputs by their
@wl_output@ global name, the two keys that survive a window-manager
reconnect. It has no 'WindowId' or 'OutputId' field, since those are wire
object ids that mean nothing to the next process. The file is a version
header followed by 'show' of a 'RestoreState'.
-}
module Seki.Internal.Core.Restore (
  WindowKey (..),
  OutputKey (..),
  SavedFloat (..),
  SavedWorkspace (..),
  RestoreState (..),
  RestoreError (..),
  restoreVersion,
  encodeRestoreState,
  decodeRestoreState,
  LiveInventory (..),
  saveCore,
  RestoreNote (..),
  restoreCore,
) where

import Data.Foldable (find, foldl', toList)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (mapAccumL)
import Data.Word (Word32)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize, rectHeight, rectWidth, rectX, rectY)
import Seki.Internal.Core.StackSet (FloatGeom (..), OutputId, Screen (..), Stack, StackSet (..), WindowId, Workspace (..), mapMaybeStack)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..), fromWorkspaces, normalizeCore)
import Text.Read (readMaybe)

-- | A river window identifier (version 4). Any text reads, since a key that matches no live window is dropped at restore.
newtype WindowKey = WindowKey Text deriving (Eq, Ord, Show, Read)

-- | A @wl_output@ global name, which stays the same across a window-manager reconnect.
newtype OutputKey = OutputKey Word32 deriving (Eq, Ord, Show, Read)

{- | A float's stored box as plain numbers: x, y, width, and height relative
to its output. Restore rebuilds the box through the geometry constructors,
so a file cannot bypass their checks.
-}
data SavedFloat = SavedClientSized | SavedFloatAt Int32 Int32 Int32 Int32
  deriving (Eq, Show, Read)

data SavedWorkspace i = SavedWorkspace
  { savedTag :: i
  , savedOutput :: Maybe OutputKey
  -- ^ The output that showed it, or 'Nothing' when hidden or headless.
  , savedLayout :: String
  -- ^ 'show' of the layout, read on its own so an unreadable layout falls back alone.
  , savedStack :: Maybe (Stack WindowKey)
  }
  deriving (Eq, Show, Read)

{- | Everything a restart carries over. Workspaces come in StackSet order:
the current one first (the remembered one when headless), then the visible
ones, then the hidden ones. Unassigned outputs and the pointer op are left
out: replaying the new connection's outputs rebuilds the first, and an op
does not survive a reconnect.
-}
data RestoreState i = RestoreState
  { savedWorkspaces :: [SavedWorkspace i]
  , savedFloats :: [(WindowKey, SavedFloat)]
  -- ^ Bottom to top, so this one list is both the floating map and the raise order.
  , savedFullscreen :: Set WindowKey
  , savedSpawnedOnce :: Set String
  }
  deriving (Eq, Show, Read)

-- | Why a saved file was not used. Either way seki starts fresh.
data RestoreError = RestoreUnknownVersion String | RestoreUnreadable
  deriving (Eq, Show)

restoreVersion :: Int
restoreVersion = 1

-- | The header line @seki-restore 1@, then 'show' of the state.
encodeRestoreState :: (Show i) => RestoreState i -> String
encodeRestoreState st = header ++ "\n" ++ show st

{- | Read a file written by 'encodeRestoreState'. A header naming another
version is 'RestoreUnknownVersion', and anything else that does not read is
'RestoreUnreadable'.
-}
decodeRestoreState :: (Read i) => String -> Either RestoreError (RestoreState i)
decodeRestoreState text = case break (== '\n') text of
  (line, '\n' : body)
    | ["seki-restore", version] <- words line ->
        if version == show restoreVersion
          then maybe (Left RestoreUnreadable) Right (readMaybe body)
          else Left (RestoreUnknownVersion version)
  _ -> Left RestoreUnreadable

header :: String
header = "seki-restore " ++ show restoreVersion

{- | The keys the live session knows when restore applies or the state is
saved: windows that have an identifier and outputs that have a @wl_output@
name. The runtime builds it, so the core never sees wire types.
-}
data LiveInventory = LiveInventory
  { liveWindows :: Map WindowKey WindowId
  , liveOutputs :: Map OutputKey OutputId
  }
  deriving (Eq, Show)

{- | Key the state by the inventory. A window with no identifier is left out
and gets normal placement after the restart, and a screen whose output has
no name is saved as hidden.
-}
saveCore :: (Show l) => LiveInventory -> CoreState i l -> RestoreState i
saveCore inventory st =
  RestoreState
    { savedWorkspaces = [SavedWorkspace (tag w) (o >>= (`Map.lookup` outputKeys)) (show (layout w)) (stack w >>= mapMaybeStack (`Map.lookup` windowKeys)) | (w, o) <- shownOn]
    , savedFloats = [(k, savedFloat geom) | w <- coreRaiseOrder st, Just geom <- [Map.lookup w floats], Just k <- [Map.lookup w windowKeys]]
    , savedFullscreen = Set.fromList (mapMaybe (`Map.lookup` windowKeys) (Set.toList (coreFullscreen st)))
    , savedSpawnedOnce = coreSpawnedOnce st
    }
 where
  windowKeys = invert (liveWindows inventory)
  outputKeys = invert (liveOutputs inventory)
  invert m = Map.fromList [(v, k) | (k, v) <- Map.toList m]
  (shownOn, floats) = case coreWorkspaces st of
    AttachedState ss _ -> ([(workspace s, Just (output s)) | s <- current ss : visible ss] ++ [(w, Nothing) | w <- hidden ss], floating ss)
    HeadlessState (HeadlessWorkspaces c hs f) -> ([(w, Nothing) | w <- c : hs], f)
  savedFloat ClientSized = SavedClientSized
  savedFloat (FloatAt (OutputRect r)) = SavedFloatAt (rectX r) (rectY r) (rectWidth r) (rectHeight r)

-- | What restore changed from the saved form, one log line each.
data RestoreNote i
  = -- | Saved but no longer configured: its windows go through the manage hook.
    TagDropped i
  | -- | Configured but not saved: it starts empty with the configured layout.
    TagAdded i
  | -- | The saved layout did not read as the configured layout type.
    LayoutFallback i
  | -- | No live output has this name, so its workspace stays hidden.
    OutputUnknown OutputKey
  | -- | This many saved windows are gone.
    WindowsGone Int
  | -- | The saved box was not a valid rectangle, so the float lets its client pick its size.
    FloatBoxInvalid WindowKey
  deriving (Eq, Show)

{- | Rebuild the core over a fresh one, whose workspaces supply the configured
tags and layout. Total: any saved state, including a hand-edited one with
repeated keys, tags, or outputs, yields a core that keeps every core
invariant, since the first claim of a tag, window, or output wins.

Saved windows with no live window are dropped here, since no close event
would ever remove them. The next drain's creation events then change nothing
for restored windows and outputs, while every other window is placed
normally, its manage hook included.
-}
restoreCore :: (Eq i, Read l) => LiveInventory -> RestoreState i -> CoreState i l -> (CoreState i l, [RestoreNote i])
restoreCore inventory saved fresh = (normalizeCore restored, walkNotes ++ map TagAdded added ++ gone ++ boxNotes)
 where
  configured = case coreWorkspaces fresh of
    AttachedState ss _ -> map workspace (current ss : visible ss) ++ hidden ss
    HeadlessState (HeadlessWorkspaces c hs _) -> c : hs
  (claims, placed, walkNotes) = foldl' place (Claims [] Set.empty Set.empty, [], []) (savedWorkspaces saved)
  place (cl, acc, notes) sw = case find ((== savedTag sw) . tag) configured of
    Just base
      | savedTag sw `notElem` claimedTags cl ->
          let (windowClaims, marked) = maybe (claimedWindows cl, Nothing) (fmap Just . mapAccumL claimWindow (claimedWindows cl)) (savedStack sw)
              shownOn = savedOutput sw >>= (`Map.lookup` liveOutputs inventory) >>= \o -> if o `Set.member` claimedOutputs cl then Nothing else Just o
              (l, layoutNotes) = maybe (layout base, [LayoutFallback (savedTag sw)]) (,[]) (readMaybe (savedLayout sw))
              outputNotes = [OutputUnknown k | Just k <- [savedOutput sw], Map.notMember k (liveOutputs inventory)]
           in ( Claims (savedTag sw : claimedTags cl) windowClaims (maybe id Set.insert shownOn (claimedOutputs cl))
              , acc ++ [(Workspace (savedTag sw) l (marked >>= mapMaybeStack id), shownOn)]
              , notes ++ layoutNotes ++ outputNotes
              )
    Just _ -> (cl, acc, notes)
    Nothing -> (cl, acc, notes ++ [TagDropped (savedTag sw)])
  claimWindow claimed k = case Map.lookup k (liveWindows inventory) of
    Just w | w `Set.notMember` claimed -> (Set.insert w claimed, Just w)
    _ -> (claimed, Nothing)
  added = [tag w | w <- configured, tag w `notElem` claimedTags claims]
  everyWorkspace = placed ++ [(w{stack = Nothing}, Nothing) | w <- configured, tag w `elem` added]
  gone = [WindowsGone n | let n = Set.size (Set.filter (`Map.notMember` liveWindows inventory) savedKeys), n > 0]
  savedKeys = Set.fromList (concatMap (maybe [] toList . savedStack) (savedWorkspaces saved))
  restoredWindows = claimedWindows claims
  (floats, boxNotes) = foldl' addFloat ([], []) (savedFloats saved)
  addFloat (fs, notes) (k, f) = case Map.lookup k (liveWindows inventory) of
    Just w | w `Set.member` restoredWindows && w `notElem` map fst fs -> case floatGeom f of
      Just geom -> (fs ++ [(w, geom)], notes)
      Nothing -> (fs ++ [(w, ClientSized)], notes ++ [FloatBoxInvalid k])
    _ -> (fs, notes)
  floatGeom SavedClientSized = Just ClientSized
  floatGeom (SavedFloatAt x y width height) = either (const Nothing) (Just . FloatAt . OutputRect) (mkSize width height >>= mkRect (Point x y))
  floatMap = Map.fromList floats
  workspaces' = case [(w, o) | (w, Just o) <- everyWorkspace] of
    (c, o) : vs -> AttachedState (StackSet (Screen c o) [Screen w o' | (w, o') <- vs] [w | (w, Nothing) <- everyWorkspace] floatMap) []
    [] -> case map fst everyWorkspace of
      c : hs -> HeadlessState (HeadlessWorkspaces c hs floatMap)
      [] -> coreWorkspaces fresh
  restored =
    (fromWorkspaces workspaces')
      { coreSpawnedOnce = savedSpawnedOnce saved
      , coreRaiseOrder = map fst floats
      , coreFullscreen = Set.fromList [w | k <- Set.toList (savedFullscreen saved), Just w <- [Map.lookup k (liveWindows inventory)], w `Set.member` restoredWindows]
      }

-- | What earlier saved workspaces have taken.
data Claims i = Claims
  { claimedTags :: [i]
  , claimedWindows :: Set WindowId
  , claimedOutputs :: Set OutputId
  }
