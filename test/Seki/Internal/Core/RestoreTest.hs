module Seki.Internal.Core.RestoreTest (tests) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Seki.Internal.Core.Event (CoreEvent (..), drainCore)
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize)
import Seki.Internal.Core.Layout (CLR (..), ChangeLayout (..), Choose (..), IncMasterN (..), LayoutClass (..), Mirror (..), ModifiedLayout, Resize (..), SomeMessage (..), Tolerant, tolerant)
import Seki.Internal.Core.ManageHook (ManageHook, doShift, idHook, noWindowProps)
import Seki.Internal.Core.Restore
import Seki.Internal.Core.StackSet (FloatGeom (..), OutputId (..), Screen (..), Stack (..), StackSet (..), WindowId (..), Workspace (..), integrate')
import Seki.Internal.Core.StackSetGen (DefaultLayout, EventHistory (..), EventStep (..), coreFloating, coreInvariant, historyStart, sortedWorkspaces)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..), newCoreState)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Test.QuickCheck (Gen, Property, arbitrary, checkCoverage, chooseInt, conjoin, counterexample, cover, elements, forAll, listOf, oneof, sublistOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Core restore"
    [ testProperty "a layout reached by messages reads back from its show" $
        forAll (messaged defaultLayout) $
          \l -> read (show l) === l
    , testProperty "a tolerant layout reached by messages reads back from its show" $
        forAll (messaged (tolerant defaultLayout)) $
          \l -> read (show l) === (l :: ModifiedLayout Tolerant DefaultLayout)
    , testProperty "a saved state decodes from its encoding" $ forAll restoreStates $ \s -> decodeRestoreState (encodeRestoreState s) === Right s
    , testProperty "a saved generated core decodes from its encoding" $ \h ->
        let st = finalCore h
         in forAll (inventoryOf st) $ \inv -> let s = saveCore inv st in decodeRestoreState (encodeRestoreState s) === Right s
    , testCase "an encoding starts with the version header" $
        takeWhile (/= '\n') (encodeRestoreState emptyState) @?= "seki-restore 1"
    , testCase "an unknown version is refused by name" $
        decodeInt ("seki-restore 99\n" <> dropWhile (/= 'R') (encodeRestoreState emptyState)) @?= Left (RestoreUnknownVersion "99")
    , testCase "a file without the header is unreadable" $
        decodeInt (show emptyState) @?= Left RestoreUnreadable
    , testCase "a known version with an unreadable body is unreadable" $
        decodeInt "seki-restore 1\nnot a state" @?= Left RestoreUnreadable
    , testProperty "survivors keep their place, gone windows are dropped, and new windows place normally" (checkCoverage propKeepsSurvivors)
    , testProperty "any saved state restores over any inventory to a valid core" $
        forAll restoreStates $ \saved -> forAll liveInventories $ \inv ->
          restoredInvariant [1, 2, 3] (fst (restoreCore inv saved (freshCore [1, 2, 3])))
    , testCase "a workspace whose output is unknown stays hidden" testUnknownOutput
    , testCase "a headless save shows the remembered workspace on the first output" testHeadless
    , testCase "a dropped tag's windows go through the manage hook" testDroppedTag
    , testCase "a layout that does not read falls back to the configured layout" testLayoutFallback
    , testCase "without identifiers every window is new and goes through the manage hook" testNoIdentifiers
    , testCase "an invalid float box lets the client pick its size" testInvalidFloatBox
    ]
 where
  emptyState = RestoreState [] [] Set.empty Set.empty :: RestoreState Int
  decodeInt = decodeRestoreState :: String -> Either RestoreError (RestoreState Int)

finalCore :: EventHistory -> CoreState Int DefaultLayout
finalCore h = fst (drainCore idHook [e | EventStep _ e <- historySteps h] (historyStart h))

freshCore :: [Int] -> CoreState Int DefaultLayout
freshCore = either (error . show) id . newCoreState defaultLayout

keyOf :: WindowId -> WindowKey
keyOf (WindowId w) = WindowKey (Text.pack ("id-" ++ show w))

-- | Names for the generated outputs, and identifiers for some managed windows, as a version 4 river gives.
inventoryOf :: CoreState Int l -> Gen LiveInventory
inventoryOf st = do
  named <- sublistOf (managed st)
  pure (LiveInventory (Map.fromList [(keyOf w, w) | w <- named]) (Map.fromList [(OutputKey (100 + n), OutputId n) | n <- [1 .. 4]]))

managed :: CoreState i l -> [WindowId]
managed st = concatMap (integrate' . stack) (allWorkspaces (coreWorkspaces st))

allWorkspaces :: Workspaces i l -> [Workspace i l]
allWorkspaces (HeadlessState (HeadlessWorkspaces c hs _)) = c : hs
allWorkspaces (AttachedState ss _) = map workspace (current ss : visible ss) ++ hidden ss

{- | Save a generated core keyed by identifier, then restore it where a random
subset of its windows survive under new object ids, with new windows and
renumbered outputs, as the next process sees them.
-}
propKeepsSurvivors :: EventHistory -> Property
propKeepsSurvivors h = forAll ((,) <$> sublistOf (managed st) <*> chooseInt (0, 3)) $ \(survivors, newCount) ->
  let newIds = [WindowId (2000 + fromIntegral i) | i <- [1 .. newCount :: Int]]
      inv =
        LiveInventory
          (Map.fromList ([(keyOf w, renumber w) | w <- survivors] ++ [(WindowKey (Text.pack ("new-" ++ show w)), w) | w <- newIds]))
          (Map.fromList [(OutputKey (100 + n), OutputId (500 + n)) | n <- [1 .. 4]])
      survivorKeys = Set.fromList (map keyOf survivors)
      (restored, _) = restoreCore inv saved (historyStart h)
      again = saveCore inv restored
      shownOutputs = case coreWorkspaces restored of
        AttachedState ss _ -> map output (current ss : visible ss)
        HeadlessState _ -> []
      outputEvents = [OutputCreated o | o <- Map.elems (liveOutputs inv)]
      newOutputEvents = [OutputCreated o | o <- Map.elems (liveOutputs inv), o `notElem` shownOutputs]
      windowEvents ws = [WindowCreated w noWindowProps | w <- ws]
      (drained, _) = drainCore hook (outputEvents ++ windowEvents (Map.elems (liveWindows inv))) restored
      (onlyNew, _) = drainCore hook (newOutputEvents ++ windowEvents newIds) restored
      keep k = if k `Set.member` survivorKeys then Just k else Nothing
   in cover 5 (any ((`Set.member` survivorKeys) . fst) (savedFloats saved)) "a surviving float"
        . cover 2 (any (`Set.member` survivorKeys) (savedFullscreen saved)) "a surviving fullscreen window"
        . cover 10 (length survivors < length (managed st)) "a gone window"
        $ conjoin
          [ counterexample "restored core" (restoredInvariant (historyTags h) restored)
          , counterexample "drained core" (restoredInvariant (historyTags h) drained)
          , counterexample "no old object id leaks" (all (`elem` map renumber survivors) (managed restored))
          , counterexample "survivors keep tag, order, and focus; gone windows are dropped" $
              Map.fromList [(savedTag w, savedStack w) | w <- savedWorkspaces again]
                === Map.fromList [(savedTag w, savedStack w >>= keepKeys keep) | w <- savedWorkspaces saved]
          , counterexample "workspaces keep their outputs, order, and layouts" $
              map shape (savedWorkspaces again) === map shape (savedWorkspaces saved)
          , counterexample "survivors keep their float boxes" $
              Map.fromList (savedFloats again) === Map.fromList [f | f@(k, _) <- savedFloats saved, k `Set.member` survivorKeys]
          , counterexample "survivors stay fullscreen" $
              savedFullscreen again === Set.intersection survivorKeys (savedFullscreen saved)
          , savedSpawnedOnce again === savedSpawnedOnce saved
          , counterexample "replayed creation leaves survivors and shown outputs alone and places new windows by the hook" (drained === onlyNew)
          ]
 where
  st = finalCore h
  saved = saveCore (LiveInventory (Map.fromList [(keyOf w, w) | w <- managed st]) (Map.fromList [(OutputKey (100 + n), OutputId n) | n <- [1 .. 4]])) st
  renumber (WindowId w) = WindowId (1000 + w)
  shape w = (savedTag w, savedOutput w, savedLayout w)
  hook = doShift (last (historyTags h))

-- | xmonad's delete rule written out: focus stays, else the next below, else the nearest above.
keepKeys :: (a -> Maybe b) -> Stack a -> Maybe (Stack b)
keepKeys f (Stack x u d) = case (f x, mapMaybe f u, mapMaybe f d) of
  (Just x', u', d') -> Just (Stack x' u' d')
  (Nothing, u', y : d') -> Just (Stack y u' d')
  (Nothing, y : u', []) -> Just (Stack y u' [])
  (Nothing, [], []) -> Nothing

-- | The core invariants restore must keep, beyond the StackSet's own.
restoredInvariant :: [Int] -> CoreState Int l -> Property
restoredInvariant configured st =
  conjoin
    [ coreInvariant configured ws
    , counterexample "floats are managed windows" (all (`elem` managed st) (Map.keys floats))
    , counterexample "the raise order holds each float once" (sort (coreRaiseOrder st) === Map.keys floats)
    , counterexample "fullscreen windows focus their workspace" (all (`elem` focuses) (Set.toList (coreFullscreen st)))
    , counterexample "no pointer op survives" (isNothing (corePointerOp st))
    ]
 where
  ws = coreWorkspaces st
  floats = coreFloating ws
  focuses = [focus s | Just s <- map stack (sortedWorkspaces ws)]

-- | Any inventory, including two keys on one window and two names on one output.
liveInventories :: Gen LiveInventory
liveInventories =
  LiveInventory
    <$> (Map.fromList <$> listOf ((,) <$> key <*> (WindowId . fromIntegral <$> chooseInt (1, 4))))
    <*> (Map.fromList <$> listOf ((,) <$> (OutputKey . fromIntegral <$> chooseInt (0, 3)) <*> (OutputId . fromIntegral <$> chooseInt (1, 3))))

-- | Any saved state, including keys and tags no live session would produce.
restoreStates :: Gen (RestoreState Int)
restoreStates = RestoreState <$> listOf workspace <*> listOf ((,) <$> key <*> saved) <*> (Set.fromList <$> listOf key) <*> (Set.fromList <$> listOf command)
 where
  workspace =
    SavedWorkspace
      <$> chooseInt (0, 4)
      <*> oneof [pure Nothing, Just . OutputKey . fromIntegral <$> chooseInt (0, 3)]
      <*> elements ["Full", show defaultLayout, "garbage"]
      <*> oneof [pure Nothing, Just <$> stack]
  stack = Stack <$> key <*> listOf key <*> listOf key
  saved = oneof [pure SavedClientSized, SavedFloatAt <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]
  command = elements ["foot", "waybar -c \"x y\"", "printf '%s\\n' \12354"]

key :: Gen WindowKey
key = WindowKey . Text.pack <$> elements ["c68415ce", "9b107380", "", "with space", "\"quoted\"", "\12354"]

defaultLayout :: DefaultLayout
defaultLayout = Choose CL tall (Choose CL (Mirror tall) Full)
 where
  tall = Tall 1 (3 / 100) (1 / 2)

messaged :: (LayoutClass l) => l -> Gen l
messaged l0 = foldl' (\l m -> fromMaybe l (pureMessage l m)) l0 <$> listOf message
 where
  message =
    oneof
      [ elements [SomeMessage NextLayout, SomeMessage FirstLayout, SomeMessage Shrink, SomeMessage Expand]
      , SomeMessage . IncMasterN <$> chooseInt (-2, 2)
      ]

workspaceAt :: Int -> Maybe OutputKey -> [String] -> SavedWorkspace Int
workspaceAt t o ks = SavedWorkspace t o (show defaultLayout) (case map (WindowKey . Text.pack) ks of [] -> Nothing; k : rest -> Just (Stack k [] rest))

savedOf :: [SavedWorkspace Int] -> RestoreState Int
savedOf ws = RestoreState ws [] Set.empty Set.empty

windowsOf :: [(String, Int)] -> Map.Map WindowKey WindowId
windowsOf ws = Map.fromList [(WindowKey (Text.pack k), WindowId (fromIntegral w)) | (k, w) <- ws]

stacks :: CoreState Int l -> [(Int, [WindowId])]
stacks st = [(tag w, integrate' (stack w)) | w <- allWorkspaces (coreWorkspaces st)]

testUnknownOutput :: Assertion
testUnknownOutput = do
  let saved = savedOf [workspaceAt 1 (Just (OutputKey 1)) ["a"], workspaceAt 2 (Just (OutputKey 2)) ["b"], workspaceAt 3 Nothing []]
      inv = LiveInventory (windowsOf [("a", 5), ("b", 6)]) (Map.singleton (OutputKey 1) (OutputId 10))
      (st, notes) = restoreCore inv saved (freshCore [1, 2, 3])
  notes @?= [OutputUnknown (OutputKey 2)]
  case coreWorkspaces st of
    AttachedState ss [] -> do
      (tag (workspace (current ss)), output (current ss)) @?= (1, OutputId 10)
      map (tag . workspace) (visible ss) @?= []
      [(tag w, integrate' (stack w)) | w <- hidden ss] @?= [(2, [WindowId 6]), (3, [])]
    other -> assertFailure ("expected workspace 1 shown alone, got " <> show other)

testHeadless :: Assertion
testHeadless = do
  let saved = savedOf [workspaceAt 2 Nothing ["a"], workspaceAt 1 Nothing [], workspaceAt 3 Nothing []]
      inv = LiveInventory (windowsOf [("a", 5)]) (Map.singleton (OutputKey 1) (OutputId 10))
      (st, notes) = restoreCore inv saved (freshCore [1, 2, 3])
      (shown, _) = drainCore idHook [OutputCreated (OutputId 10), WindowCreated (WindowId 5) noWindowProps] st
  notes @?= []
  case coreWorkspaces shown of
    AttachedState ss [] -> (tag (workspace (current ss)), integrate' (stack (workspace (current ss))), output (current ss)) @?= (2, [WindowId 5], OutputId 10)
    other -> assertFailure ("expected workspace 2 shown, got " <> show other)

testDroppedTag :: Assertion
testDroppedTag = do
  let saved = savedOf [workspaceAt 1 (Just (OutputKey 1)) [], workspaceAt 9 Nothing ["a"]]
      inv = LiveInventory (windowsOf [("a", 5)]) (Map.singleton (OutputKey 1) (OutputId 10))
      (st, notes) = restoreCore inv saved (freshCore [1, 2])
      hook = doShift 2 :: ManageHook Int DefaultLayout
      (placed, _) = drainCore hook [WindowCreated (WindowId 5) noWindowProps] st
  notes @?= [TagDropped 9, TagAdded 2]
  managed st @?= []
  stacks placed @?= [(1, []), (2, [WindowId 5])]

testLayoutFallback :: Assertion
testLayoutFallback = do
  let other = Choose CR (Tall 2 (1 / 10) (1 / 3)) (Choose CL (Mirror (Tall 1 (3 / 100) (1 / 2))) Full)
      saved = savedOf [(workspaceAt 1 (Just (OutputKey 1)) []){savedLayout = "Spiral 3"}, (workspaceAt 2 Nothing []){savedLayout = show other}]
      inv = LiveInventory Map.empty (Map.singleton (OutputKey 1) (OutputId 10))
      (st, notes) = restoreCore inv saved (freshCore [1, 2])
  notes @?= [LayoutFallback 1]
  [(tag w, layout w) | w <- allWorkspaces (coreWorkspaces st)] @?= [(1, defaultLayout), (2, other)]

testNoIdentifiers :: Assertion
testNoIdentifiers = do
  let saved = savedOf [workspaceAt 1 (Just (OutputKey 1)) ["a", "b"], workspaceAt 2 Nothing ["c"]]
      inv = LiveInventory Map.empty (Map.singleton (OutputKey 1) (OutputId 10))
      (st, notes) = restoreCore inv saved (freshCore [1, 2])
      hook = doShift 2 :: ManageHook Int DefaultLayout
      (placed, _) = drainCore hook [WindowCreated (WindowId w) noWindowProps | w <- [5, 6, 7]] st
  notes @?= [WindowsGone 3]
  managed st @?= []
  stacks placed @?= [(1, []), (2, map WindowId [7, 6, 5])]

testInvalidFloatBox :: Assertion
testInvalidFloatBox = do
  let saved = (savedOf [workspaceAt 1 (Just (OutputKey 1)) ["a", "b"]]){savedFloats = [(WindowKey (Text.pack "a"), SavedFloatAt 10 20 (-5) 30), (WindowKey (Text.pack "b"), SavedFloatAt 10 20 300 200)]}
      inv = LiveInventory (windowsOf [("a", 5), ("b", 6)]) (Map.singleton (OutputKey 1) (OutputId 10))
      (st, notes) = restoreCore inv saved (freshCore [1])
      box = either (error . show) (FloatAt . OutputRect) (mkSize 300 200 >>= mkRect (Point 10 20))
  notes @?= [FloatBoxInvalid (WindowKey (Text.pack "a"))]
  coreFloating (coreWorkspaces st) @?= Map.fromList [(WindowId 5, ClientSized), (WindowId 6, box)]
  coreRaiseOrder st @?= [WindowId 6, WindowId 5]
