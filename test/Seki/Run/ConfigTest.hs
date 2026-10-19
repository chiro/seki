{-# LANGUAGE LambdaCase #-}

module Seki.Run.ConfigTest (tests) where

import Control.Exception (bracket)
import Data.Bits ((.|.))
import Data.Map.Strict qualified as Map
import Data.Monoid (Endo (..))
import Data.Text qualified as Text
import Seki.Internal.Config
import Seki.Internal.Core.Action qualified as Action
import Seki.Internal.Core.Layout (CLR (..), Choose (..), Mirror (..), description)
import Seki.Internal.Core.ManageHook (WindowProps (..), appId, composeAll, doShift, noWindowProps, runQuery, (-->), (=?))
import Seki.Internal.Core.StackSet qualified as StackSet
import Seki.Internal.Core.State (CoreState (..), Workspaces (..), fromWorkspaces)
import Seki.Internal.Layout.Full (Full (..))
import Seki.Internal.Layout.Tall (Tall (..))
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Run.Config (ConfigRunError (..), policyConfigOf, prepareConfig, runConfig)
import Seki.Run.Policy (PolicyConfig (..), policyRuntimeHandlers)
import Seki.Run.Runtime (RiverRuntimeHandlers (..))
import Seki.Run.Xkb (RiverKeyBinding (..))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "configuration"
    [ testCase "default configuration values" $ do
        assertEqual "modifier" Mod4 (modMask def)
        assertEqual "terminal" "foot" (terminal def)
        assertEqual "tags" (map show [1 .. 9 :: Int]) (workspaces def)
        assertEqual "layout" (Choose CL tall (Choose CL (Mirror tall) Full)) (layoutHook def)
        assertEqual "border width" 2 (borderWidth (borders def))
        assertEqual "fixed group" True (bindingsUseFixedLayout def)
        assertEqual "binding count" 29 (length (keys def def))
        policy <- prepareConfig def >>= either (assertFailure . show) pure
        assertEqual "lowered fixed group" (replicate 29 (Just 0)) (map riverKeyBindingLayoutOverride (riverKeyBindings (policyRuntimeHandlers policy)))
    , testCase "final overrides reach keys and wire binding identity" $ do
        let config = def{modMask = Mod1, terminal = "custom-terminal", workspaces = ["a", "b"], bindingsUseFixedLayout = False}
        assertEqual "terminal action" [Action.Spawn "custom-terminal"] (intents (press config Shift 0xff0d initial))
        policy <- prepareConfig config >>= either (assertFailure . show) pure
        let bindings = riverKeyBindings (policyRuntimeHandlers policy)
        assertEqual "all modifiers" [River.RiverSeatV1ModifiersFlags 9, River.RiverSeatV1ModifiersFlags 9] (map riverKeyBindingModifiers (take 2 bindings))
        assertEqual "no fixed layout" (replicate 15 Nothing) (map riverKeyBindingLayoutOverride bindings)
        assertEqual "overridden tags" "b" (currentTag (press config (Mods 0) 0x32 (stateWithTags ["a", "b"])))
    , testCase "every default action changes the intended core state" $ do
        assertEqual "spawn" [Action.Spawn "foot"] (intents (press def Shift 0xff0d initial))
        assertEqual "kill" [Action.CloseWindow (StackSet.WindowId 2)] (intents (press def Shift 0x63 initial))
        assertEqual "focus down" (Just (StackSet.WindowId 3)) (focused (press def (Mods 0) 0x6a initial))
        assertEqual "focus up" (Just (StackSet.WindowId 1)) (focused (press def (Mods 0) 0x6b initial))
        assertEqual "swap master" [2, 1, 3] (windowNumbers (press def (Mods 0) 0xff0d initial))
        assertEqual "shrink" (47 / 100) (ratio (press def (Mods 0) 0x68 initial))
        assertEqual "expand" (53 / 100) (ratio (press def (Mods 0) 0x6c initial))
        assertEqual "more masters" 2 (masters (press def (Mods 0) 0x2c initial))
        assertEqual "fewer masters" 0 (masters (press def (Mods 0) 0x2e initial))
        let mirrored = state (press def (Mods 0) 0x20 initial)
        assertEqual "mirror selected" "Mirror Tall" (description (currentLayout mirrored))
        assertEqual "mirror resize" (Choose CR tall (Choose CL (Mirror (Tall 1 (3 / 100) (47 / 100))) Full)) (currentLayout (state (press def (Mods 0) 0x68 mirrored)))
        let full = state (press def (Mods 0) 0x20 mirrored)
        assertEqual "Full selected" "Full" (description (currentLayout full))
        assertEqual "cycle wraps" "Tall" (description (currentLayout (state (press def (Mods 0) 0x20 full))))
        mapM_ checkTag (zip (workspaces def) [0x31 .. 0x39])
    , testCase "the default manage hook floats a dialog and leaves other windows tiled where they were inserted" $ do
        assertEqual "inserted window" (Just (StackSet.WindowId 2)) (peekState (manage def mpv initial))
        assertEqual "other window tiled" Map.empty (floatingState (manage def mpv initial))
        assertEqual "dialog floats with no stored box" (Map.singleton (StackSet.WindowId 2) StackSet.ClientSized) (floatingState (manage def noWindowProps{propParent = Just (StackSet.WindowId 1)} initial))
    , testCase "defaultManageHook composes with a hook of another layout type" $ do
        let config = def{layoutHook = Full, keys = defaultKeys, manageHook = composeAll [appId =? "mpv" --> doShift "2", defaultManageHook], startupHook = pure ()}
            fullInitial = fromWorkspaces (AttachedState (StackSet.StackSet (StackSet.Screen (StackSet.Workspace "1" Full (Just (StackSet.Stack (StackSet.WindowId 2) [] []))) (StackSet.OutputId 1)) [] [StackSet.Workspace "2" Full Nothing] Map.empty) [])
        assertEqual "dialog floats" (Map.singleton (StackSet.WindowId 2) StackSet.ClientSized) (floatingState (manage config noWindowProps{propParent = Just (StackSet.WindowId 1)} fullInitial))
    , testCase "mod-t tiles the focused floating window again" $ do
        let floated = state (Action.runAction (Action.float (StackSet.WindowId 2)) initial)
        assertEqual "floated first" (Map.singleton (StackSet.WindowId 2) StackSet.ClientSized) (floatingState floated)
        assertEqual "sunk" Map.empty (floatingState (state (press def (Mods 0) 0x74 floated)))
    , testCase "the configured manage hook reaches the policy" $ do
        let config = def{manageHook = appId =? "mpv" --> doShift "2"}
        assertEqual "matching window shifted" (Just "2") (findTagState (StackSet.WindowId 2) (manage config mpv initial))
        assertEqual "other window kept" (Just "1") (findTagState (StackSet.WindowId 2) (manage config noWindowProps initial))
    , testCase "custom pure actions compose" $ do
        let (_, changed, effects) = Action.runAction (Action.windows StackSet.focusDown >> Action.kill >> Action.spawn "one" >> Action.spawn "two") initial
        assertEqual "focus" (Just (StackSet.WindowId 3)) (peekState changed)
        assertEqual "ordered effects" [Action.ReclaimFocus, Action.CloseWindow (StackSet.WindowId 3), Action.Spawn "one", Action.Spawn "two"] effects
    , testCase "the startup hook reaches the policy" $ do
        assertEqual "the default hook does nothing" [] (intents (Action.runAction (policyStartupHook (policyConfigOf def)) initial))
        assertEqual "a configured hook" [Action.Spawn "bar"] (intents (Action.runAction (policyStartupHook (policyConfigOf def{startupHook = Action.spawnOnce "bar"})) initial))
    , testCase "additionalKeys replaces a binding with the same identity" $ do
        let config = additionalKeys def [(KeyBinding (Mod4 .|. Shift) (Keysym 0xff0d), Action.spawn "alacritty")]
        assertEqual "the new action wins" [Action.Spawn "alacritty"] (intents (press config Shift 0xff0d initial))
        assertEqual "the binding count is unchanged" 29 (length (keys config config))
        prepareConfig config >>= either (assertFailure . ("validation rejected the replacement: " <>) . show) (const (pure ()))
    , testCase "additionalKeys keeps the last of repeated new identities and adds new ones" $ do
        let volume = KeyBinding (Mods 0) (Keysym 0x1008ff13)
            config = additionalKeys def [(volume, Action.spawn "first"), (volume, Action.spawn "louder")]
        assertEqual "the last repeat wins" [Action.Spawn "louder"] (intents (pressExactly config volume initial))
        assertEqual "one binding added" 30 (length (keys config config))
        prepareConfig config >>= either (assertFailure . ("validation rejected the addition: " <>) . show) (const (pure ()))
    , testCase "additionalKeys keeps later record overrides" $ do
        let config = (additionalKeys def [(KeyBinding Mod4 (Keysym 0x70), Action.spawn "menu")]){modMask = Mod1, terminal = "custom-terminal"}
        assertEqual "default keys see the final modifier and terminal" [Action.Spawn "custom-terminal"] (intents (press config Shift 0xff0d initial))
        assertEqual "the added binding keeps its literal modifier" [Action.Spawn "menu"] (intents (pressExactly config (KeyBinding Mod4 (Keysym 0x70)) initial))
    , testCase "removeKeys removes a binding" $ do
        let config = removeKeys def [KeyBinding (Mod4 .|. Shift) (Keysym 0x63)]
        assertBool "kill is gone" (KeyBinding (Mod4 .|. Shift) (Keysym 0x63) `notElem` map fst (keys config config))
        assertEqual "the rest remain" 28 (length (keys config config))
        assertEqual "removed and re-added" [Action.Spawn "x"] (intents (pressExactly (additionalKeys config [(KeyBinding (Mod4 .|. Shift) (Keysym 0x63), Action.spawn "x")]) (KeyBinding (Mod4 .|. Shift) (Keysym 0x63)) initial))
    , testCase "a binding with no modifier passes validation and lowers with no modifier" $
        bracket (lookupEnv "WAYLAND_DISPLAY") restore $ \_ -> do
          let config = additionalKeys def [(KeyBinding (Mods 0) (Keysym 0x1008ff13), Action.spawn "louder")]
          policy <- prepareConfig config >>= either (assertFailure . show) pure
          assertBool
            "lowered with modifiers 0"
            (RiverKeyBinding 0x1008ff13 (River.RiverSeatV1ModifiersFlags 0) (Just 0) `elem` riverKeyBindings (policyRuntimeHandlers policy))
          setEnv "WAYLAND_DISPLAY" "/this-wayland-display-does-not-exist"
          runConfig config >>= \case
            Left ConfigRuntimeError{} -> pure ()
            other -> assertFailure ("expected validation to pass and the connection to fail, got " ++ show other)
    , testCase "invalid configuration fails before connecting" $
        bracket (lookupEnv "WAYLAND_DISPLAY") restore $ \_ -> do
          setEnv "WAYLAND_DISPLAY" "/this-wayland-display-does-not-exist"
          mapM_
            invalid
            [ def{modMask = Mods 16}
            , def{modMask = Mods 0x100}
            , def{workspaces = []}
            , def{workspaces = ["same", "same"]}
            , def{borders = (borders def){borderWidth = -1}}
            , def{borders = (borders def){focusedBorderColor = BorderColor 2 0 0 1}}
            , def{keys = \_ -> [(KeyBinding Mod4 (Keysym 1), pure ()), (KeyBinding Mod4 (Keysym 1), pure ())]}
            ]
    ]
 where
  restore = maybe (unsetEnv "WAYLAND_DISPLAY") (setEnv "WAYLAND_DISPLAY")
  invalid config =
    runConfig config >>= \result -> case result of
      Left ConfigPolicyError{} -> pure ()
      other -> assertFailure ("expected configuration failure before connection, got " ++ show other)
  checkTag (tag, symbol) = do
    assertEqual ("view " ++ tag) tag (currentTag (press def (Mods 0) symbol initial))
    let moved = state (press def Shift symbol initial)
    assertEqual ("shift " ++ tag) (Just tag) (findTagState (StackSet.WindowId 2) moved)

tall :: Tall
tall = Tall 1 (3 / 100) (1 / 2)

initial :: CoreState String StdLayouts
initial = stateWithTags (workspaces def)

stateWithTags :: [String] -> CoreState String StdLayouts
stateWithTags tags = fromWorkspaces (AttachedState (StackSet.StackSet (StackSet.Screen first (StackSet.OutputId 1)) [] rest Map.empty) [])
 where
  first = StackSet.Workspace (head tags) (layoutHook def) (Just (StackSet.Stack (StackSet.WindowId 2) [StackSet.WindowId 1] [StackSet.WindowId 3]))
  rest = [StackSet.Workspace tag (layoutHook def) Nothing | tag <- tail tags]

press :: Config l -> Mods -> Word -> CoreState String l -> ((), CoreState String l, [Action.Intent])
press config modifiers symbol = pressExactly config (KeyBinding (modMask config .|. modifiers) (Keysym (fromIntegral symbol)))

pressExactly :: Config l -> KeyBinding -> CoreState String l -> ((), CoreState String l, [Action.Intent])
pressExactly config binding core = case lookup binding (keys config config) of
  Nothing -> error ("missing binding " <> show binding)
  Just action -> Action.runAction action core

-- | Run the policy's manage hook for window 2, already inserted in the state.
manage :: Config l -> WindowProps -> CoreState String l -> CoreState String l
manage config props st = case coreWorkspaces st of
  AttachedState ss u -> fromWorkspaces (AttachedState (appEndo (runQuery (policyManageHook (policyConfigOf config)) (StackSet.WindowId 2) props) ss) u)
  _ -> st

mpv :: WindowProps
mpv = noWindowProps{propAppId = Just (Text.pack "mpv")}

state :: (a, b, c) -> b
state (_, value, _) = value

intents :: (a, b, c) -> c
intents (_, _, value) = value

peekState :: CoreState i l -> Maybe StackSet.WindowId
peekState st = case coreWorkspaces st of
  AttachedState stackSet _ -> StackSet.peek stackSet
  HeadlessState{} -> Nothing

focused :: (a, CoreState i l, c) -> Maybe StackSet.WindowId
focused = peekState . state

currentLayout :: CoreState i l -> l
currentLayout st = case coreWorkspaces st of
  AttachedState stackSet _ -> StackSet.layout (StackSet.workspace (StackSet.current stackSet))
  HeadlessState{} -> error "expected attached state"

currentTag :: (a, CoreState String l, c) -> String
currentTag (_, CoreState{coreWorkspaces = AttachedState stackSet _}, _) = StackSet.currentTag stackSet
currentTag _ = error "expected attached state"

ratio :: (a, CoreState i StdLayouts, c) -> Rational
ratio result = case currentLayout (state result) of Choose _ layout _ -> tallRatio layout

masters :: (a, CoreState i StdLayouts, c) -> Int
masters result = case currentLayout (state result) of Choose _ layout _ -> tallNMaster layout

windowNumbers :: (a, CoreState i l, c) -> [Word]
windowNumbers (_, CoreState{coreWorkspaces = AttachedState stackSet _}, _) = [fromIntegral n | StackSet.WindowId n <- StackSet.index stackSet]
windowNumbers _ = []

findTagState :: StackSet.WindowId -> CoreState i l -> Maybe i
findTagState window st = case coreWorkspaces st of
  AttachedState stackSet _ -> StackSet.findTag window stackSet
  HeadlessState{} -> Nothing

floatingState :: CoreState i l -> Map.Map StackSet.WindowId StackSet.FloatGeom
floatingState st = case coreWorkspaces st of
  AttachedState stackSet _ -> StackSet.floating stackSet
  HeadlessState{} -> Map.empty
