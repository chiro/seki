module Seki.Run.ConfigTest (tests) where

import Control.Exception (bracket)
import Data.Bits ((.|.))
import Data.Map.Strict qualified as Map
import Data.Monoid (Endo (..))
import Data.Text qualified as Text
import Seki.Internal.Config
import Seki.Internal.Core.Action qualified as Action
import Seki.Internal.Core.Layout (CLR (..), Choose (..), Mirror (..), description)
import Seki.Internal.Core.ManageHook (WindowProps (..), appId, doShift, noWindowProps, runQuery, (-->), (=?))
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
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

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
        assertEqual "binding count" 28 (length (keys def def))
        policy <- prepareConfig def >>= either (assertFailure . show) pure
        assertEqual "lowered fixed group" (replicate 28 (Just 0)) (map riverKeyBindingLayoutOverride (riverKeyBindings (policyRuntimeHandlers policy)))
    , testCase "final overrides reach keys and wire binding identity" $ do
        let config = def{modMask = Mod1, terminal = "custom-terminal", workspaces = ["a", "b"], bindingsUseFixedLayout = False}
        assertEqual "terminal action" [Action.Spawn "custom-terminal"] (intents (press config Shift 0xff0d initial))
        policy <- prepareConfig config >>= either (assertFailure . show) pure
        let bindings = riverKeyBindings (policyRuntimeHandlers policy)
        assertEqual "all modifiers" [River.RiverSeatV1ModifiersFlags 9, River.RiverSeatV1ModifiersFlags 9] (map riverKeyBindingModifiers (take 2 bindings))
        assertEqual "no fixed layout" (replicate 14 Nothing) (map riverKeyBindingLayoutOverride bindings)
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
    , testCase "the default manage hook leaves a new window where it was inserted" $
        assertEqual "inserted window" (Just (StackSet.WindowId 2)) (peekState (manage def mpv initial))
    , testCase "the configured manage hook reaches the policy" $ do
        let config = def{manageHook = appId =? "mpv" --> doShift "2"}
        assertEqual "matching window shifted" (Just "2") (findTagState (StackSet.WindowId 2) (manage config mpv initial))
        assertEqual "other window kept" (Just "1") (findTagState (StackSet.WindowId 2) (manage config noWindowProps initial))
    , testCase "custom pure actions compose" $ do
        let (_, changed, effects) = Action.runAction (Action.windows StackSet.focusDown >> Action.kill >> Action.spawn "one" >> Action.spawn "two") initial
        assertEqual "focus" (Just (StackSet.WindowId 3)) (peekState changed)
        assertEqual "ordered effects" [Action.ReclaimFocus, Action.CloseWindow (StackSet.WindowId 3), Action.Spawn "one", Action.Spawn "two"] effects
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
press config modifiers symbol core = case lookup (KeyBinding (modMask config .|. modifiers) (Keysym (fromIntegral symbol))) (keys config config) of
  Nothing -> error "missing binding"
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
