{-# LANGUAGE OverloadedStrings #-}

module Seki.Run.Snapshot (projectSnapshot) where

import Data.Aeson (Value (..), object, (.=))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Seki.Internal.Core.Layout (LayoutClass, description)
import Seki.Internal.Core.StackSet (OutputId (..), Screen (..), StackSet (..), WindowId (..), Workspace (..), integrate', peek)
import Seki.Internal.Core.State (CoreState (..), HeadlessWorkspaces (..), Workspaces (..))
import Seki.Run.Inventory (RiverOutputState (..), RiverWindowState (..), riverInventoryOutputs, riverInventoryWindows)
import Seki.Run.Runtime (RiverLayerFocus (..), RiverRuntimeSnapshot (..), RiverSeatBindings (..))
import Seki.Wire.Connection.ObjectTable (objectId)
import Seki.Wire.Types (objectIdValue)

-- | Call on the policy owner after reducing against this inventory.
projectSnapshot :: (Eq i, LayoutClass l) => Text -> (i -> Text) -> [i] -> Integer -> CoreState i l -> RiverRuntimeSnapshot -> Value
projectSnapshot session label tags revision st runtime =
  object
    [ "session" .= session
    , "revision" .= revision
    , "outputs" .= map outputValue (riverInventoryOutputs inventory)
    , "workspaces" .= map workspaceValue tags
    , "focused_title" .= fmap (Text.take 1024) title
    , "focused_title_truncated" .= maybe False ((> 1024) . Text.length) title
    , "keyboard_available" .= keyboardAvailable
    , "urgent" .= Null
    , "xkb" .= Null
    , "submap" .= Null
    ]
 where
  inventory = riverRuntimeInventory runtime
  (selected, shown, allWorkspaces, focused) = case coreWorkspaces st of
    HeadlessState h -> (tag (headlessCurrent h), [], headlessCurrent h : headlessHidden h, Nothing)
    AttachedState ss _ ->
      (tag (workspace (current ss)), current ss : visible ss, map workspace (current ss : visible ss) <> hidden ss, peek ss)
  title = do
    WindowId w <- focused
    window <- find ((== w) . objectIdValue . objectId . riverWindowObject) (riverInventoryWindows inventory)
    riverWindowTitle window
  keyboardAvailable =
    isJust focused && not (null seats) && all ((== RiverLayerFocusNone) . riverSeatLayerFocus) seats
  seats = riverRuntimeSeatBindings runtime
  workspaceValue t =
    object
      [ "tag" .= label t
      , "occupied" .= maybe False (not . null . integrate' . stack) ws
      , "visible" .= any ((== t) . tag . workspace) shown
      , "focused" .= (t == selected)
      , "layout" .= fmap (take 1024 . description . layout) ws
      , "layout_truncated" .= maybe False ((> 1024) . length . take 1025 . description . layout) ws
      ]
   where
    ws = find ((== t) . tag) allWorkspaces
  outputValue o =
    object
      [ "name" .= fmap (Text.take 1024) (riverOutputName o)
      , "name_truncated" .= maybe False ((> 1024) . Text.length) (riverOutputName o)
      , "wire_id" .= ident
      , "wl_output_name" .= riverOutputWlOutputName o
      , "workspace" .= fmap (label . tag . workspace) (find ((== OutputId ident) . output) shown)
      ]
   where
    ident = objectIdValue (objectId (riverOutputObject o))
