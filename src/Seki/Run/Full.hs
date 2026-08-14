-- | Minimal Full layout plans for the M0 river window-manager slice.
module Seki.Run.Full (
  selectFullOutputRectangle,
  prepareFullManagePlan,
  prepareFullRenderPlan,
)
where

import Seki.Run.Inventory (
  RiverInventory,
  RiverRectangle (..),
  RiverWindowState (..),
  riverInventoryOutputs,
  riverInventoryWindows,
  riverOutputRectangle,
 )
import Seki.Run.Phase (
  ManageM,
  RenderM,
  hideWindow,
  proposeWindowDimensions,
  setNodePosition,
  showWindow,
 )

{- | Select the complete output with the lowest object ID.

Outputs without both position and dimensions are ignored. Returning 'Nothing'
leaves the manage plan empty and makes the render plan hide every window.
-}
selectFullOutputRectangle :: RiverInventory -> Maybe RiverRectangle
selectFullOutputRectangle inventory =
  case [ rectangle
       | output <- riverInventoryOutputs inventory
       , Just rectangle <- [riverOutputRectangle output]
       ] of
    [] -> Nothing
    rectangle : _ -> Just rectangle

-- | Propose the selected output's dimensions for every live window.
prepareFullManagePlan :: RiverInventory -> ManageM ()
prepareFullManagePlan inventory =
  case selectFullOutputRectangle inventory of
    Nothing -> pure ()
    Just rectangle ->
      mapM_
        ( \window ->
            proposeWindowDimensions
              (riverWindowObject window)
              (riverRectangleWidth rectangle)
              (riverRectangleHeight rectangle)
        )
        (riverInventoryWindows inventory)

{- | Place and show every initialized window, or hide it without an output.

A window without its node is kept hidden as a defensive fallback. The owned
runtime normally attaches the node before the window enters the inventory
snapshot observed at a sequence boundary.
-}
prepareFullRenderPlan :: RiverInventory -> RenderM ()
prepareFullRenderPlan inventory =
  case selectFullOutputRectangle inventory of
    Nothing -> mapM_ (hideWindow . riverWindowObject) windows
    Just rectangle -> mapM_ (placeWindow rectangle) windows
 where
  windows = riverInventoryWindows inventory
  placeWindow rectangle window =
    case riverWindowNode window of
      Nothing -> hideWindow (riverWindowObject window)
      Just node -> do
        setNodePosition
          node
          (riverRectangleX rectangle)
          (riverRectangleY rectangle)
        showWindow (riverWindowObject window)
