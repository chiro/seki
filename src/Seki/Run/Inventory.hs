-- | Pure live inventory of river windows and outputs.
module Seki.Run.Inventory (
  RiverInventory,
  RiverWindowState (..),
  RiverOutputState (..),
  RiverDimensionsHint (..),
  RiverPoint (..),
  RiverSize (..),
  RiverRectangle (..),
  RiverInventoryError (..),
  emptyRiverInventory,
  riverInventoryWindows,
  riverInventoryOutputs,
  riverInventoryActiveWindow,
  lookupRiverWindow,
  lookupRiverOutput,
  riverOutputRectangle,
  addRiverWindow,
  addRiverOutput,
  attachRiverWindowNode,
  activateRiverWindow,
  applyRiverWindowEvent,
  applyRiverOutputEvent,
)
where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Wire.Connection.ObjectTable (Object, objectId)
import Seki.Wire.Types (ObjectId)

data RiverPoint = RiverPoint
  { riverPointX :: Int32
  , riverPointY :: Int32
  }
  deriving (Eq, Show)

data RiverSize = RiverSize
  { riverSizeWidth :: Int32
  , riverSizeHeight :: Int32
  }
  deriving (Eq, Show)

data RiverRectangle = RiverRectangle
  { riverRectangleX :: Int32
  , riverRectangleY :: Int32
  , riverRectangleWidth :: Int32
  , riverRectangleHeight :: Int32
  }
  deriving (Eq, Show)

data RiverDimensionsHint = RiverDimensionsHint
  { riverMinimumWidth :: Int32
  , riverMinimumHeight :: Int32
  , riverMaximumWidth :: Int32
  , riverMaximumHeight :: Int32
  }
  deriving (Eq, Show)

data RiverWindowState = RiverWindowState
  { riverWindowObject :: Object River.RiverWindowV1
  , riverWindowNode :: Maybe (Object River.RiverNodeV1)
  , riverWindowDimensionsHint :: Maybe RiverDimensionsHint
  , riverWindowDimensions :: Maybe RiverSize
  , riverWindowAppId :: Maybe Text
  , riverWindowTitle :: Maybe Text
  , riverWindowParent :: Maybe ObjectId
  , riverWindowDecorationHint :: Maybe River.RiverWindowV1DecorationHintEnum
  , riverWindowUnreliablePid :: Maybe Int32
  , riverWindowPresentationHint :: Maybe River.RiverOutputV1PresentationModeEnum
  , riverWindowIdentifier :: Maybe Text
  }
  deriving (Eq, Show)

data RiverOutputState = RiverOutputState
  { riverOutputObject :: Object River.RiverOutputV1
  , riverOutputWlOutputName :: Maybe Word32
  , riverOutputPosition :: Maybe RiverPoint
  , riverOutputDimensions :: Maybe RiverSize
  }
  deriving (Eq, Show)

data RiverInventory = RiverInventory
  { inventoryWindows :: Map ObjectId RiverWindowState
  , inventoryOutputs :: Map ObjectId RiverOutputState
  , inventoryActiveWindow :: Maybe ObjectId
  }
  deriving (Eq, Show)

data RiverInventoryError
  = RiverInventoryObjectAlreadyTracked ObjectId
  | RiverInventoryWindowNotTracked ObjectId
  | RiverInventoryOutputNotTracked ObjectId
  | RiverInventoryWindowHandleMismatch ObjectId
  | RiverInventoryOutputHandleMismatch ObjectId
  | RiverInventoryInvalidDimensionsHint ObjectId RiverDimensionsHint
  | RiverInventoryInvalidWindowDimensions ObjectId RiverSize
  | RiverInventoryInvalidOutputDimensions ObjectId RiverSize
  | RiverInventoryRepeatedWindowPid ObjectId
  | RiverInventoryInvalidWindowIdentifier ObjectId Text
  | RiverInventoryRepeatedWindowIdentifier ObjectId
  | RiverInventoryRepeatedOutputName ObjectId
  | RiverInventoryWindowNodeAlreadyAttached ObjectId
  deriving (Eq, Show)

emptyRiverInventory :: RiverInventory
emptyRiverInventory = RiverInventory Map.empty Map.empty Nothing

riverInventoryWindows :: RiverInventory -> [RiverWindowState]
riverInventoryWindows = Map.elems . inventoryWindows

riverInventoryOutputs :: RiverInventory -> [RiverOutputState]
riverInventoryOutputs = Map.elems . inventoryOutputs

riverInventoryActiveWindow :: RiverInventory -> Maybe RiverWindowState
riverInventoryActiveWindow inventory =
  inventoryActiveWindow inventory >>= (`Map.lookup` inventoryWindows inventory)

lookupRiverWindow :: ObjectId -> RiverInventory -> Maybe RiverWindowState
lookupRiverWindow identifier = Map.lookup identifier . inventoryWindows

lookupRiverOutput :: ObjectId -> RiverInventory -> Maybe RiverOutputState
lookupRiverOutput identifier = Map.lookup identifier . inventoryOutputs

riverOutputRectangle :: RiverOutputState -> Maybe RiverRectangle
riverOutputRectangle output = do
  RiverPoint x y <- riverOutputPosition output
  RiverSize width height <- riverOutputDimensions output
  pure (RiverRectangle x y width height)

addRiverWindow ::
  Object River.RiverWindowV1 ->
  RiverInventory ->
  Either RiverInventoryError RiverInventory
addRiverWindow window inventory = do
  ensureObjectUntracked (objectId window) inventory
  let state =
        RiverWindowState
          { riverWindowObject = window
          , riverWindowNode = Nothing
          , riverWindowDimensionsHint = Nothing
          , riverWindowDimensions = Nothing
          , riverWindowAppId = Nothing
          , riverWindowTitle = Nothing
          , riverWindowParent = Nothing
          , riverWindowDecorationHint = Nothing
          , riverWindowUnreliablePid = Nothing
          , riverWindowPresentationHint = Nothing
          , riverWindowIdentifier = Nothing
          }
  pure
    inventory
      { inventoryWindows = Map.insert (objectId window) state (inventoryWindows inventory)
      , inventoryActiveWindow = Just (objectId window)
      }

activateRiverWindow ::
  ObjectId ->
  RiverInventory ->
  Either RiverInventoryError RiverInventory
activateRiverWindow identifier inventory =
  case Map.lookup identifier (inventoryWindows inventory) of
    Nothing -> Left (RiverInventoryWindowNotTracked identifier)
    Just _ -> Right inventory{inventoryActiveWindow = Just identifier}

addRiverOutput ::
  Object River.RiverOutputV1 ->
  RiverInventory ->
  Either RiverInventoryError RiverInventory
addRiverOutput output inventory = do
  ensureObjectUntracked (objectId output) inventory
  let state =
        RiverOutputState
          { riverOutputObject = output
          , riverOutputWlOutputName = Nothing
          , riverOutputPosition = Nothing
          , riverOutputDimensions = Nothing
          }
  pure
    inventory
      { inventoryOutputs = Map.insert (objectId output) state (inventoryOutputs inventory)
      }

attachRiverWindowNode ::
  Object River.RiverWindowV1 ->
  Object River.RiverNodeV1 ->
  RiverInventory ->
  Either RiverInventoryError RiverInventory
attachRiverWindowNode window node inventory = do
  current <- requireWindow window inventory
  case riverWindowNode current of
    Just _ -> Left (RiverInventoryWindowNodeAlreadyAttached identifier)
    Nothing ->
      Right
        inventory
          { inventoryWindows =
              Map.insert
                identifier
                current{riverWindowNode = Just node}
                (inventoryWindows inventory)
          }
 where
  identifier = objectId window

applyRiverWindowEvent ::
  Object River.RiverWindowV1 ->
  River.RiverWindowV1Event fd ->
  RiverInventory ->
  Either RiverInventoryError RiverInventory
applyRiverWindowEvent window event inventory = do
  current <- requireWindow window inventory
  case event of
    River.RiverWindowV1Closed ->
      let remaining = Map.delete identifier (inventoryWindows inventory)
          nextActive
            | inventoryActiveWindow inventory == Just identifier = fst <$> Map.lookupMax remaining
            | otherwise = inventoryActiveWindow inventory
       in pure
            inventory
              { inventoryWindows = remaining
              , inventoryActiveWindow = nextActive
              }
    River.RiverWindowV1DimensionsHint minimumWidth minimumHeight maximumWidth maximumHeight -> do
      let hint = RiverDimensionsHint minimumWidth minimumHeight maximumWidth maximumHeight
      if validDimensionsHint hint
        then update current{riverWindowDimensionsHint = Just hint}
        else Left (RiverInventoryInvalidDimensionsHint identifier hint)
    River.RiverWindowV1Dimensions width height -> do
      let dimensions = RiverSize width height
      if positiveSize dimensions
        then update current{riverWindowDimensions = Just dimensions}
        else Left (RiverInventoryInvalidWindowDimensions identifier dimensions)
    River.RiverWindowV1AppId appId -> update current{riverWindowAppId = appId}
    River.RiverWindowV1Title title -> update current{riverWindowTitle = title}
    River.RiverWindowV1Parent parent -> update current{riverWindowParent = parent}
    River.RiverWindowV1DecorationHint hint ->
      update current{riverWindowDecorationHint = Just hint}
    River.RiverWindowV1UnreliablePid processId ->
      case riverWindowUnreliablePid current of
        Nothing -> update current{riverWindowUnreliablePid = Just processId}
        Just _ -> Left (RiverInventoryRepeatedWindowPid identifier)
    River.RiverWindowV1PresentationHint hint ->
      update current{riverWindowPresentationHint = Just hint}
    River.RiverWindowV1Identifier value ->
      case riverWindowIdentifier current of
        Nothing
          | validWindowIdentifier value ->
              update current{riverWindowIdentifier = Just value}
          | otherwise -> Left (RiverInventoryInvalidWindowIdentifier identifier value)
        Just _ -> Left (RiverInventoryRepeatedWindowIdentifier identifier)
    _ -> Right inventory
 where
  identifier = objectId window
  update state =
    Right
      inventory
        { inventoryWindows = Map.insert identifier state (inventoryWindows inventory)
        }

applyRiverOutputEvent ::
  Object River.RiverOutputV1 ->
  River.RiverOutputV1Event fd ->
  RiverInventory ->
  Either RiverInventoryError RiverInventory
applyRiverOutputEvent output event inventory = do
  current <- requireOutput output inventory
  case event of
    River.RiverOutputV1Removed ->
      pure inventory{inventoryOutputs = Map.delete identifier (inventoryOutputs inventory)}
    River.RiverOutputV1WlOutput name ->
      case riverOutputWlOutputName current of
        Nothing -> update current{riverOutputWlOutputName = Just name}
        Just _ -> Left (RiverInventoryRepeatedOutputName identifier)
    River.RiverOutputV1Position x y ->
      update current{riverOutputPosition = Just (RiverPoint x y)}
    River.RiverOutputV1Dimensions width height -> do
      let dimensions = RiverSize width height
      if positiveSize dimensions
        then update current{riverOutputDimensions = Just dimensions}
        else Left (RiverInventoryInvalidOutputDimensions identifier dimensions)
 where
  identifier = objectId output
  update state =
    Right
      inventory
        { inventoryOutputs = Map.insert identifier state (inventoryOutputs inventory)
        }

ensureObjectUntracked :: ObjectId -> RiverInventory -> Either RiverInventoryError ()
ensureObjectUntracked identifier inventory
  | Map.member identifier (inventoryWindows inventory) =
      Left (RiverInventoryObjectAlreadyTracked identifier)
  | Map.member identifier (inventoryOutputs inventory) =
      Left (RiverInventoryObjectAlreadyTracked identifier)
  | otherwise = Right ()

requireWindow ::
  Object River.RiverWindowV1 ->
  RiverInventory ->
  Either RiverInventoryError RiverWindowState
requireWindow window inventory =
  case Map.lookup identifier (inventoryWindows inventory) of
    Nothing -> Left (RiverInventoryWindowNotTracked identifier)
    Just state
      | riverWindowObject state == window -> Right state
      | otherwise -> Left (RiverInventoryWindowHandleMismatch identifier)
 where
  identifier = objectId window

requireOutput ::
  Object River.RiverOutputV1 ->
  RiverInventory ->
  Either RiverInventoryError RiverOutputState
requireOutput output inventory =
  case Map.lookup identifier (inventoryOutputs inventory) of
    Nothing -> Left (RiverInventoryOutputNotTracked identifier)
    Just state
      | riverOutputObject state == output -> Right state
      | otherwise -> Left (RiverInventoryOutputHandleMismatch identifier)
 where
  identifier = objectId output

positiveSize :: RiverSize -> Bool
positiveSize (RiverSize width height) = width > 0 && height > 0

validDimensionsHint :: RiverDimensionsHint -> Bool
validDimensionsHint (RiverDimensionsHint minimumWidth minimumHeight maximumWidth maximumHeight) =
  minimumWidth >= 0
    && minimumHeight >= 0
    && maximumWidth >= 0
    && maximumHeight >= 0
    && (maximumWidth == 0 || minimumWidth <= maximumWidth)
    && (maximumHeight == 0 || minimumHeight <= maximumHeight)

validWindowIdentifier :: Text -> Bool
validWindowIdentifier value =
  not (Text.null value)
    && Text.length value <= 32
    && Text.all (\character -> character >= ' ' && character <= '~') value
