-- | Typed Wayland object IDs, allocation, and lifecycle bookkeeping.
module Seki.Wire.Connection.ObjectTable (
  ObjectLimit,
  makeObjectLimit,
  defaultObjectLimit,
  clientObjectIdMaximum,
  serverObjectIdMinimum,
  Object,
  objectId,
  objectVersion,
  ObjectOrigin (..),
  ObjectLifecycle (..),
  ObjectInfo (..),
  ObjectTable,
  ObjectTableError (..),
  newObjectTable,
  allocateClientObject,
  registerServerObject,
  lookupObjectInfo,
  requireActiveObject,
  validateObject,
  destroyObjectByRequest,
  destroyObjectByEvent,
  acknowledgeDeleteId,
  objectCount,
  retiredObjectCount,
  closeObjectTable,
)
where

import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Seki.Wire.Binding (Interface (..))
import Seki.Wire.Types (ObjectId, mkObjectId, objectIdValue)

-- | Maximum number of live objects and unreleased client-ID tombstones.
newtype ObjectLimit = ObjectLimit Int
  deriving (Eq, Show)

makeObjectLimit :: Int -> Either ObjectTableError ObjectLimit
makeObjectLimit limit
  | limit <= 0 = Left (InvalidObjectLimit limit)
  | otherwise = Right (ObjectLimit limit)

-- | Conservative object-table limit for one connection.
defaultObjectLimit :: ObjectLimit
defaultObjectLimit = ObjectLimit 65536

-- | Last ID in the client-allocated namespace.
clientObjectIdMaximum :: Word32
clientObjectIdMaximum = 0xfeffffff

-- | First ID in the server-allocated namespace.
serverObjectIdMinimum :: Word32
serverObjectIdMinimum = 0xff000000

-- | A typed reference to a protocol object.
data Object interface = Object
  { objectId :: ObjectId
  , objectVersion :: Word32
  , objectInterfaceName :: Text
  , objectGeneration :: Word64
  }
  deriving (Eq, Show)

data ObjectOrigin
  = DisplayObject
  | ClientCreatedObject
  | ServerCreatedObject
  deriving (Eq, Show)

data ObjectLifecycle
  = ObjectActive
  | ObjectAwaitingDeleteId
  deriving (Eq, Show)

data ObjectInfo = ObjectInfo
  { objectInfoId :: ObjectId
  , objectInfoInterfaceName :: Text
  , objectInfoInterfaceMaximumVersion :: Word32
  , objectInfoVersion :: Word32
  , objectInfoOrigin :: ObjectOrigin
  , objectInfoLifecycle :: ObjectLifecycle
  }
  deriving (Eq, Show)

data ObjectEntry = ObjectEntry
  { entryInfo :: ObjectInfo
  , entryGeneration :: Word64
  }
  deriving (Eq, Show)

data ObjectTable = ObjectTable
  { tableLimit :: ObjectLimit
  , tableEntries :: Map Word32 ObjectEntry
  , tableReusableClientIds :: Set Word32
  , tableNextClientId :: Word32
  , tableNextGeneration :: Word64
  , tableClosed :: Bool
  }
  deriving (Eq, Show)

data ObjectTableError
  = InvalidObjectLimit Int
  | InvalidObjectVersion Text Word32 Word32
  | ObjectLimitExceeded Int
  | ClientObjectIdSpaceExhausted
  | ServerObjectIdOutOfRange Word32
  | ObjectIdAlreadyRegistered ObjectId
  | UnknownObject ObjectId
  | ObjectNotActive ObjectId
  | ObjectInterfaceMismatch ObjectId Text Text
  | StaleObjectHandle ObjectId
  | DisplayObjectCannotBeDestroyed
  | InvalidDeleteId Word32
  | DeleteIdForDisplay
  | DeleteIdForServerObject ObjectId
  | ObjectTableClosed
  deriving (Eq, Show)

-- | Install the implicit object ID 1 using its generated display descriptor.
newObjectTable :: ObjectLimit -> Interface display -> Either ObjectTableError (ObjectTable, Object display)
newObjectTable limit displayInterface = do
  validateVersion displayInterface (interfaceMaximumVersion displayInterface)
  displayId <- maybe (Left (InvalidDeleteId 1)) Right (mkObjectId 1)
  let generation = 1
      displayObject = makeObject displayId displayInterface (interfaceMaximumVersion displayInterface) generation
      displayInfo = makeObjectInfo displayId displayInterface (interfaceMaximumVersion displayInterface) DisplayObject ObjectActive
      table =
        ObjectTable
          { tableLimit = limit
          , tableEntries = Map.singleton 1 (ObjectEntry displayInfo generation)
          , tableReusableClientIds = Set.empty
          , tableNextClientId = 2
          , tableNextGeneration = 2
          , tableClosed = False
          }
  pure (table, displayObject)

-- | Allocate the lowest reusable client ID or the next dense client ID.
allocateClientObject ::
  Interface interface ->
  Word32 ->
  ObjectTable ->
  Either ObjectTableError (ObjectTable, Object interface)
allocateClientObject interface version table = do
  requireOpen table
  ensureCapacity table
  validateVersion interface version
  (allocatedId, nextTable) <- allocateClientId table
  let generation = tableNextGeneration nextTable
      object = makeObject allocatedId interface version generation
      info = makeObjectInfo allocatedId interface version ClientCreatedObject ObjectActive
      entry = ObjectEntry info generation
      updated =
        nextTable
          { tableEntries = Map.insert (objectIdValue allocatedId) entry (tableEntries nextTable)
          , tableNextGeneration = generation + 1
          }
  pure (updated, object)

-- | Register an object whose ID was allocated by the compositor.
registerServerObject ::
  ObjectId ->
  Interface interface ->
  Word32 ->
  ObjectTable ->
  Either ObjectTableError (ObjectTable, Object interface)
registerServerObject allocatedId interface version table = do
  requireOpen table
  ensureCapacity table
  let rawId = objectIdValue allocatedId
  when (rawId < serverObjectIdMinimum) $
    Left (ServerObjectIdOutOfRange rawId)
  when (Map.member rawId (tableEntries table)) $
    Left (ObjectIdAlreadyRegistered allocatedId)
  validateVersion interface version
  let generation = tableNextGeneration table
      object = makeObject allocatedId interface version generation
      info = makeObjectInfo allocatedId interface version ServerCreatedObject ObjectActive
      entry = ObjectEntry info generation
      updated =
        table
          { tableEntries = Map.insert rawId entry (tableEntries table)
          , tableNextGeneration = generation + 1
          }
  pure (updated, object)

lookupObjectInfo :: ObjectId -> ObjectTable -> Maybe ObjectInfo
lookupObjectInfo identifier table = entryInfo <$> Map.lookup (objectIdValue identifier) (tableEntries table)

requireActiveObject :: ObjectId -> ObjectTable -> Either ObjectTableError ObjectInfo
requireActiveObject identifier table = do
  requireOpen table
  entry <- maybe (Left (UnknownObject identifier)) Right (Map.lookup (objectIdValue identifier) (tableEntries table))
  case objectInfoLifecycle (entryInfo entry) of
    ObjectActive -> Right (entryInfo entry)
    ObjectAwaitingDeleteId -> Left (ObjectNotActive identifier)

-- | Validate that a typed handle still names the same active table generation.
validateObject :: Interface interface -> Object interface -> ObjectTable -> Either ObjectTableError ObjectInfo
validateObject interface object table = do
  info <- requireActiveObject (objectId object) table
  let expectedInterface = interfaceName interface
      handleInterface = objectInterfaceName object
  when (handleInterface /= expectedInterface) $
    Left (ObjectInterfaceMismatch (objectId object) expectedInterface handleInterface)
  when (objectInfoInterfaceName info /= expectedInterface) $
    Left (ObjectInterfaceMismatch (objectId object) expectedInterface (objectInfoInterfaceName info))
  entry <- maybe (Left (UnknownObject (objectId object))) Right (Map.lookup (objectIdValue (objectId object)) (tableEntries table))
  if entryGeneration entry /= objectGeneration object || objectInfoVersion info /= objectVersion object
    then Left (StaleObjectHandle (objectId object))
    else Right info

-- | Apply a generated destructor request to an active object.
destroyObjectByRequest :: Object interface -> ObjectTable -> Either ObjectTableError ObjectTable
destroyObjectByRequest = destroyObject

-- | Apply a generated destructor event after it has decoded successfully.
destroyObjectByEvent :: Object interface -> ObjectTable -> Either ObjectTableError ObjectTable
destroyObjectByEvent = destroyObject

-- | Release a client ID after @wl_display.delete_id@.
acknowledgeDeleteId :: Word32 -> ObjectTable -> Either ObjectTableError ObjectTable
acknowledgeDeleteId rawId table = do
  requireOpen table
  identifier <- maybe (Left (InvalidDeleteId rawId)) Right (mkObjectId rawId)
  entry <- maybe (Left (UnknownObject identifier)) Right (Map.lookup rawId (tableEntries table))
  case objectInfoOrigin (entryInfo entry) of
    DisplayObject -> Left DeleteIdForDisplay
    ServerCreatedObject -> Left (DeleteIdForServerObject identifier)
    ClientCreatedObject ->
      Right
        table
          { tableEntries = Map.delete rawId (tableEntries table)
          , tableReusableClientIds = Set.insert rawId (tableReusableClientIds table)
          }

objectCount :: ObjectTable -> Int
objectCount = Map.size . tableEntries

retiredObjectCount :: ObjectTable -> Int
retiredObjectCount = length . filter ((== ObjectAwaitingDeleteId) . objectInfoLifecycle . entryInfo) . Map.elems . tableEntries

-- | Drop all objects and make subsequent operations fail deterministically.
closeObjectTable :: ObjectTable -> ObjectTable
closeObjectTable table =
  table
    { tableEntries = Map.empty
    , tableReusableClientIds = Set.empty
    , tableClosed = True
    }

destroyObject :: Object interface -> ObjectTable -> Either ObjectTableError ObjectTable
destroyObject object table = do
  requireOpen table
  entry <- validateHandle object table
  case objectInfoOrigin (entryInfo entry) of
    DisplayObject -> Left DisplayObjectCannotBeDestroyed
    ClientCreatedObject ->
      let retiredInfo = (entryInfo entry){objectInfoLifecycle = ObjectAwaitingDeleteId}
       in Right table{tableEntries = Map.insert rawId entry{entryInfo = retiredInfo} (tableEntries table)}
    ServerCreatedObject -> Right table{tableEntries = Map.delete rawId (tableEntries table)}
 where
  rawId = objectIdValue (objectId object)

validateHandle :: Object interface -> ObjectTable -> Either ObjectTableError ObjectEntry
validateHandle object table = do
  entry <- maybe (Left (UnknownObject (objectId object))) Right (Map.lookup rawId (tableEntries table))
  case objectInfoLifecycle (entryInfo entry) of
    ObjectAwaitingDeleteId -> Left (ObjectNotActive (objectId object))
    ObjectActive
      | entryGeneration entry /= objectGeneration object -> Left (StaleObjectHandle (objectId object))
      | objectInfoInterfaceName (entryInfo entry) /= objectInterfaceName object ->
          Left (ObjectInterfaceMismatch (objectId object) (objectInfoInterfaceName (entryInfo entry)) (objectInterfaceName object))
      | objectInfoVersion (entryInfo entry) /= objectVersion object -> Left (StaleObjectHandle (objectId object))
      | otherwise -> Right entry
 where
  rawId = objectIdValue (objectId object)

allocateClientId :: ObjectTable -> Either ObjectTableError (ObjectId, ObjectTable)
allocateClientId table = case Set.minView (tableReusableClientIds table) of
  Just (rawId, remaining) -> do
    identifier <- maybe (Left ClientObjectIdSpaceExhausted) Right (mkObjectId rawId)
    pure (identifier, table{tableReusableClientIds = remaining})
  Nothing
    | tableNextClientId table > clientObjectIdMaximum -> Left ClientObjectIdSpaceExhausted
    | otherwise -> do
        let rawId = tableNextClientId table
        identifier <- maybe (Left ClientObjectIdSpaceExhausted) Right (mkObjectId rawId)
        pure (identifier, table{tableNextClientId = rawId + 1})

ensureCapacity :: ObjectTable -> Either ObjectTableError ()
ensureCapacity table =
  let ObjectLimit limit = tableLimit table
      count = objectCount table
   in if count >= limit then Left (ObjectLimitExceeded limit) else Right ()

validateVersion :: Interface interface -> Word32 -> Either ObjectTableError ()
validateVersion interface version
  | version == 0 || version > maximumVersion =
      Left (InvalidObjectVersion (interfaceName interface) version maximumVersion)
  | otherwise = Right ()
 where
  maximumVersion = interfaceMaximumVersion interface

requireOpen :: ObjectTable -> Either ObjectTableError ()
requireOpen table
  | tableClosed table = Left ObjectTableClosed
  | otherwise = Right ()

makeObject :: ObjectId -> Interface interface -> Word32 -> Word64 -> Object interface
makeObject identifier interface version generation =
  Object
    { objectId = identifier
    , objectVersion = version
    , objectInterfaceName = interfaceName interface
    , objectGeneration = generation
    }

makeObjectInfo :: ObjectId -> Interface interface -> Word32 -> ObjectOrigin -> ObjectLifecycle -> ObjectInfo
makeObjectInfo identifier interface version origin lifecycle =
  ObjectInfo
    { objectInfoId = identifier
    , objectInfoInterfaceName = interfaceName interface
    , objectInfoInterfaceMaximumVersion = interfaceMaximumVersion interface
    , objectInfoVersion = version
    , objectInfoOrigin = origin
    , objectInfoLifecycle = lifecycle
    }
