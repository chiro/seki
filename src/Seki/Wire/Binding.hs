-- | Runtime-neutral metadata and codecs shared by generated protocol bindings.
module Seki.Wire.Binding (
  Interface (..),
  NewObject,
  newObjectId,
  getNewObject,
  DynamicNewId (..),
  putDynamicNewId,
  getDynamicNewId,
  RequestEncoder (..),
  EventDecoder (..),
)
where

import Data.Text (Text)
import Data.Word (Word32)
import Seki.Wire.Arguments (
  ArgumentDecoder,
  ArgumentEncoder,
  getNewId,
  getString,
  getUint,
  putNewId,
  putString,
  putUint,
 )
import Seki.Wire.Types (ObjectId, Opcode)

-- | The wire name and generated maximum version of an interface.
data Interface interface = Interface
  { interfaceName :: Text
  , interfaceMaximumVersion :: Word32
  }
  deriving (Eq, Show)

-- | A typed object ID introduced by a server-side @new_id@ event.
newtype NewObject interface = NewObject
  { newObjectId :: ObjectId
  }
  deriving (Eq, Show)

-- | Decode a statically typed object ID introduced by an event.
getNewObject :: ArgumentDecoder fd (NewObject interface)
getNewObject = NewObject <$> getNewId

-- | The expanded wire form of a @new_id@ with no static interface.
data DynamicNewId = DynamicNewId
  { dynamicNewIdInterface :: Text
  , dynamicNewIdVersion :: Word32
  , dynamicNewIdObject :: ObjectId
  }
  deriving (Eq, Show)

-- | Encode the interface name, version, and object ID of a dynamic @new_id@.
putDynamicNewId :: DynamicNewId -> ArgumentEncoder fd ()
putDynamicNewId value = do
  putString (dynamicNewIdInterface value)
  putUint (dynamicNewIdVersion value)
  putNewId (dynamicNewIdObject value)

-- | Decode the interface name, version, and object ID of a dynamic @new_id@.
getDynamicNewId :: ArgumentDecoder fd DynamicNewId
getDynamicNewId = DynamicNewId <$> getString <*> getUint <*> getNewId

-- | Pure argument encoding and metadata for one generated request.
data RequestEncoder fd = RequestEncoder
  { requestName :: Text
  , requestOpcode :: Opcode
  , requestSince :: Word32
  , requestDestructor :: Bool
  , requestArguments :: ArgumentEncoder fd ()
  }

-- | Pure argument decoding and version metadata for one generated event.
data EventDecoder fd event = EventDecoder
  { eventName :: Text
  , eventSince :: Word32
  , eventDestructor :: Bool
  , eventArguments :: ArgumentDecoder fd event
  , eventNewIds :: event -> [ObjectId]
  }
