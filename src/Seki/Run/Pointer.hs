-- | River pointer bindings on a seat.
module Seki.Run.Pointer (
  RiverPointerBinding (..),
  createRiverPointerBinding,
)
where

import Data.Word (Word32)
import Seki.Protocol.RiverWindowManagementV1 qualified as River
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  eventRoute,
  submitNewObjectRequestWithRoute,
 )
import Seki.Wire.Connection.ObjectTable (Object, objectId, objectVersion)
import Seki.Wire.Transport (OwnedFd)

-- | A Linux input event code, such as @0x110@ for the left button, and the modifiers held with it.
data RiverPointerBinding = RiverPointerBinding
  { riverPointerButton :: Word32
  , riverPointerModifiers :: River.RiverSeatV1ModifiersFlags
  }
  deriving (Eq, Show)

-- | Create one initially disabled binding and install its event route atomically.
createRiverPointerBinding ::
  Connection ->
  Object River.RiverSeatV1 ->
  RiverPointerBinding ->
  (Object River.RiverPointerBindingV1 -> River.RiverPointerBindingV1Event OwnedFd -> IO ()) ->
  IO (Either ConnectionError (Object River.RiverPointerBindingV1))
createRiverPointerBinding connection seat definition handler =
  submitNewObjectRequestWithRoute
    connection
    River.riverSeatV1Interface
    seat
    River.riverPointerBindingV1Interface
    (objectVersion seat)
    ( \binding ->
        River.encodeRiverSeatV1GetPointerBinding
          (objectId binding)
          (riverPointerButton definition)
          (riverPointerModifiers definition)
    )
    (eventRoute River.decodeRiverPointerBindingV1Event handler)
