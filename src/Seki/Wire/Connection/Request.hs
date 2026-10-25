-- | Pure, typed preparation of generated Wayland requests.
module Seki.Wire.Connection.Request (
  PreparedRequest (..),
  RequestPreparationError (..),
  prepareRequest,
  prepareNewObjectRequest,
  prepareInheritedObjectRequest,
)
where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Wire.Arguments (ArgumentError, runArgumentEncoder)
import Seki.Wire.Binding (Interface (..), RequestEncoder (..))
import Seki.Wire.Codec (encodeFrame)
import Seki.Wire.Connection.ObjectTable (
  Object,
  ObjectInfo (..),
  ObjectTable,
  ObjectTableError,
  allocateClientObject,
  destroyObjectByRequest,
  objectId,
  objectVersion,
  validateObject,
 )
import Seki.Wire.Types (FrameError, RawFrame (..))

-- | A complete encoded frame and its ordered ancillary descriptors.
data PreparedRequest fd = PreparedRequest
  { preparedRequestBytes :: ByteString
  , preparedRequestFds :: [fd]
  }
  deriving (Eq, Show)

data RequestPreparationError
  = RequestObjectError ObjectTableError
  | InvalidRequestSince Text Text Word32
  | UnsupportedRequestVersion Text Text Word32 Word32
  | RequestArgumentError Text Text ArgumentError
  | RequestFrameError Text Text FrameError
  deriving (Eq, Show)

-- | Validate and encode a generated request, applying destructor state last.
prepareRequest ::
  Interface interface ->
  Object interface ->
  RequestEncoder fd ->
  ObjectTable ->
  Either RequestPreparationError (ObjectTable, PreparedRequest fd)
prepareRequest interface sender request table = do
  senderInfo <- first RequestObjectError (validateObject interface sender table)
  validateRequestVersion interface senderInfo request
  ((), payload, descriptors) <-
    first
      (RequestArgumentError (interfaceName interface) (requestName request))
      (runArgumentEncoder (requestArguments request))
  bytes <-
    first
      (RequestFrameError (interfaceName interface) (requestName request))
      ( encodeFrame
          RawFrame
            { rawFrameSender = objectId sender
            , rawFrameOpcode = requestOpcode request
            , rawFramePayload = payload
            }
      )
  nextTable <-
    if requestDestructor request
      then first RequestObjectError (destroyObjectByRequest sender table)
      else Right table
  pure
    ( nextTable
    , PreparedRequest
        { preparedRequestBytes = bytes
        , preparedRequestFds = descriptors
        }
    )

{- | Allocate and register a client object before preparing the request that
creates it. Failure returns no updated table, so allocation and encoding form
one pure transaction.
-}
prepareNewObjectRequest ::
  Interface parent ->
  Object parent ->
  Interface child ->
  Word32 ->
  (Object child -> RequestEncoder fd) ->
  ObjectTable ->
  Either RequestPreparationError (ObjectTable, Object child, PreparedRequest fd)
prepareNewObjectRequest parentInterface parent childInterface childVersion makeRequest table = do
  _ <- first RequestObjectError (validateObject parentInterface parent table)
  (tableWithChild, child) <-
    first RequestObjectError (allocateClientObject childInterface childVersion table)
  (nextTable, prepared) <- prepareRequest parentInterface parent (makeRequest child) tableWithChild
  pure (nextTable, child, prepared)

-- | Create a child at the lower of its generated maximum and parent version.
prepareInheritedObjectRequest ::
  Interface parent ->
  Object parent ->
  Interface child ->
  (Object child -> RequestEncoder fd) ->
  ObjectTable ->
  Either RequestPreparationError (ObjectTable, Object child, PreparedRequest fd)
prepareInheritedObjectRequest parentInterface parent childInterface =
  prepareNewObjectRequest
    parentInterface
    parent
    childInterface
    (min (objectVersion parent) (interfaceMaximumVersion childInterface))

validateRequestVersion ::
  Interface interface ->
  ObjectInfo ->
  RequestEncoder fd ->
  Either RequestPreparationError ()
validateRequestVersion interface senderInfo request
  | requestSince request == 0 =
      Left (InvalidRequestSince interfaceValue requestValue 0)
  | requestSince request > objectInfoVersion senderInfo =
      Left
        ( UnsupportedRequestVersion
            interfaceValue
            requestValue
            (requestSince request)
            (objectInfoVersion senderInfo)
        )
  | otherwise = Right ()
 where
  interfaceValue = interfaceName interface
  requestValue = requestName request
