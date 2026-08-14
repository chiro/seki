{-# LANGUAGE OverloadedStrings #-}

-- | Pure typed selection and lifecycle finalization of Wayland events.
module Seki.Wire.Connection.Event (
  PreparedEvent,
  preparedEventSender,
  preparedEventInterface,
  preparedEventName,
  preparedEventOpcode,
  preparedEventSince,
  preparedEventDestructor,
  preparedEventArguments,
  preparedEventNewIds,
  preparedEventVersion,
  EventPreparationError (..),
  prepareEvent,
  finalizeDecodedEvent,
  finalizeDeleteIdEvent,
)
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Wire.Arguments (ArgumentDecoder)
import Seki.Wire.Binding (EventDecoder (..), Interface (..))
import Seki.Wire.Connection.ObjectTable (
  Object,
  ObjectInfo (..),
  ObjectTable,
  ObjectTableError,
  acknowledgeDeleteId,
  destroyObjectByEvent,
  objectId,
  validateObject,
 )
import Seki.Wire.Types (ObjectId, Opcode, RawFrame (..))

-- | A generated decoder selected for one validated event frame.
data PreparedEvent interface fd event = PreparedEvent
  { preparedEventSender :: ObjectId
  , preparedEventInterface :: Text
  , preparedEventName :: Text
  , preparedEventOpcode :: Opcode
  , preparedEventSince :: Word32
  , preparedEventDestructor :: Bool
  , preparedEventArguments :: ArgumentDecoder fd event
  , preparedEventNewIds :: event -> [ObjectId]
  , preparedEventVersion :: Word32
  , preparedEventObject :: Object interface
  }

data EventPreparationError
  = EventObjectError ObjectTableError
  | EventSenderMismatch ObjectId ObjectId
  | UnexpectedEventOpcode Text Opcode
  | InvalidEventSince Text Text Word32
  | UnsupportedEventVersion Text Text Word32 Word32
  | ExpectedDeleteIdEvent Text Text
  deriving (Eq, Show)

{- | Validate the sender and select its generated event decoder.

No lifecycle state changes here: the caller must first decode the complete
argument sequence and then call 'finalizeDecodedEvent'.
-}
prepareEvent ::
  Interface interface ->
  Object interface ->
  (Opcode -> Maybe (EventDecoder fd event)) ->
  RawFrame ->
  ObjectTable ->
  Either EventPreparationError (PreparedEvent interface fd event)
prepareEvent interface sender decodeEvent frame table = do
  if rawFrameSender frame == objectId sender
    then pure ()
    else Left (EventSenderMismatch (objectId sender) (rawFrameSender frame))
  senderInfo <- first EventObjectError (validateObject interface sender table)
  decoder <-
    maybe
      (Left (UnexpectedEventOpcode (interfaceName interface) (rawFrameOpcode frame)))
      Right
      (decodeEvent (rawFrameOpcode frame))
  validateEventVersion interface senderInfo decoder
  pure
    PreparedEvent
      { preparedEventSender = rawFrameSender frame
      , preparedEventInterface = interfaceName interface
      , preparedEventName = eventName decoder
      , preparedEventOpcode = rawFrameOpcode frame
      , preparedEventSince = eventSince decoder
      , preparedEventDestructor = eventDestructor decoder
      , preparedEventArguments = eventArguments decoder
      , preparedEventNewIds = eventNewIds decoder
      , preparedEventVersion = objectInfoVersion senderInfo
      , preparedEventObject = sender
      }

-- | Apply sender destruction only after its event arguments decoded fully.
finalizeDecodedEvent ::
  Interface interface ->
  PreparedEvent interface fd event ->
  ObjectTable ->
  Either EventPreparationError ObjectTable
finalizeDecodedEvent interface prepared table = do
  _ <- first EventObjectError (validateObject interface (preparedEventObject prepared) table)
  if preparedEventDestructor prepared
    then first EventObjectError (destroyObjectByEvent (preparedEventObject prepared) table)
    else Right table

{- | Finalize the core @wl_display.delete_id@ event and release its client ID.

The decoded ID is supplied separately so this runtime layer stays independent
of the generated core event ADT.
-}
finalizeDeleteIdEvent ::
  Interface display ->
  PreparedEvent display fd event ->
  Word32 ->
  ObjectTable ->
  Either EventPreparationError ObjectTable
finalizeDeleteIdEvent interface prepared deletedId table
  | preparedEventInterface prepared /= "wl_display" || preparedEventName prepared /= "delete_id" =
      Left (ExpectedDeleteIdEvent (preparedEventInterface prepared) (preparedEventName prepared))
  | otherwise = do
      finalized <- finalizeDecodedEvent interface prepared table
      first EventObjectError (acknowledgeDeleteId deletedId finalized)

validateEventVersion ::
  Interface interface ->
  ObjectInfo ->
  EventDecoder fd event ->
  Either EventPreparationError ()
validateEventVersion interface senderInfo decoder
  | eventSince decoder == 0 =
      Left (InvalidEventSince interfaceValue eventValue 0)
  | eventSince decoder > objectInfoVersion senderInfo =
      Left
        ( UnsupportedEventVersion
            interfaceValue
            eventValue
            (eventSince decoder)
            (objectInfoVersion senderInfo)
        )
  | otherwise = Right ()
 where
  interfaceValue = interfaceName interface
  eventValue = eventName decoder
