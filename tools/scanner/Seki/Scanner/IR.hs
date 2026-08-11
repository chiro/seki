-- | Validated, protocol-neutral Wayland XML representation.
module Seki.Scanner.IR (
  Protocol (..),
  Description (..),
  Interface (..),
  Message (..),
  Argument (..),
  ArgumentType (..),
  EnumDefinition (..),
  EnumEntry (..),
)
where

import Data.Text (Text)
import Data.Word (Word16, Word32)

data Protocol = Protocol
  { protocolName :: Text
  , protocolCopyright :: Maybe Text
  , protocolDescription :: Maybe Description
  , protocolInterfaces :: [Interface]
  }
  deriving (Eq, Show)

data Description = Description
  { descriptionSummary :: Text
  , descriptionBody :: Text
  }
  deriving (Eq, Show)

data Interface = Interface
  { interfaceName :: Text
  , interfaceVersion :: Word32
  , interfaceFrozen :: Bool
  , interfaceDescription :: Maybe Description
  , interfaceRequests :: [Message]
  , interfaceEvents :: [Message]
  , interfaceEnums :: [EnumDefinition]
  }
  deriving (Eq, Show)

data Message = Message
  { messageName :: Text
  , messageOpcode :: Word16
  , messageSince :: Word32
  , messageDeprecatedSince :: Maybe Word32
  , messageDestructor :: Bool
  , messageDescription :: Maybe Description
  , messageArguments :: [Argument]
  }
  deriving (Eq, Show)

data Argument = Argument
  { argumentName :: Text
  , argumentType :: ArgumentType
  , argumentNullable :: Bool
  , argumentInterface :: Maybe Text
  , argumentEnum :: Maybe Text
  , argumentSummary :: Maybe Text
  }
  deriving (Eq, Show)

data ArgumentType
  = ArgumentInt
  | ArgumentUint
  | ArgumentFixed
  | ArgumentString
  | ArgumentObject
  | ArgumentNewId
  | ArgumentArray
  | ArgumentFd
  deriving (Eq, Ord, Show)

data EnumDefinition = EnumDefinition
  { enumName :: Text
  , enumSince :: Word32
  , enumBitfield :: Bool
  , enumDescription :: Maybe Description
  , enumEntries :: [EnumEntry]
  }
  deriving (Eq, Show)

data EnumEntry = EnumEntry
  { enumEntryName :: Text
  , enumEntryValue :: Integer
  , enumEntrySince :: Word32
  , enumEntryDeprecatedSince :: Maybe Word32
  , enumEntrySummary :: Maybe Text
  , enumEntryDescription :: Maybe Description
  }
  deriving (Eq, Show)
