{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Strict Wayland protocol XML parsing and cross-document validation.
module Seki.Scanner.Parse (
  ScannerError (..),
  ScannerProblem (..),
  parseProtocolBytes,
  parseProtocolFile,
  validateProtocols,
)
where

import Control.Exception (IOException, try)
import Control.Monad (foldM, unless, when, zipWithM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (chr, isAscii, isAsciiLower, isDigit)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16, Word32)
import Numeric (readHex)
import Seki.Scanner.IR (
  Argument (..),
  ArgumentType (..),
  Description (..),
  EnumDefinition (..),
  EnumEntry (..),
  Interface (..),
  Message (..),
  Protocol (..),
 )
import Text.Read (readMaybe)
import Xeno.DOM.Robust qualified as Xml
import Xeno.SAX qualified as Sax

data ScannerError = ScannerError
  { scannerContext :: [Text]
  , scannerProblem :: ScannerProblem
  }
  deriving (Eq, Show)

data ScannerProblem
  = InputReadError Text
  | XmlSyntaxError Text
  | XmlDoctypeNotAllowed
  | XmlMultipleRoots
  | XmlMissingRoot
  | XmlMismatchedClosingTag Text Text
  | XmlTextOutsideRoot
  | InvalidUtf8
  | InvalidXmlEntity Text
  | UnexpectedRoot Text
  | UnknownElement Text
  | UnknownAttribute Text
  | DuplicateAttribute Text
  | MissingAttribute Text
  | InvalidAttribute Text Text
  | UnexpectedElementText
  | DuplicateSingletonElement Text
  | DuplicateName Text
  | MessageOpcodeOverflow Text Int
  | VersionOutOfRange Word32 Word32
  | NumericValueOutOfRange Integer
  | DuplicateProtocolName Text
  | DuplicateInterfaceName Text
  | UnknownInterfaceReference Text
  | UnknownEnumReference Text
  deriving (Eq, Show)

parseProtocolBytes :: ByteString -> Either ScannerError Protocol
parseProtocolBytes bytes = do
  validateXmlStructure bytes
  root <- first (syntaxError . Text.pack . show) (Xml.parse bytes)
  parseProtocolNode root

parseProtocolFile :: FilePath -> IO (Either ScannerError Protocol)
parseProtocolFile path = do
  result <- try @IOException (ByteString.readFile path)
  pure $ case result of
    Left exception -> Left (ScannerError [Text.pack path] (InputReadError (Text.pack (show exception))))
    Right bytes -> first (prependContext (Text.pack path)) (parseProtocolBytes bytes)

validateProtocols :: [Protocol] -> Either [ScannerError] [Protocol]
validateProtocols protocols =
  case protocolErrors <> interfaceErrors <> referenceErrors of
    [] -> Right protocols
    errors -> Left errors
 where
  protocolErrors = duplicateErrors protocolName DuplicateProtocolName ["protocols"] protocols
  interfaces = [(protocolName protocol, interface) | protocol <- protocols, interface <- protocolInterfaces protocol]
  interfaceErrors = duplicateErrors (interfaceName . snd) DuplicateInterfaceName ["protocols"] interfaces
  interfaceMap = Map.fromList [(interfaceName interface, interface) | (_, interface) <- interfaces]
  referenceErrors = concatMap (validateInterfaceReferences interfaceMap) interfaces

data StructureState = StructureState
  { structureStack :: [ByteString]
  , structureRoots :: Int
  , structureProblem :: Maybe ScannerProblem
  }

validateXmlStructure :: ByteString -> Either ScannerError ()
validateXmlStructure bytes
  | "<!DOCTYPE" `ByteString.isInfixOf` bytes = Left (ScannerError [] XmlDoctypeNotAllowed)
  | otherwise = do
      state <- first (syntaxError . Text.pack . show) (Sax.fold openTag attribute endOpen textNode closeTag cdata initial bytes)
      maybe (pure ()) (Left . ScannerError []) (structureProblem state)
      unless (null (structureStack state)) (Left (syntaxError "unclosed XML element"))
      case structureRoots state of
        0 -> Left (ScannerError [] XmlMissingRoot)
        1 -> Right ()
        _ -> Left (ScannerError [] XmlMultipleRoots)
 where
  initial = StructureState [] 0 Nothing

  openTag state name
    | hasProblem state = state
    | null (structureStack state) && structureRoots state > 0 = state{structureProblem = Just XmlMultipleRoots}
    | otherwise =
        state
          { structureStack = name : structureStack state
          , structureRoots = structureRoots state + if null (structureStack state) then 1 else 0
          }

  attribute state _ _ = state
  endOpen state _ = state
  cdata = textNode

  textNode state value
    | hasProblem state = state
    | null (structureStack state) && not (xmlWhitespace value) = state{structureProblem = Just XmlTextOutsideRoot}
    | otherwise = state

  closeTag state actual
    | hasProblem state = state
    | otherwise = case structureStack state of
        [] -> state{structureProblem = Just (XmlMismatchedClosingTag "" (decodeNameLenient actual))}
        expected : rest
          | expected == actual -> state{structureStack = rest}
          | otherwise ->
              state
                { structureProblem =
                    Just (XmlMismatchedClosingTag (decodeNameLenient expected) (decodeNameLenient actual))
                }

hasProblem :: StructureState -> Bool
hasProblem = isJust . structureProblem

parseProtocolNode :: Xml.Node -> Either ScannerError Protocol
parseProtocolNode node = do
  rootName <- decodeName (Xml.name node)
  unless (rootName == "protocol") (problem [] (UnexpectedRoot rootName))
  attributes <- parseAttributes ["protocol"] ["name"] node
  name <- requiredAttribute ["protocol"] "name" attributes >>= validateIdentifier ["protocol", "@name"]
  children <- containerChildren ["protocol", name] node
  copyright <- parseOptionalSingleton ["protocol", name] "copyright" parseCopyright children
  description <- parseOptionalSingleton ["protocol", name] "description" parseDescription children
  interfaces <- mapM (parseInterface name) [child | child <- children, nodeNameIs "interface" child]
  rejectUnknownChildren ["protocol", name] ["copyright", "description", "interface"] children
  ensureUnique ["protocol", name] interfaceName interfaces
  pure
    Protocol
      { protocolName = name
      , protocolCopyright = copyright
      , protocolDescription = description
      , protocolInterfaces = interfaces
      }

parseCopyright :: [Text] -> Xml.Node -> Either ScannerError Text
parseCopyright context node = do
  _ <- parseAttributes context [] node
  Text.strip <$> textOnlyContent context node

parseDescription :: [Text] -> Xml.Node -> Either ScannerError Description
parseDescription context node = do
  attributes <- parseAttributes context ["summary"] node
  summary <- requiredAttribute context "summary" attributes
  body <- Text.strip <$> textOnlyContent context node
  pure Description{descriptionSummary = summary, descriptionBody = body}

parseInterface :: Text -> Xml.Node -> Either ScannerError Interface
parseInterface protocolNameValue node = do
  let baseContext = ["protocol", protocolNameValue, "interface"]
  attributes <- parseAttributes baseContext ["name", "version", "frozen"] node
  name <- requiredAttribute baseContext "name" attributes >>= validateIdentifier (baseContext <> ["@name"])
  let context = baseContext <> [name]
  version <- requiredAttribute context "version" attributes >>= parsePositiveWord32 context "version"
  frozen <- optionalBoolean context "frozen" False attributes
  children <- containerChildren context node
  description <- parseOptionalSingleton context "description" parseDescription children
  let requestNodes = [child | child <- children, nodeNameIs "request" child]
      eventNodes = [child | child <- children, nodeNameIs "event" child]
      enumNodes = [child | child <- children, nodeNameIs "enum" child]
  rejectUnknownChildren context ["description", "request", "event", "enum"] children
  requests <- parseMessages context version True requestNodes
  events <- parseMessages context version False eventNodes
  enums <- mapM (parseEnum context version) enumNodes
  ensureUnique context messageName requests
  ensureUnique context messageName events
  ensureUnique context enumName enums
  pure
    Interface
      { interfaceName = name
      , interfaceVersion = version
      , interfaceFrozen = frozen
      , interfaceDescription = description
      , interfaceRequests = requests
      , interfaceEvents = events
      , interfaceEnums = enums
      }

parseMessages :: [Text] -> Word32 -> Bool -> [Xml.Node] -> Either ScannerError [Message]
parseMessages context interfaceVersionValue requests nodes
  | length nodes > fromIntegral (maxBound :: Word16) + 1 =
      problem context (MessageOpcodeOverflow kindName (length nodes))
  | otherwise = zipWithM (parseMessage context interfaceVersionValue requests) [0 ..] nodes
 where
  kindName = if requests then "request" else "event"

parseMessage :: [Text] -> Word32 -> Bool -> Int -> Xml.Node -> Either ScannerError Message
parseMessage interfaceContext interfaceVersionValue requestMessage opcode node = do
  let kind = if requestMessage then "request" else "event"
      allowed = ["name", "since", "deprecated-since", "type"]
      baseContext = interfaceContext <> [kind]
  attributes <- parseAttributes baseContext allowed node
  name <- requiredAttribute baseContext "name" attributes >>= validateIdentifier (baseContext <> ["@name"])
  let context = baseContext <> [name]
  since <- optionalVersion context "since" 1 interfaceVersionValue attributes
  deprecated <- optionalDeprecatedVersion context since interfaceVersionValue attributes
  destructor <- case Map.lookup "type" attributes of
    Nothing -> Right False
    Just "destructor" -> Right True
    Just value -> problem context (InvalidAttribute "type" value)
  children <- containerChildren context node
  description <- parseOptionalSingleton context "description" parseDescription children
  arguments <- mapM (parseArgument context) [child | child <- children, nodeNameIs "arg" child]
  rejectUnknownChildren context ["description", "arg"] children
  ensureUnique context argumentName arguments
  pure
    Message
      { messageName = name
      , messageOpcode = fromIntegral opcode
      , messageSince = since
      , messageDeprecatedSince = deprecated
      , messageDestructor = destructor
      , messageDescription = description
      , messageArguments = arguments
      }

parseArgument :: [Text] -> Xml.Node -> Either ScannerError Argument
parseArgument messageContext node = do
  let baseContext = messageContext <> ["arg"]
  attributes <- parseAttributes baseContext ["name", "type", "allow-null", "interface", "enum", "summary"] node
  name <- requiredAttribute baseContext "name" attributes >>= validateIdentifier (baseContext <> ["@name"])
  let context = baseContext <> [name]
  typeValue <- requiredAttribute context "type" attributes >>= parseArgumentType context
  nullable <- optionalBoolean context "allow-null" False attributes
  when (nullable && typeValue `notElem` [ArgumentString, ArgumentObject]) $
    problem context (InvalidAttribute "allow-null" "true")
  let interfaceReference = Map.lookup "interface" attributes
      enumReference = Map.lookup "enum" attributes
  when (isJustValue interfaceReference && typeValue `notElem` [ArgumentObject, ArgumentNewId]) $
    problem context (InvalidAttribute "interface" (fromMaybe "" interfaceReference))
  when (isJustValue enumReference && typeValue `notElem` [ArgumentInt, ArgumentUint]) $
    problem context (InvalidAttribute "enum" (fromMaybe "" enumReference))
  children <- containerChildren context node
  case children of
    child : _ -> nodeName child >>= problem context . UnknownElement
    [] -> pure ()
  pure
    Argument
      { argumentName = name
      , argumentType = typeValue
      , argumentNullable = nullable
      , argumentInterface = interfaceReference
      , argumentEnum = enumReference
      , argumentSummary = Map.lookup "summary" attributes
      }

parseEnum :: [Text] -> Word32 -> Xml.Node -> Either ScannerError EnumDefinition
parseEnum interfaceContext interfaceVersionValue node = do
  let baseContext = interfaceContext <> ["enum"]
  attributes <- parseAttributes baseContext ["name", "since", "bitfield"] node
  name <- requiredAttribute baseContext "name" attributes >>= validateIdentifier (baseContext <> ["@name"])
  let context = baseContext <> [name]
  since <- optionalVersion context "since" 1 interfaceVersionValue attributes
  bitfield <- optionalBoolean context "bitfield" False attributes
  children <- containerChildren context node
  description <- parseOptionalSingleton context "description" parseDescription children
  entries <- mapM (parseEnumEntry context interfaceVersionValue since) [child | child <- children, nodeNameIs "entry" child]
  rejectUnknownChildren context ["description", "entry"] children
  ensureUnique context enumEntryName entries
  pure
    EnumDefinition
      { enumName = name
      , enumSince = since
      , enumBitfield = bitfield
      , enumDescription = description
      , enumEntries = entries
      }

parseEnumEntry :: [Text] -> Word32 -> Word32 -> Xml.Node -> Either ScannerError EnumEntry
parseEnumEntry enumContext interfaceVersionValue enumVersion node = do
  let baseContext = enumContext <> ["entry"]
  attributes <- parseAttributes baseContext ["name", "value", "since", "deprecated-since", "summary"] node
  name <- requiredAttribute baseContext "name" attributes >>= validateIdentifier (baseContext <> ["@name"])
  let context = baseContext <> [name]
  value <- requiredAttribute context "value" attributes >>= parseEnumValue context
  since <- optionalVersion context "since" enumVersion interfaceVersionValue attributes
  deprecated <- optionalDeprecatedVersion context since interfaceVersionValue attributes
  children <- containerChildren context node
  description <- parseOptionalSingleton context "description" parseDescription children
  rejectUnknownChildren context ["description"] children
  pure
    EnumEntry
      { enumEntryName = name
      , enumEntryValue = value
      , enumEntrySince = since
      , enumEntryDeprecatedSince = deprecated
      , enumEntrySummary = Map.lookup "summary" attributes
      , enumEntryDescription = description
      }

parseArgumentType :: [Text] -> Text -> Either ScannerError ArgumentType
parseArgumentType context value = case value of
  "int" -> Right ArgumentInt
  "uint" -> Right ArgumentUint
  "fixed" -> Right ArgumentFixed
  "string" -> Right ArgumentString
  "object" -> Right ArgumentObject
  "new_id" -> Right ArgumentNewId
  "array" -> Right ArgumentArray
  "fd" -> Right ArgumentFd
  _ -> problem context (InvalidAttribute "type" value)

parseEnumValue :: [Text] -> Text -> Either ScannerError Integer
parseEnumValue context value = do
  parsed <- maybe (problem context (InvalidAttribute "value" value)) Right (readInteger value)
  if parsed < -2147483648 || parsed > 4294967295
    then problem context (NumericValueOutOfRange parsed)
    else Right parsed

readInteger :: Text -> Maybe Integer
readInteger value
  | Just hexadecimal <- Text.stripPrefix "0x" value = readHexInteger False hexadecimal
  | Just hexadecimal <- Text.stripPrefix "-0x" value = readHexInteger True hexadecimal
  | otherwise = readMaybe (Text.unpack value)

readHexInteger :: Bool -> Text -> Maybe Integer
readHexInteger negative digits = case readHex (Text.unpack digits) of
  [(value, "")] -> Just (if negative then -value else value)
  _ -> Nothing

parsePositiveWord32 :: [Text] -> Text -> Text -> Either ScannerError Word32
parsePositiveWord32 context attribute value = case readMaybe (Text.unpack value) of
  Just parsed | parsed > 0 -> Right parsed
  _ -> problem context (InvalidAttribute attribute value)

optionalVersion :: [Text] -> Text -> Word32 -> Word32 -> Map Text Text -> Either ScannerError Word32
optionalVersion context attribute defaultValue maximumVersion attributes = do
  value <- maybe (Right defaultValue) (parsePositiveWord32 context attribute) (Map.lookup attribute attributes)
  if value <= maximumVersion
    then Right value
    else problem context (VersionOutOfRange value maximumVersion)

optionalDeprecatedVersion :: [Text] -> Word32 -> Word32 -> Map Text Text -> Either ScannerError (Maybe Word32)
optionalDeprecatedVersion context since maximumVersion attributes = case Map.lookup "deprecated-since" attributes of
  Nothing -> Right Nothing
  Just raw -> do
    value <- parsePositiveWord32 context "deprecated-since" raw
    if value >= since && value <= maximumVersion
      then Right (Just value)
      else problem context (VersionOutOfRange value maximumVersion)

optionalBoolean :: [Text] -> Text -> Bool -> Map Text Text -> Either ScannerError Bool
optionalBoolean context attribute defaultValue attributes = case Map.lookup attribute attributes of
  Nothing -> Right defaultValue
  Just "true" -> Right True
  Just "false" -> Right False
  Just value -> problem context (InvalidAttribute attribute value)

parseAttributes :: [Text] -> [Text] -> Xml.Node -> Either ScannerError (Map Text Text)
parseAttributes context allowed node = foldM insertAttribute Map.empty (Xml.attributes node)
 where
  allowedSet = Set.fromList allowed
  insertAttribute attributes (rawName, rawValue) = do
    name <- decodeName rawName
    unless (name `Set.member` allowedSet) (problem context (UnknownAttribute name))
    when (Map.member name attributes) (problem context (DuplicateAttribute name))
    value <- decodeXmlText context rawValue
    pure (Map.insert name value attributes)

requiredAttribute :: [Text] -> Text -> Map Text Text -> Either ScannerError Text
requiredAttribute context name attributes =
  maybe (problem context (MissingAttribute name)) Right (Map.lookup name attributes)

containerChildren :: [Text] -> Xml.Node -> Either ScannerError [Xml.Node]
containerChildren context node = fmap catMaybes . mapM contentNode $ Xml.contents node
 where
  contentNode = \case
    Xml.Element child -> Right (Just child)
    Xml.Text bytes -> validateWhitespace bytes
    Xml.CData bytes -> validateWhitespace bytes

  validateWhitespace bytes = do
    value <- decodeXmlText context bytes
    if Text.all (`elem` [' ', '\t', '\r', '\n']) value
      then Right Nothing
      else problem context UnexpectedElementText

textOnlyContent :: [Text] -> Xml.Node -> Either ScannerError Text
textOnlyContent context node = Text.concat <$> mapM contentText (Xml.contents node)
 where
  contentText = \case
    Xml.Element child -> nodeName child >>= problem context . UnknownElement
    Xml.Text bytes -> decodeXmlText context bytes
    Xml.CData bytes -> first (const (ScannerError context InvalidUtf8)) (TextEncoding.decodeUtf8' bytes)

parseOptionalSingleton ::
  [Text] ->
  Text ->
  ([Text] -> Xml.Node -> Either ScannerError value) ->
  [Xml.Node] ->
  Either ScannerError (Maybe value)
parseOptionalSingleton context name parser children = case filter (nodeNameIs name) children of
  [] -> Right Nothing
  [node] -> Just <$> parser (context <> [name]) node
  _ -> problem context (DuplicateSingletonElement name)

rejectUnknownChildren :: [Text] -> [Text] -> [Xml.Node] -> Either ScannerError ()
rejectUnknownChildren context allowed = mapM_ $ \child -> do
  name <- nodeName child
  unless (name `elem` allowed) (problem context (UnknownElement name))

ensureUnique :: [Text] -> (value -> Text) -> [value] -> Either ScannerError ()
ensureUnique context nameOf values = case firstDuplicate (map nameOf values) of
  Nothing -> Right ()
  Just duplicate -> problem context (DuplicateName duplicate)

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate = go Set.empty
 where
  go _ [] = Nothing
  go seen (value : rest)
    | value `Set.member` seen = Just value
    | otherwise = go (Set.insert value seen) rest

duplicateErrors :: (Ord name) => (value -> name) -> (name -> ScannerProblem) -> [Text] -> [value] -> [ScannerError]
duplicateErrors nameOf makeProblem context values =
  [ScannerError context (makeProblem duplicate) | duplicate <- duplicates (map nameOf values)]

duplicates :: (Ord value) => [value] -> [value]
duplicates values = Map.keys (Map.filter (> (1 :: Int)) counts)
 where
  counts = foldl' (\result value -> Map.insertWith (+) value 1 result) Map.empty values

validateInterfaceReferences :: Map Text Interface -> (Text, Interface) -> [ScannerError]
validateInterfaceReferences interfaces (protocolNameValue, interface) =
  concatMap validateMessage (interfaceRequests interface <> interfaceEvents interface)
 where
  context = ["protocol", protocolNameValue, "interface", interfaceName interface]
  localEnums = Set.fromList (map enumName (interfaceEnums interface))

  validateMessage message = concatMap (validateArgument message) (messageArguments message)

  validateArgument message argument =
    catMaybes
      [ validateInterfaceReference argumentContext =<< argumentInterface argument
      , validateEnumReference argumentContext =<< argumentEnum argument
      ]
   where
    argumentContext = context <> ["message", messageName message, "arg", argumentName argument]

  validateInterfaceReference argumentContext reference
    | Map.member reference interfaces = Nothing
    | otherwise = Just (ScannerError argumentContext (UnknownInterfaceReference reference))

  validateEnumReference argumentContext reference = case Text.splitOn "." reference of
    [local]
      | local `Set.member` localEnums -> Nothing
      | otherwise -> Just (ScannerError argumentContext (UnknownEnumReference reference))
    [interfaceReference, enumReference] -> case Map.lookup interfaceReference interfaces of
      Just target | enumReference `elem` map enumName (interfaceEnums target) -> Nothing
      _ -> Just (ScannerError argumentContext (UnknownEnumReference reference))
    _ -> Just (ScannerError argumentContext (UnknownEnumReference reference))

decodeName :: ByteString -> Either ScannerError Text
decodeName = first (const (ScannerError [] InvalidUtf8)) . TextEncoding.decodeUtf8'

decodeNameLenient :: ByteString -> Text
decodeNameLenient = fromRight "<invalid UTF-8>" . TextEncoding.decodeUtf8'

decodeXmlText :: [Text] -> ByteString -> Either ScannerError Text
decodeXmlText context bytes = do
  value <- first (const (ScannerError context InvalidUtf8)) (TextEncoding.decodeUtf8' bytes)
  decodeEntities context value

decodeEntities :: [Text] -> Text -> Either ScannerError Text
decodeEntities context value = Text.concat <$> traverse decodePart (splitEntities value)
 where
  decodePart (Left plain) = Right plain
  decodePart (Right entity) = maybe (problem context (InvalidXmlEntity entity)) (Right . Text.singleton) (entityCharacter entity)

splitEntities :: Text -> [Either Text Text]
splitEntities value
  | Text.null value = []
  | otherwise = case Text.breakOn "&" value of
      (plain, rest)
        | Text.null rest -> [Left plain | not (Text.null plain)]
        | otherwise ->
            let afterAmpersand = Text.drop 1 rest
                (entity, suffix) = Text.breakOn ";" afterAmpersand
                prefix = [Left plain | not (Text.null plain)]
             in if Text.null suffix
                  then prefix <> [Right afterAmpersand]
                  else prefix <> [Right entity] <> splitEntities (Text.drop 1 suffix)

entityCharacter :: Text -> Maybe Char
entityCharacter entity = case entity of
  "lt" -> Just '<'
  "gt" -> Just '>'
  "amp" -> Just '&'
  "apos" -> Just '\''
  "quot" -> Just '"'
  _
    | Just hexadecimal <- Text.stripPrefix "#x" entity -> codePoint 16 hexadecimal
    | Just decimal <- Text.stripPrefix "#" entity -> codePoint 10 decimal
    | otherwise -> Nothing
 where
  codePoint :: Int -> Text -> Maybe Char
  codePoint base digits = do
    value <- case base of
      16 -> case readHex (Text.unpack digits) of
        [(parsed, "")] -> Just parsed
        _ -> Nothing
      _ -> readMaybe (Text.unpack digits)
    if validXmlCodePoint value then Just (chr value) else Nothing

validXmlCodePoint :: Int -> Bool
validXmlCodePoint value =
  value == 0x9
    || value == 0xa
    || value == 0xd
    || (value >= 0x20 && value <= 0xd7ff)
    || (value >= 0xe000 && value <= 0xfffd)
    || (value >= 0x10000 && value <= 0x10ffff)

validateIdentifier :: [Text] -> Text -> Either ScannerError Text
validateIdentifier context value
  | Text.null value = problem context (InvalidAttribute "name" value)
  | Text.all validCharacter value = Right value
  | otherwise = problem context (InvalidAttribute "name" value)
 where
  validCharacter character = isAsciiLower character || (isAscii character && isDigit character) || character == '_'

nodeName :: Xml.Node -> Either ScannerError Text
nodeName = decodeName . Xml.name

nodeNameIs :: Text -> Xml.Node -> Bool
nodeNameIs expected node = decodeNameLenient (Xml.name node) == expected

xmlWhitespace :: ByteString -> Bool
xmlWhitespace = ByteString.all (`elem` [0x20, 0x09, 0x0a, 0x0d])

isJustValue :: Maybe value -> Bool
isJustValue = isJust

problem :: [Text] -> ScannerProblem -> Either ScannerError value
problem context problemValue = Left (ScannerError context problemValue)

syntaxError :: Text -> ScannerError
syntaxError message = ScannerError [] (XmlSyntaxError message)

prependContext :: Text -> ScannerError -> ScannerError
prependContext context errorValue = errorValue{scannerContext = context : scannerContext errorValue}
