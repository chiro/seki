-- | Typed catalog and binding of globals announced by @wl_registry@.
module Seki.Wire.Connection.Registry (
  GlobalName,
  globalNameFromWord,
  globalNameToWord,
  GlobalAnnouncement (..),
  GlobalCatalog,
  RegistryError (..),
  emptyGlobalCatalog,
  applyRegistryEvent,
  announcedGlobals,
  Global,
  globalName,
  globalVersion,
  globalInterface,
  selectGlobal,
  selectGlobals,
  bindGlobal,
  bindGlobalWithRoute,
)
where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Protocol.Wayland qualified as Wayland
import Seki.Wire.Binding (DynamicNewId (..), Interface (..), RequestEncoder)
import Seki.Wire.Connection (
  Connection,
  ConnectionError,
  EventRoute,
  submitNewObjectRequest,
  submitNewObjectRequestWithRoute,
 )
import Seki.Wire.Connection.ObjectTable (Object, objectId)

newtype GlobalName = GlobalName Word32
  deriving (Eq, Ord, Show)

globalNameFromWord :: Word32 -> GlobalName
globalNameFromWord = GlobalName

globalNameToWord :: GlobalName -> Word32
globalNameToWord (GlobalName value) = value

data GlobalAnnouncement = GlobalAnnouncement
  { announcedGlobalName :: GlobalName
  , announcedGlobalInterface :: Text
  , announcedGlobalVersion :: Word32
  }
  deriving (Eq, Show)

newtype GlobalCatalog = GlobalCatalog (Map GlobalName GlobalAnnouncement)
  deriving (Eq, Show)

data RegistryError
  = InvalidAnnouncedGlobalVersion GlobalName Text Word32
  | DuplicateGlobalName GlobalName Text Text
  | UnknownGlobalName GlobalName
  | GlobalInterfaceMismatch GlobalName Text Text
  | InvalidGeneratedGlobalVersion Text Word32
  | InvalidRequestedGlobalVersion Text Word32
  deriving (Eq, Show)

emptyGlobalCatalog :: GlobalCatalog
emptyGlobalCatalog = GlobalCatalog Map.empty

-- | Apply one generated registry event in wire order.
applyRegistryEvent :: Wayland.WlRegistryEvent fd -> GlobalCatalog -> Either RegistryError GlobalCatalog
applyRegistryEvent event (GlobalCatalog globals) = case event of
  Wayland.WlRegistryGlobal rawName interfaceNameValue version -> do
    let name = GlobalName rawName
    if version == 0
      then Left (InvalidAnnouncedGlobalVersion name interfaceNameValue version)
      else case Map.lookup name globals of
        Just existing ->
          Left
            ( DuplicateGlobalName
                name
                (announcedGlobalInterface existing)
                interfaceNameValue
            )
        Nothing ->
          let announcement = GlobalAnnouncement name interfaceNameValue version
           in Right (GlobalCatalog (Map.insert name announcement globals))
  Wayland.WlRegistryGlobalRemove rawName ->
    let name = GlobalName rawName
     in if Map.member name globals
          then Right (GlobalCatalog (Map.delete name globals))
          else Left (UnknownGlobalName name)

announcedGlobals :: GlobalCatalog -> [GlobalAnnouncement]
announcedGlobals (GlobalCatalog globals) = sortOn announcedGlobalName (Map.elems globals)

data Global interface = Global
  { globalName :: GlobalName
  , globalVersion :: Word32
  , globalInterface :: Interface interface
  }
  deriving (Eq, Show)

{- | Select one announced global and negotiate the highest version permitted by
the compositor, generated binding, and caller-provided cap.
-}
selectGlobal ::
  Interface interface ->
  Word32 ->
  GlobalName ->
  GlobalCatalog ->
  Either RegistryError (Global interface)
selectGlobal interface requestedMaximum name (GlobalCatalog globals)
  | interfaceMaximumVersion interface == 0 =
      Left (InvalidGeneratedGlobalVersion (interfaceName interface) (interfaceMaximumVersion interface))
  | requestedMaximum == 0 = Left (InvalidRequestedGlobalVersion (interfaceName interface) requestedMaximum)
  | otherwise = do
      announcement <- maybe (Left (UnknownGlobalName name)) Right (Map.lookup name globals)
      if announcedGlobalInterface announcement /= interfaceName interface
        then
          Left
            ( GlobalInterfaceMismatch
                name
                (interfaceName interface)
                (announcedGlobalInterface announcement)
            )
        else
          Right
            Global
              { globalName = name
              , globalVersion =
                  minimum
                    [ announcedGlobalVersion announcement
                    , interfaceMaximumVersion interface
                    , requestedMaximum
                    ]
              , globalInterface = interface
              }

-- | Select all globals for an interface, ordered by numeric global name.
selectGlobals ::
  Interface interface ->
  Word32 ->
  GlobalCatalog ->
  Either RegistryError [Global interface]
selectGlobals interface requestedMaximum catalog
  | interfaceMaximumVersion interface == 0 =
      Left (InvalidGeneratedGlobalVersion (interfaceName interface) (interfaceMaximumVersion interface))
  | requestedMaximum == 0 =
      Left (InvalidRequestedGlobalVersion (interfaceName interface) requestedMaximum)
  | otherwise = traverse select matching
 where
  matching =
    [ announcedGlobalName announcement
    | announcement <- announcedGlobals catalog
    , announcedGlobalInterface announcement == interfaceName interface
    ]
  select name = selectGlobal interface requestedMaximum name catalog

-- | Bind a typed global without installing an event route.
bindGlobal ::
  Connection ->
  Object Wayland.WlRegistry ->
  Global interface ->
  IO (Either ConnectionError (Object interface))
bindGlobal connection registry global =
  submitNewObjectRequest
    connection
    Wayland.wlRegistryInterface
    registry
    (globalInterface global)
    (globalVersion global)
    (makeBindRequest global)

-- | Bind a typed global and publish its event route atomically.
bindGlobalWithRoute ::
  Connection ->
  Object Wayland.WlRegistry ->
  Global interface ->
  EventRoute interface ->
  IO (Either ConnectionError (Object interface))
bindGlobalWithRoute connection registry global =
  submitNewObjectRequestWithRoute
    connection
    Wayland.wlRegistryInterface
    registry
    (globalInterface global)
    (globalVersion global)
    (makeBindRequest global)

makeBindRequest :: Global interface -> Object interface -> RequestEncoder fd
makeBindRequest global object =
  Wayland.encodeWlRegistryBind
    (globalNameToWord (globalName global))
    DynamicNewId
      { dynamicNewIdInterface = interfaceName (globalInterface global)
      , dynamicNewIdVersion = globalVersion global
      , dynamicNewIdObject = objectId object
      }
