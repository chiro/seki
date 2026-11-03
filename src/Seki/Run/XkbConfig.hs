module Seki.Run.XkbConfig (
  XkbFacet (..),
  XkbState (..),
  emptyXkbFacet,
  xkbKey,
  stageXkb,
  commitXkb,
  xkbValues,
  xkbRequest,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Seki.Internal.Input
import Seki.Protocol.RiverXkbConfigV1 qualified as X
import Seki.Run.Libinput (FacetGeneration, FacetLifecycle (..))
import Seki.Wire.Binding (RequestEncoder)
import Seki.Wire.Connection.ObjectTable (Object)

data XkbState = XkbState
  { xkbLayout :: Maybe (Word32, Maybe Text)
  , xkbCapsLock :: Maybe Bool
  , xkbNumLock :: Maybe Bool
  , xkbScrollLock :: Maybe Bool
  }
  deriving (Eq, Show)
data XkbFacet = XkbFacet
  { xkbObject :: Maybe (Object X.RiverXkbKeyboardV1)
  , xkbGeneration :: Maybe FacetGeneration
  , xkbLifecycle :: FacetLifecycle
  , xkbStaged :: XkbState
  , xkbDiagnostics :: [Text]
  , xkbCurrent :: XkbState
  }
  deriving (Eq, Show)

emptyXkbFacet :: FacetLifecycle -> XkbFacet
emptyXkbFacet lifecycle = XkbFacet Nothing Nothing lifecycle empty [] empty
 where
  empty = XkbState Nothing Nothing Nothing Nothing

xkbKey :: SettingKey -> Bool
xkbKey key = key `elem` [KeymapKey, ActiveLayoutKey, CapsLockKey, NumLockKey, ScrollLockKey]

stageXkb :: X.RiverXkbKeyboardV1Event fd -> XkbFacet -> XkbFacet
stageXkb event facet =
  facet
    { xkbStaged = case event of
        X.RiverXkbKeyboardV1Layout index name -> s{xkbLayout = Just (index, name)}
        X.RiverXkbKeyboardV1CapslockEnabled -> s{xkbCapsLock = Just True}
        X.RiverXkbKeyboardV1CapslockDisabled -> s{xkbCapsLock = Just False}
        X.RiverXkbKeyboardV1NumlockEnabled -> s{xkbNumLock = Just True}
        X.RiverXkbKeyboardV1NumlockDisabled -> s{xkbNumLock = Just False}
        X.RiverXkbKeyboardV1ScrolllockEnabled -> s{xkbScrollLock = Just True}
        X.RiverXkbKeyboardV1ScrolllockDisabled -> s{xkbScrollLock = Just False}
        _ -> s
    }
 where
  s = xkbStaged facet

commitXkb :: XkbFacet -> XkbFacet
commitXkb facet = facet{xkbLifecycle = FacetReady, xkbCurrent = xkbStaged facet}

xkbValues :: XkbState -> Map SettingKey InputSetting
xkbValues s =
  Map.fromList $
    maybe [] (\(index, _) -> if index <= 2147483647 then [(ActiveLayoutKey, ActiveLayout (LayoutByIndex (fromIntegral index)))] else []) (xkbLayout s)
      <> maybe [] (\v -> [(CapsLockKey, CapsLock v)]) (xkbCapsLock s)
      <> maybe [] (\v -> [(NumLockKey, NumLock v)]) (xkbNumLock s)
      <> maybe [] (\v -> [(ScrollLockKey, ScrollLock v)]) (xkbScrollLock s)

xkbRequest :: InputSetting -> Maybe (RequestEncoder fd)
xkbRequest setting = case setting of
  ActiveLayout (LayoutByIndex index) -> Just (X.encodeRiverXkbKeyboardV1SetLayoutByIndex index)
  ActiveLayout (LayoutByName name) -> Just (X.encodeRiverXkbKeyboardV1SetLayoutByName name)
  CapsLock True -> Just X.encodeRiverXkbKeyboardV1CapslockEnable
  CapsLock False -> Just X.encodeRiverXkbKeyboardV1CapslockDisable
  NumLock True -> Just X.encodeRiverXkbKeyboardV1NumlockEnable
  NumLock False -> Just X.encodeRiverXkbKeyboardV1NumlockDisable
  ScrollLock True -> Just X.encodeRiverXkbKeyboardV1ScrolllockEnable
  ScrollLock False -> Just X.encodeRiverXkbKeyboardV1ScrolllockDisable
  _ -> Nothing
