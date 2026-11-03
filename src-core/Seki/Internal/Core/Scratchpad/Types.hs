module Seki.Internal.Core.Scratchpad.Types (
  Scratchpad (..),
  ScratchpadState (..),
  ScratchpadEffect (..),
  validateScratchpads,
) where

import Data.Char (isSpace)
import Seki.Internal.Core.Geometry (OutputRect (..), rectHeight, rectWidth)
import Seki.Internal.Core.ManageHook (Query)
import Seki.Internal.Core.StackSet (FloatGeom, WindowId)

-- | Definitions are ordered. The first matching query owns the new window.
data Scratchpad = Scratchpad
  { scratchpadName :: String
  , scratchpadCommand :: String
  , scratchpadQuery :: Query Bool
  , scratchpadGeometry :: OutputRect
  }

data ScratchpadState
  = LaunchPending Integer
  | HiddenPad WindowId FloatGeom
  | ShownPad WindowId (Maybe WindowId)
  deriving (Eq, Show)

data ScratchpadEffect
  = LaunchPad String Integer String
  | PadNotice String
  deriving (Eq, Show)

validateScratchpads :: [Scratchpad] -> Either String ()
validateScratchpads = go []
 where
  go _ [] = Right ()
  go seen (p : ps)
    | all isSpace name = Left "scratchpad name is blank"
    | name `elem` seen = Left ("duplicate scratchpad name: " <> name)
    | all isSpace (scratchpadCommand p) = Left ("scratchpad command is blank: " <> name)
    | OutputRect rect <- scratchpadGeometry p, rectWidth rect <= 0 || rectHeight rect <= 0 = Left ("scratchpad rectangle is empty: " <> name)
    | otherwise = go (name : seen) ps
   where
    name = scratchpadName p
