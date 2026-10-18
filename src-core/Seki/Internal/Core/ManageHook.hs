{- | What a new window reported to river before the window manager first saw it.

The runtime reads these from its inventory at @manage_start@: river sends a
window's @app_id@, @title@, @parent@, and @dimensions_hint@ events before the
@manage_start@ that follows the window's creation.
-}
module Seki.Internal.Core.ManageHook (
  WindowProps (..),
  DimensionsHint (..),
  noWindowProps,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Seki.Internal.Core.StackSet (WindowId)

-- | A new window's properties, each absent until the client sets it.
data WindowProps = WindowProps
  { propAppId :: Maybe Text
  , propTitle :: Maybe Text
  , propParent :: Maybe WindowId
  , propDimensionsHint :: Maybe DimensionsHint
  }
  deriving (Eq, Show)

-- | River's @dimensions_hint@ in logical pixels, where 0 means no preference.
data DimensionsHint = DimensionsHint
  { hintMinWidth :: Int32
  , hintMinHeight :: Int32
  , hintMaxWidth :: Int32
  , hintMaxHeight :: Int32
  }
  deriving (Eq, Show)

noWindowProps :: WindowProps
noWindowProps = WindowProps Nothing Nothing Nothing Nothing
