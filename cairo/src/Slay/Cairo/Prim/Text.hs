module Slay.Cairo.Prim.Text
  ( FontWeight(..),
    Font(..),
    PrimText(..),
    text
  ) where

import Data.Fixed
import Data.Text
import Numeric.Natural

import Slay.Core
import Slay.Cairo.Prim.Color

data FontWeight =
  FontWeightBold | FontWeightNormal
  deriving (Eq, Ord, Show)

data Font =
  Font
    { fontFamily :: Text,
      fontSize :: Centi,
      fontWeight :: FontWeight
    } deriving (Eq, Ord, Show)

data PrimText g =
  PrimText
    { textFont :: Font,
      textColor :: g Color,
      textContent :: Text,
      textCursor :: g (Maybe Natural)
    }

deriving instance (Eq (g (Maybe Natural)), Eq (g (Color))) => Eq (PrimText g)
deriving instance (Ord (g (Maybe Natural)), Ord (g (Color))) => Ord (PrimText g)
deriving instance (Show (g (Maybe Natural)), Show (g (Color))) => Show (PrimText g)

text :: Inj (PrimText g) a => Font -> g Color -> Text -> g (Maybe Natural) -> a
text font color content cursor = inj (PrimText font color content cursor)
