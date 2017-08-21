module Slay.Prim
  ( Color(..)
  , FontWeight(..)
  , Font(..)
  , Curvature(..)
  , Direction(..)
  , PrimRect(..)
  , PrimText(..)
  , PrimCurve(..)
  , PrimCircle(..)
  , LRTB(..)
  , Inj(..)
  , substrate
  , rgb
  , rect
  , text
  , curve
  , circle
  ) where

import Data.Word
import Data.Fixed
import Data.Text
import Numeric.Natural
import Slay.Core

data Color =
  -- true color (24bit)
  RGB Word8 Word8 Word8
  deriving (Eq, Ord, Show)

data FontWeight =
  FontWeightBold | FontWeightNormal
  deriving (Eq, Ord, Show)

data Font g =
  Font
    { fontFamily :: Text,
      fontSize :: Centi,
      fontColor :: g Color,
      fontWeight :: FontWeight
    }

deriving instance Eq (g Color) => Eq (Font g)
deriving instance Ord (g Color) => Ord (Font g)
deriving instance Show (g Color) => Show (Font g)

data PrimRect g =
  PrimRect
    { rectExtents :: Extents,
      rectBackground :: g (Maybe Color)
    }

deriving instance Eq (g (Maybe Color)) => Eq (PrimRect g)
deriving instance Ord (g (Maybe Color)) => Ord (PrimRect g)
deriving instance Show (g (Maybe Color)) => Show (PrimRect g)

data PrimText g =
  PrimText
    { textFont :: Font g,
      textContent :: Text,
      textCursor :: g (Maybe Natural)
    }

deriving instance (Eq (g (Maybe Natural)), Eq (g (Color))) => Eq (PrimText g)
deriving instance (Ord (g (Maybe Natural)), Ord (g (Color))) => Ord (PrimText g)
deriving instance (Show (g (Maybe Natural)), Show (g (Color))) => Show (PrimText g)

-- from -1 to 1
newtype Curvature = Curvature Rational

data Direction =
  Direction
    { directionLeftToRight :: Bool,
      directionTopToBottom :: Bool
    }


data PrimCurve g =
  PrimCurve
    { curveExtents :: Extents,
      curveCurvature :: g Curvature,
      curveColor :: g Color,
      curveDirection :: g Direction
    }


data PrimCircle g = Circle
    { circleColor :: g Color
    , circleRadius :: Double
    }


class Inj p a where
  inj :: p -> a

instance Inj a a where
  inj = id

instance Inj p a => Inj p (Maybe a) where
  inj = Just . inj

instance (View s e a, Inj p e) => Inj p (Collage s a) where
  inj = collageSingleton . inj


rgb :: Inj Color a => Word8 -> Word8 -> Word8 -> a
rgb r g b = inj (RGB r g b)

rect :: Inj (PrimRect g) a => g (Maybe Color) -> Extents -> a
rect background extents = inj (PrimRect extents background)

text :: Inj (PrimText g) a => Font g -> Text -> g (Maybe Natural) -> a
text font content cursor = inj (PrimText font content cursor)

curve :: Inj (PrimCurve g) a => g Curvature -> g Color -> g Direction -> Extents -> a
curve curvature color direction extents = inj (PrimCurve extents curvature color direction)

circle :: Inj (PrimCircle g) a => g Color -> Double ->  a
circle color radius  = inj (Circle color radius)

data LRTB a = LRTB
  { left :: a,
    right :: a,
    top :: a,
    bottom :: a
  } deriving (Eq, Ord, Show)

substrate ::
  View s e a =>
  LRTB Natural ->
  (Extents -> e) ->
  Collage s a ->
  Collage s a
substrate pad mkObject collage =
  collageCompose
    Offset
      { offsetX = toInteger $ left pad,
        offsetY = toInteger $ top pad }
    (collageSingleton $ mkObject extents)
    collage
  where
    e = collageExtents collage
    extents = Extents
      { extentsW = left pad + extentsW e + right pad,
        extentsH = top pad + extentsH e + bottom pad }
