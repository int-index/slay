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

-- | curve direction
data Direction =
  Direction
    { directionLeftToRight :: Bool,
      directionTopToBottom :: Bool
    }

-- | adt for curves
data PrimCurve g =
  PrimCurve
    { curveExtents :: Extents,
      curveCurvature :: g Curvature,
      curveColor :: g Color,
      curveDirection :: g Direction
    }

-- | Circle property types
newtype CircleCenterX = CircleCenterX Double
newtype CircleCenterY = CircleCenterY Double
newtype CircleRadius = CircleRadius Double
newtype CircleStartAngle = CircleStartAngle Double
newtype CircleEndAngle = CircleEndAngle Double

-- | circle adt
data PrimCircle g =
    PrimCircle
    {
      circleCenterX :: g CircleCenterX
    , circleCenterY :: g CircleCenterY
    , circleRadius :: g CircleRadius
    , circleStartAngle :: g CircleStartAngle
    , circleEndAngle :: g CircleEndAngle
    }



-- | Inj typeclass
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

-- | draw a circle
makeCircle :: Inj (PrimCircle g) a => g CircleCenterX -> g CircleCenterY -> g CircleRadius -> g CircleStartAngle -> g CircleEndAngle -> a
makeCircle cx cy r sa ea = inj (PrimCircle cx cy r sa ea)


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
