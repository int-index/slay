module Slay.Cairo.Prim.Rect
  ( PrimRect(..),
    rect,
    renderElementRect
  ) where

import Numeric.NonNegative
import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PrimRect g =
  PrimRect
    { rectExtents :: Extents,
      rectThickness :: g (Maybe (LRTB (NonNegative Double))),
      rectColor :: g (Maybe Color)
    }

deriving instance (Eq (g (Maybe Color)), Eq (g (Maybe (LRTB (NonNegative Double))))) => Eq (PrimRect g)
deriving instance (Ord (g (Maybe Color)), Ord (g (Maybe (LRTB (NonNegative Double))))) => Ord (PrimRect g)
deriving instance (Show (g (Maybe Color)), Show (g (Maybe (LRTB (NonNegative Double))))) => Show (PrimRect g)

instance p ~ PrimRect g => Inj p (PrimRect g)

rect :: Inj (PrimRect g) a => g (Maybe (LRTB (NonNegative Double))) -> g (Maybe Color) -> Extents -> a
rect thickness color extents = inj (PrimRect extents thickness color)

renderElementRect :: (forall x. g x -> x) -> (Offset, Extents, PrimRect g) -> Cairo.Render ()
renderElementRect getG (Offset x y, Extents w h, PrimRect _ gmthickness gmcolor) = do
  for_ (getG gmcolor) $ \color -> do
    setSourceColor color
    for_ (getG gmthickness) $ \thck -> do
      Cairo.setFillRule Cairo.FillRuleEvenOdd
      Cairo.rectangle
        (fromIntegral x + getNonNegative (left thck))
        (fromIntegral y + getNonNegative (top thck))
        (fromIntegral w - getNonNegative (left thck + right thck))
        (fromIntegral h - getNonNegative (top thck + bottom thck))
    Cairo.rectangle
      (fromIntegral x)
      (fromIntegral y)
      (fromIntegral w)
      (fromIntegral h)
    Cairo.fill

instance RenderElement g (PrimRect g) where
  renderElement = renderElementRect
