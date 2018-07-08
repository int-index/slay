module Slay.Cairo.Prim.Rect
  ( PrimRect(..),
    rect,
    renderElementRect
  ) where

import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PrimRect g =
  PrimRect
    { rectExtents :: Extents,
      rectThickness :: g (Maybe (LRTB Unsigned)),
      rectColor :: g (Maybe Color)
    }

deriving instance (Eq (g (Maybe Color)), Eq (g (Maybe (LRTB Unsigned)))) => Eq (PrimRect g)
deriving instance (Ord (g (Maybe Color)), Ord (g (Maybe (LRTB Unsigned)))) => Ord (PrimRect g)
deriving instance (Show (g (Maybe Color)), Show (g (Maybe (LRTB Unsigned)))) => Show (PrimRect g)

instance p ~ PrimRect g => Inj p (PrimRect g)

rect :: Inj (PrimRect g) a => g (Maybe (LRTB Unsigned)) -> g (Maybe Color) -> Extents -> a
rect thickness color extents = inj (PrimRect extents thickness color)

renderElementRect :: (forall x. g x -> x) -> (Offset, Extents, PrimRect g) -> Cairo.Render ()
renderElementRect getG (Offset x y, Extents w h, PrimRect _ gmthickness gmcolor) = do
  for_ (getG gmcolor) $ \color -> do
    setSourceColor color
    for_ (getG gmthickness) $ \thck -> do
      Cairo.setFillRule Cairo.FillRuleEvenOdd
      Cairo.rectangle
        (x + toSigned (left thck))
        (y + toSigned (top thck))
        (toSigned w - toSigned (left thck + right thck))
        (toSigned h - toSigned (top thck + bottom thck))
    Cairo.rectangle x y (toSigned w) (toSigned h)
    Cairo.fill

instance RenderElement g (PrimRect g) where
  renderElement = renderElementRect
