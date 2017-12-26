module Slay.Cairo.Prim.Rect
  ( PrimRect(..),
    rect,
    renderElementRect
  ) where

import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PrimRect g =
  PrimRect
    { rectExtents :: Extents,
      rectBackground :: g (Maybe Color)
    }

deriving instance Eq (g (Maybe Color)) => Eq (PrimRect g)
deriving instance Ord (g (Maybe Color)) => Ord (PrimRect g)
deriving instance Show (g (Maybe Color)) => Show (PrimRect g)

rect :: Inj (PrimRect g) a => g (Maybe Color) -> Extents -> a
rect background extents = inj (PrimRect extents background)

renderElementRect :: PrimRect g -> (forall x. g x -> x) -> Offset -> Cairo.Render ()
renderElementRect (PrimRect (Extents w h) gmbc) getG (Offset x y) = do
  for_ (getG gmbc) $ \bc -> do
    setSourceColor bc
    Cairo.rectangle x y (toSigned w) (toSigned h)
    Cairo.fill

instance RenderElement g (PrimRect g) where
  renderElement = renderElementRect
