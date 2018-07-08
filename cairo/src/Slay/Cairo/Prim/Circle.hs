module Slay.Cairo.Prim.Circle
  ( PrimCircle(..),
    circle,
    circleExtents,
    renderElementCircle
  ) where

import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Number
import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PrimCircle g =
  PrimCircle
    { circleColor :: g Color,
      circleThickness :: g (Maybe Unsigned),
      circleRadius :: Unsigned
    }

instance p ~ PrimCircle g => Inj p (PrimCircle g)

circle :: Inj (PrimCircle g) a => g Color -> g (Maybe Unsigned) -> Unsigned -> a
circle color mthickness radius = inj (PrimCircle color mthickness radius)

circleExtents :: PrimCircle g -> Extents
circleExtents c = Extents (2 * r) (2 * r) where r = circleRadius c

renderElementCircle ::
  (forall x. g x -> x) ->
  (Offset, Extents, PrimCircle g) ->
  Cairo.Render ()
renderElementCircle getG (Offset x y, _, PrimCircle gcolor gmthickness radius) = do
  setSourceColor $ getG gcolor
  let r = toSigned radius
  for_ (getG gmthickness) $ \thck -> do
    Cairo.setFillRule Cairo.FillRuleEvenOdd
    Cairo.arc (r + x) (r + y) (r - toSigned thck) 0 (2 * pi)
  Cairo.arc (r + x) (r + y) r 0 (2 * pi)
  Cairo.fill

instance RenderElement g (PrimCircle g) where
  renderElement = renderElementCircle
