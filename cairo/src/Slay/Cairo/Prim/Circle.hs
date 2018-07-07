module Slay.Cairo.Prim.Circle
  ( PrimCircle(..),
    circle,
    circleExtents,
    renderElementCircle
  ) where

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Number
import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PrimCircle g =
  PrimCircle
    { circleColor :: g Color,
      circleRadius :: Unsigned
    }

instance p ~ PrimCircle g => Inj p (PrimCircle g)

circle :: Inj (PrimCircle g) a => g Color -> Unsigned -> a
circle color radius = inj (PrimCircle color radius)

circleExtents :: PrimCircle g -> Extents
circleExtents c = Extents (2 * r) (2 * r) where r = circleRadius c

renderElementCircle ::
  (forall x. g x -> x) ->
  (Offset, Extents, PrimCircle g) ->
  Cairo.Render ()
renderElementCircle getG (Offset x y, _, PrimCircle gcolor radius) = do
  setSourceColor $ getG gcolor
  let r = toSigned radius
  Cairo.arc (r + x) (r + y) r 0 (2 * pi)
  Cairo.fill

instance RenderElement g (PrimCircle g) where
  renderElement = renderElementCircle
