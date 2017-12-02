module Slay.Cairo.Prim.Circle
  ( PrimCircle(..),
    circle,
    circleExtents,
    renderElementCircle
  ) where

import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Number
import Slay.Core

import Slay.Cairo.Inj
import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PrimCircle g =
  PrimCircle
    { circleColor :: g Color,
      circleRadius :: Unsigned
    }

circle :: Inj (PrimCircle g) a => g Color -> Unsigned -> a
circle color radius = inj (PrimCircle color radius)

circleExtents :: PrimCircle g -> Extents
circleExtents c = Extents (2 * r) (2 * r) where r = circleRadius c

renderElementCircle :: PrimCircle g -> (forall x. g x -> x) -> Offset -> Cairo.Render ()
renderElementCircle (PrimCircle gcolor radius) getG (Offset x y) = do
  setSourceColor $ getG gcolor
  let r = toSigned radius
  Cairo.arc (r + x) (r + y) r 0 (2 * pi)
  Cairo.fill

instance RenderElement g (PrimCircle g) where
  renderElement = renderElementCircle
