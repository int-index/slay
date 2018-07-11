module Slay.Cairo.Prim.Circle
  ( PrimCircle(..),
    circle,
    circleExtents,
    renderElementCircle
  ) where

import Numeric.NonNegative
import Numeric.Natural
import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PrimCircle g =
  PrimCircle
    { circleColor :: g Color,
      circleThickness :: g (Maybe (NonNegative Double)),
      circleDiameter :: Natural
    }

instance p ~ PrimCircle g => Inj p (PrimCircle g)

circle :: Inj (PrimCircle g) a => g Color -> g (Maybe (NonNegative Double)) -> Natural -> a
circle color mthickness diameter = inj (PrimCircle color mthickness diameter)

circleExtents :: PrimCircle g -> Extents
circleExtents c = Extents d d where d = circleDiameter c

renderElementCircle ::
  (forall x. g x -> x) ->
  (Offset, Extents, PrimCircle g) ->
  Cairo.Render ()
renderElementCircle getG (offset, _, PrimCircle gcolor gmthickness diameter) = do
  let (Offset (fromIntegral -> x) (fromIntegral -> y)) = offset
  setSourceColor $ getG gcolor
  let r = fromIntegral diameter / 2
  for_ (getG gmthickness) $ \thck -> do
    Cairo.setFillRule Cairo.FillRuleEvenOdd
    Cairo.arc (r + x) (r + y) (r - getNonNegative thck) 0 (2 * pi)
  Cairo.arc (r + x) (r + y) r 0 (2 * pi)
  Cairo.fill

instance RenderElement g (PrimCircle g) where
  renderElement = renderElementCircle
