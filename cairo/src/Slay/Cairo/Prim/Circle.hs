module Slay.Cairo.Prim.Circle (circle) where

import Numeric.NonNegative
import Numeric.Natural
import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Element

circle ::
  forall g a.
  Inj (CairoElement g) a =>
  g Color ->
  g (Maybe (NonNegative Double)) ->
  Natural ->
  a
circle gcolor gmthickness diameter =
  inj CairoElement
    { cairoElementExtents = extents,
      cairoElementBaseline = NoBaseline,
      cairoElementRender = render }
  where
    extents = Extents diameter diameter

    render :: Offset -> (forall x. g x -> x) -> Cairo.Render ()
    render offset getG = do
      let (Offset (fromIntegral -> x) (fromIntegral -> y)) = offset
      setSourceColor $ getG gcolor
      let r = fromIntegral diameter / 2
      for_ (getG gmthickness) $ \thck -> do
        Cairo.setFillRule Cairo.FillRuleEvenOdd
        Cairo.arc (r + x) (r + y) (r - getNonNegative thck) 0 (2 * pi)
      Cairo.arc (r + x) (r + y) r 0 (2 * pi)
      Cairo.fill
