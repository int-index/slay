module Slay.Cairo.Prim.Curve
  ( Curvature(..),
    Direction(..),
    PrimCurve(..),
    PrimArrowhead(..),
    curve,
    arrowhead,
    renderElementCurve
  ) where

import Numeric.NonNegative
import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

-- from -1 to 1
newtype Curvature = Curvature Rational

instance p ~ Curvature => Inj p Curvature

curvatureCoeff :: Curvature -> Double
curvatureCoeff (Curvature r) = fromRational r

data Direction =
  Direction
    { directionLeftToRight :: Bool,
      directionTopToBottom :: Bool
    }

instance p ~ Direction => Inj p Direction

data PrimArrowhead g =
  PrimArrowhead
    { arrowheadWidth :: g (NonNegative Double),
      arrowheadLength :: g (NonNegative Double),
      arrowheadDepth :: g (NonNegative Double)
    }

instance p ~ PrimArrowhead g => Inj p (PrimArrowhead g)

data PrimCurve g =
  PrimCurve
    { curveExtents :: Extents,
      curveDebug :: Maybe Color,
      curveCurvature :: g Curvature,
      curveColor :: g Color,
      curveDirection :: g Direction,
      curveWidth :: g (NonNegative Double),
      curveArrowhead :: Maybe (PrimArrowhead g)
    }

instance p ~ PrimCurve g => Inj p (PrimCurve g)

arrowhead ::
  Inj (PrimArrowhead g) a =>
  g (NonNegative Double) ->
  g (NonNegative Double) ->
  g (NonNegative Double) ->
  a
arrowhead width len depth =
  inj (PrimArrowhead width len depth)

curve ::
  Inj (PrimCurve g) a =>
  Maybe Color ->
  g Curvature ->
  g Color ->
  g Direction ->
  g (NonNegative Double) ->
  Maybe (PrimArrowhead g) ->
  Extents ->
  a
curve debug curvature color direction width arrowhd extents =
  inj (PrimCurve extents debug curvature color direction width arrowhd)

angleFromPoints :: (Double, Double) -> (Double, Double) -> (Double, Double)
angleFromPoints (a, b) (c, d) = (sine, cosine)
  where
    x = c - a
    y = d - b
    r = sqrt (x * x + y * y)
    sine = y / r
    cosine = x / r

liftPair :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftPair fn (a1, a2) (b1, b2) = (fn a1 b1, fn a2 b2)

renderElementCurve ::
  (forall x. g x -> x) ->
  (Offset, Extents, PrimCurve g) ->
  Cairo.Render ()
renderElementCurve getG (offset, extents, PrimCurve _ mdebug gcurvature gcolor gdirection gwidth marrowhead) = do
  let
    o = (fromIntegral $ offsetX offset, fromIntegral $ offsetY offset)
    width = getG gwidth
    curvature = getG gcurvature
    direction = getG gdirection
    Extents (fromIntegral -> w) (fromIntegral -> h) = extents
    (x1, x2) = if directionLeftToRight direction then (0, w) else (w, 0)
    (y1, y2) = if directionTopToBottom direction then (0, h) else (h, 0)
    p0 = (x1, y1)
    p1 = bendPoint curvature (w/2, y1) (x1, h/2)
    p2 = bendPoint curvature (w/2, y2) (x2, h/2)
    p3 = (x2, y2)
    (kp0, kp1, kp2, kp3) =
      ( liftPair (+) o p0,
        liftPair (+) o p1,
        liftPair (+) o p2,
        liftPair (+) o p3 )
    (renderArrowhead, kp2', kp3') = case marrowhead of
      Nothing -> (return (), kp2', kp3)
      Just (PrimArrowhead garrWidth garrLen garrDepth) ->
        let
          arrLen = getNonNegative (getG garrLen)
          arrWidthHalf = getNonNegative (getG garrWidth) / 2
          arrDepth = getNonNegative (getG garrDepth)

          kp2'' = rot kp2 (0, arrLen)
          kp3'' = rot kp3 (0, arrLen)

          (sine, cosine) = angleFromPoints kp2 kp3
          rot kp (x, y) = liftPair (+) kp (x', y')
            where
              x' = negate (x * sine + y * cosine)
              y' = x * cosine - y * sine

          rot' cto x y =
            let (x', y') = rot kp3 (x, y)
            in cto x' y'

          renderArr = do
            Cairo.newPath
            rot' Cairo.moveTo 0 0
            rot' Cairo.lineTo (negate arrWidthHalf) (arrDepth + arrLen)
            rot' Cairo.lineTo 0 arrLen
            rot' Cairo.lineTo arrWidthHalf (arrDepth + arrLen)
            Cairo.closePath
            Cairo.fill
        in
          (renderArr, kp2'', kp3'')
  setSourceColor (getG gcolor)
  Cairo.moveTo (fst kp0) (snd kp0)
  Cairo.curveTo
    (fst kp1) (snd kp1)
    (fst kp2') (snd kp2')
    (fst kp3') (snd kp3')
  Cairo.setLineWidth $ getNonNegative width
  Cairo.stroke
  renderArrowhead
  for_ mdebug $ \color -> do
    setSourceColor color
    for_ [kp0, kp1, kp2', kp3'] $ \(x, y) -> do
      Cairo.arc x y (getNonNegative width) 0 (2 * pi)
      Cairo.fill
    Cairo.moveTo (fst kp0) (snd kp0)
    for_ [kp1, kp2', kp3'] $ \(x, y) -> Cairo.lineTo x y
    Cairo.setLineWidth (getNonNegative width)
    Cairo.stroke

bend :: Curvature -> Double -> Double -> Double
bend c a b = c' * (b - a) + a
  where
    c' = (curvatureCoeff c + 1) / 2

bendPoint :: Curvature -> (Double, Double) -> (Double, Double) -> (Double, Double)
bendPoint c (ax, ay) (bx, by) =
  (bend c ax bx, bend c ay by)

instance RenderElement g (PrimCurve g) where
  renderElement = renderElementCurve
