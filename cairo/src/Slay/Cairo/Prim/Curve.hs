module Slay.Cairo.Prim.Curve
  ( Curvature(..),
    Direction(..),
    PrimCurve(..),
    curve,
    renderElementCurve
  ) where

import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Inj
import Slay.Cairo.Render

-- from -1 to 1
newtype Curvature = Curvature Rational

data Direction =
  Direction
    { directionLeftToRight :: Bool,
      directionTopToBottom :: Bool
    }

data PrimCurve g =
  PrimCurve
    { curveExtents :: Extents,
      curveCurvature :: g Curvature,
      curveColor :: g Color,
      curveDirection :: g Direction,
      curveWidth :: g Unsigned,
      curveDebug :: Maybe Color
    }

curve ::
  Inj (PrimCurve g) a =>
  g Curvature ->
  g Color ->
  g Direction ->
  g Unsigned ->
  Maybe Color ->
  Extents ->
  a
curve curvature color direction width debug extents =
  inj (PrimCurve extents curvature color direction width debug)

renderElementCurve :: PrimCurve g -> (forall x. g x -> x) -> Offset -> Cairo.Render ()
renderElementCurve (PrimCurve extents gcurvature gcolor gdirection gwidth mdebug) getG offset = do
  let
    width = getG gwidth
    curvature = getG gcurvature
    direction = getG gdirection
    Extents (toSigned -> w) (toSigned -> h) = extents
    (x1, x2) = if directionLeftToRight direction then (0, w) else (w, 0)
    (y1, y2) = if directionTopToBottom direction then (0, h) else (h, 0)
    p0 = Offset x1 y1
    p1 = bendPoint curvature (Offset (w/2) y1) (Offset x1 (h/2))
    p2 = bendPoint curvature (Offset (w/2) y2) (Offset x2 (h/2))
    p3 = Offset x2 y2
    (kp0, kp1, kp2, kp3) =
      ( offsetAdd offset p0,
        offsetAdd offset p1,
        offsetAdd offset p2,
        offsetAdd offset p3 )
  setSourceColor (getG gcolor)
  Cairo.moveTo (offsetX kp0) (offsetY kp0)
  Cairo.curveTo
    (offsetX kp1) (offsetY kp1)
    (offsetX kp2) (offsetY kp2)
    (offsetX kp3) (offsetY kp3)
  Cairo.setLineWidth $ toSigned width
  Cairo.stroke
  for_ mdebug $ \color -> do
    setSourceColor color
    for_ [kp0, kp1, kp2, kp3] $ \(Offset x y) -> do
      Cairo.arc x y (toSigned width) 0 (2 * pi)
      Cairo.fill
    Cairo.moveTo (offsetX kp0) (offsetY kp0)
    for_ [kp1, kp2, kp3] $ \(Offset x y) -> Cairo.lineTo x y
    Cairo.setLineWidth (toSigned width)
    Cairo.stroke

bend :: Curvature -> Double -> Double -> Double
bend (Curvature c) a b = realToFrac c' * (b - a) + a
  where
    c' = (c + 1) / 2

bendPoint :: Curvature -> Offset -> Offset -> Offset
bendPoint c (Offset ax ay) (Offset bx by) =
  Offset (bend c ax bx) (bend c ay by)

instance RenderElement g (PrimCurve g) where
  renderElement = renderElementCurve
