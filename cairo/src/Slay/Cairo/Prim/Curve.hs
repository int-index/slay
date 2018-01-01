module Slay.Cairo.Prim.Curve
  ( Curvature(..),
    Direction(..),
    PrimCurve(..),
    PrimArrowhead(..),
    curve,
    arrowhead,
    renderElementCurve
  ) where

import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Core

import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

-- from -1 to 1
newtype Curvature = Curvature Rational

curvatureCoeff :: Curvature -> Signed
curvatureCoeff (Curvature r) = fromRational r

data Direction =
  Direction
    { directionLeftToRight :: Bool,
      directionTopToBottom :: Bool
    }

data PrimArrowhead g =
  PrimArrowhead
    { arrowheadWidth :: g Unsigned,
      arrowheadLength :: g Unsigned,
      arrowheadDepth :: g Unsigned
    }

data PrimCurve g =
  PrimCurve
    { curveExtents :: Extents,
      curveDebug :: Maybe Color,
      curveCurvature :: g Curvature,
      curveColor :: g Color,
      curveDirection :: g Direction,
      curveWidth :: g Unsigned,
      curveArrowhead :: Maybe (PrimArrowhead g)
    }

arrowhead ::
  Inj (PrimArrowhead g) a =>
  g Unsigned ->
  g Unsigned ->
  g Unsigned ->
  a
arrowhead width len depth =
  inj (PrimArrowhead width len depth)

curve ::
  Inj (PrimCurve g) a =>
  Maybe Color ->
  g Curvature ->
  g Color ->
  g Direction ->
  g Unsigned ->
  Maybe (PrimArrowhead g) ->
  Extents ->
  a
curve debug curvature color direction width arrowhd extents =
  inj (PrimCurve extents debug curvature color direction width arrowhd)

angleFromPoints :: Offset -> Offset -> (Signed, Signed)
angleFromPoints (Offset a b) (Offset c d) = (sine, cosine)
  where
    x = c - a
    y = d - b
    r = sqrt (x * x + y * y)
    sine = y / r
    cosine = x / r

renderElementCurve :: PrimCurve g -> (forall x. g x -> x) -> Offset -> Cairo.Render ()
renderElementCurve (PrimCurve extents mdebug gcurvature gcolor gdirection gwidth marrowhead) getG offset = do
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
    (renderArrowhead, kp2', kp3') = case marrowhead of
      Nothing -> (return (), kp2', kp3)
      Just (PrimArrowhead garrWidth garrLen garrDepth) ->
        let
          arrLen = toSigned (getG garrLen)
          arrWidthHalf = toSigned (getG garrWidth) / 2
          arrDepth = toSigned (getG garrDepth)

          kp2'' = rot kp2 (Offset 0 arrLen)
          kp3'' = rot kp3 (Offset 0 arrLen)

          (sine, cosine) = angleFromPoints kp2 kp3
          rot kp (Offset x y) = offsetAdd kp (Offset x' y')
            where
              x' = negate (x * sine + y * cosine)
              y' = x * cosine - y * sine

          rot' cto x y =
            let o = rot kp3 (Offset x y)
            in cto (offsetX o) (offsetY o)

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
  Cairo.moveTo (offsetX kp0) (offsetY kp0)
  Cairo.curveTo
    (offsetX kp1) (offsetY kp1)
    (offsetX kp2') (offsetY kp2')
    (offsetX kp3') (offsetY kp3')
  Cairo.setLineWidth $ toSigned width
  Cairo.stroke
  renderArrowhead
  for_ mdebug $ \color -> do
    setSourceColor color
    for_ [kp0, kp1, kp2', kp3'] $ \(Offset x y) -> do
      Cairo.arc x y (toSigned width) 0 (2 * pi)
      Cairo.fill
    Cairo.moveTo (offsetX kp0) (offsetY kp0)
    for_ [kp1, kp2', kp3'] $ \(Offset x y) -> Cairo.lineTo x y
    Cairo.setLineWidth (toSigned width)
    Cairo.stroke

bend :: Curvature -> Double -> Double -> Double
bend c a b = c' * (b - a) + a
  where
    c' = (curvatureCoeff c + 1) / 2

bendPoint :: Curvature -> Offset -> Offset -> Offset
bendPoint c (Offset ax ay) (Offset bx by) =
  Offset (bend c ax bx) (bend c ay by)

instance RenderElement g (PrimCurve g) where
  renderElement = renderElementCurve
