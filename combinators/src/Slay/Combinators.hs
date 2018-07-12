{-# LANGUAGE TypeOperators, NamedFieldPuns #-}

module Slay.Combinators
  ( substrate,
    horiz,
    horizTop,
    horizBottom,
    horizCenter,
    vert,
    vertLeft,
    vertRight,
    vertCenter,
    insideBox,
    integralDistribExcess
  ) where

import Numeric.Natural
import Control.Applicative
import Slay.Core

substrate ::
  LRTB Natural ->
  (Extents -> Collage s) ->
  Collage s ->
  Collage s
substrate pad mkSub collage =
  collageCompose
    Offset
      { offsetX = toInteger $ left pad,
        offsetY = toInteger $ top pad }
    (mkSub extents)
    collage
  where
    e = collageExtents collage
    extents = Extents
      { extentsW = left pad + extentsW e + right pad,
        extentsH = top pad + extentsH e + bottom pad }

horiz, vert ::
  (Natural -> Natural -> Integer) ->
  Collage s ->
  Collage s ->
  Collage s
horiz align c1 c2 = collageCompose offset c1 c2
  where
    e1 = collageExtents c1
    e2 = collageExtents c2
    offsetX = toInteger (extentsW e1)
    offsetY = align (extentsH e1) (extentsH e2)
    offset = Offset{offsetX, offsetY}
vert align c1 c2 = collageCompose offset c1 c2
  where
    e1 = collageExtents c1
    e2 = collageExtents c2
    offsetX = align (extentsW e1) (extentsW e2)
    offsetY = toInteger (extentsH e1)
    offset = Offset{offsetX, offsetY}

horizTop, horizBottom, horizCenter ::
  Collage s -> Collage s -> Collage s
horizTop = horiz (\_ _ -> 0)
horizBottom = horiz (\h1 h2 -> toInteger h1 - toInteger h2)
horizCenter = horiz (\h1 h2 -> (toInteger h1 - toInteger h2) `div` 2)

vertLeft, vertRight, vertCenter ::
  Collage s -> Collage s -> Collage s
vertLeft = vert (\_ _ -> 0)
vertRight = vert (\w1 w2 -> toInteger w1 - toInteger w2)
vertCenter = vert (\w1 w2 -> (toInteger w1 - toInteger w2) `div` 2)

insideBox :: (Offset, Extents) -> Offset -> Bool
insideBox (Offset ax ay, Extents w h) (Offset x y) =
  inRange (ax, bx) x &&
  inRange (ay, by) y
  where
    bx = ax + toInteger w
    by = ay + toInteger h
    inRange (lower, upper) =
      liftA2 (&&) (lower<=) (upper>=)

integralDistribExcess :: Integral n => n -> n -> (n, n)
integralDistribExcess desired actual = (l, r)
  where
    excess =
      if desired > actual
      then desired - actual
      else 0
    l = excess `quot` 2
    r = excess - l
