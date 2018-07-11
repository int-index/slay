{-# LANGUAGE TypeOperators #-}

module Slay.Combinators
  ( substrate,
    vertLeft,
    vertRight,
    horizTop,
    horizBottom,
    insideBox,
    integralDistribExcess
  ) where

import Numeric.Natural
import Control.Applicative
import Slay.Core

substrate ::
  s -/ e =>
  LRTB Natural ->
  (Extents -> e) ->
  Collage s ->
  Collage s
substrate pad mkObject collage =
  collageCompose
    Offset
      { offsetX = toInteger $ left pad,
        offsetY = toInteger $ top pad }
    (collageSingleton $ mkObject extents)
    collage
  where
    e = collageExtents collage
    extents = Extents
      { extentsW = left pad + extentsW e + right pad,
        extentsH = top pad + extentsH e + bottom pad }

vertLeft ::
  s -/ e =>
  Collage s ->
  Collage s ->
  Collage s
vertLeft c1 c2 = collageCompose offset c1 c2
  where
    e1 = collageExtents c1
    offset = Offset
      { offsetX = 0,
        offsetY = toInteger (extentsH e1) }

vertRight ::
  s -/ e =>
  Collage s ->
  Collage s ->
  Collage s
vertRight c1 c2 = collageCompose offset c1 c2
  where
    e1 = collageExtents c1
    e2 = collageExtents c2
    offset = Offset
      { offsetX = toInteger (extentsW e1) - toInteger (extentsW e2),
        offsetY = toInteger (extentsH e1) }

horizTop ::
  s -/ e =>
  Collage s ->
  Collage s ->
  Collage s
horizTop c1 c2 = collageCompose offset c1 c2
  where
    e1 = collageExtents c1
    offset = Offset
      { offsetX = toInteger (extentsW e1),
        offsetY = 0 }

horizBottom ::
  s -/ e =>
  Collage s ->
  Collage s ->
  Collage s
horizBottom c1 c2 = collageCompose offset c1 c2
  where
    e1 = collageExtents c1
    e2 = collageExtents c2
    offset = Offset
      { offsetX = toInteger (extentsW e1),
        offsetY = toInteger (extentsH e1) - toInteger (extentsH e2) }

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
