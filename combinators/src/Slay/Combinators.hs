{-# LANGUAGE NamedFieldPuns, TypeApplications #-}

module Slay.Combinators
  ( substrate,
    decorateMargin,
    horiz,
    horizTop,
    horizBottom,
    horizCenter,
    horizBaseline,
    vert,
    vertLeft,
    vertRight,
    vertCenter,
    insideBox,
    integralDistribExcess
  ) where

import Numeric.Natural
import Control.Applicative
import Data.List.NonEmpty
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

decorateMargin ::
  Decoration (Extents -> Collage a) ->
  Collage a ->
  Collage a
decorateMargin d collage =
  collageDecorate decoration collage
  where
    e = collageExtents collage
    m = collageMargin collage
    extents = Extents
      { extentsW = marginLeft m + extentsW e + marginRight m,
        extentsH = marginTop m + extentsH e + marginBottom m }
    pos = Offset
      { offsetX = negate . toInteger $ marginLeft m,
        offsetY = negate . toInteger $ marginTop m }
    decoration = fmap @Decoration (\mkC -> At pos (mkC extents)) d

horiz, vert ::
  (Collage s -> Integer) ->
  Collage s ->
  Collage s ->
  Collage s
horiz align c1 c2 =
  positionedItem $
    collageComposeN (At offset1 c1 :| At offset2 c2 : [])
  where
    m1 = collageMargin c1
    m2 = collageMargin c2
    marginX = max (marginRight m1) (marginLeft m2)
    offsetX = toInteger (widthOf c1 + marginX)
    offset1 = Offset{offsetY=align c1, offsetX=0}
    offset2 = Offset{offsetY=align c2, offsetX}
vert align c1 c2 =
  positionedItem $
    collageComposeN (At offset1 c1 :| At offset2 c2 : [])
  where
    m1 = collageMargin c1
    m2 = collageMargin c2
    marginY = max (marginBottom m1) (marginTop m2)
    offsetY = toInteger (heightOf c1 + marginY)
    offset1 = Offset{offsetX=align c1, offsetY=0}
    offset2 = Offset{offsetX=align c2, offsetY}

horizTop, horizBottom, horizCenter, horizBaseline ::
  Collage s -> Collage s -> Collage s
horizTop = horiz (const 0)
horizBottom = horiz (negate . toInteger . heightOf)
horizCenter = horiz (negate . toInteger . (`quot` 2) . heightOf)
horizBaseline = horiz (negate . defaultBaseline 0 . collageBaseline)

defaultBaseline :: Integer -> Baseline -> Integer
defaultBaseline b NoBaseline = b
defaultBaseline _ (Baseline a) = toInteger a

vertLeft, vertRight, vertCenter ::
  Collage s -> Collage s -> Collage s
vertLeft = vert (const 0)
vertRight = vert (negate . toInteger . widthOf)
vertCenter = vert (negate . toInteger . (`quot` 2) . widthOf)

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
