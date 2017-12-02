module Slay.Cairo.Prim
  ( module Slay.Cairo.Prim.Color,
    module Slay.Cairo.Prim.Text,
    module Slay.Cairo.Prim.Rect,
    module Slay.Cairo.Prim.Curve,
    module Slay.Cairo.Prim.Circle,
    module Slay.Cairo.Prim.PangoText,

    -- * Layouting combinators
    LRTB(..),
    substrate
  ) where

import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Text
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Curve
import Slay.Cairo.Prim.Circle
import Slay.Cairo.Prim.PangoText

import Slay.Number
import Slay.Core

data LRTB a = LRTB
  { left :: a,
    right :: a,
    top :: a,
    bottom :: a
  } deriving (Eq, Ord, Show)

substrate ::
  HasView s e a =>
  LRTB Unsigned ->
  (Extents -> e) ->
  Collage s a ->
  Collage s a
substrate pad mkObject collage =
  collageCompose
    Offset
      { offsetX = toSigned $ left pad,
        offsetY = toSigned $ top pad }
    (collageSingleton $ mkObject extents)
    collage
  where
    e = collageExtents collage
    extents = Extents
      { extentsW = left pad + extentsW e + right pad,
        extentsH = top pad + extentsH e + bottom pad }
