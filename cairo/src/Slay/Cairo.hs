module Slay.Cairo
  ( RenderElement(..)
  , SomeRenderElement(..)
  , renderElements
  , PangoText(..)
  , primTextPango
  , setSourceColor
  ) where

import Numeric.Natural
import Data.Foldable (traverse_, for_)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word
import qualified Data.Text as Text

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import Slay.Core
import Slay.Prim

setSourceColor :: Color -> Cairo.Render ()
setSourceColor (RGB r g b) =
  Cairo.setSourceRGB (toDouble r) (toDouble g) (toDouble b)
  where
    toDouble x = fromIntegral x / fromIntegral (maxBound :: Word8)

renderElements ::
  Foldable f =>
  RenderElement g a =>
  (forall x. g x -> x) ->
  f (Offset, a) ->
  Cairo.Render ()
renderElements getG = traverse_ (\(v, a) -> renderElement a getG v)

class RenderElement g a | a -> g where
  renderElement :: a -> (forall x. g x -> x) -> Offset -> Cairo.Render ()

data SomeRenderElement g = forall a. RenderElement g a => SomeRenderElement a

instance RenderElement g (SomeRenderElement g) where
  renderElement (SomeRenderElement a) = renderElement a

instance RenderElement g (PrimRect g) where
  renderElement (PrimRect (Extents w h) gmbc) getG (Offset x y) = do
    for_ (getG gmbc) $ \bc -> do
      setSourceColor bc
      Cairo.rectangle
        (realToFrac x)
        (realToFrac y)
        (realToFrac w)
        (realToFrac h)
      Cairo.fill

data PangoText g = PangoText
  { ptextExtents :: Extents
  , ptextColor :: g Color
  , ptextCursor :: g (Maybe Natural)
  , ptextLayout :: Pango.PangoLayout
  }

primTextPango :: Cairo.Matrix -> PrimText g -> PangoText g
primTextPango matrix (PrimText font content cursor) = unsafePerformIO $ do
  pangoFont <- Pango.fontDescriptionNew
  pangoFont `Pango.fontDescriptionSetFamily` Text.unpack (fontFamily font)
  pangoFont `Pango.fontDescriptionSetSize` realToFrac (fontSize font)
  pangoFont `Pango.fontDescriptionSetWeight` (case fontWeight font of
      FontWeightNormal -> Pango.WeightNormal
      FontWeightBold -> Pango.WeightBold)
  pangoContext <- Pango.cairoCreateContext Nothing
  pangoContext `Pango.contextSetFontDescription` pangoFont
  pangoContext `Pango.contextSetMatrix` matrix
  pangoLayout <- Pango.layoutEmpty pangoContext
  pangoLayout `Pango.layoutSetText` Text.unpack content
  pangoLayout `Pango.layoutSetFontDescription` Just pangoFont
  (_, Pango.PangoRectangle _ _ w h) <-
    Pango.layoutGetExtents pangoLayout
  let extents = Extents (ceiling w) (ceiling h)
  return $ PangoText extents (fontColor font) cursor pangoLayout
{-# NOINLINE primTextPango #-}

instance RenderElement g (PangoText g) where
  renderElement (PangoText _ color gcursor pangoLayout) getG (Offset x y) = do
    Cairo.moveTo (realToFrac x) (realToFrac y)
    setSourceColor (getG color)
    Pango.showLayout pangoLayout
    for_ (getG gcursor) $ \n -> do
      Pango.PangoRectangle gx gy _ gh <-
        Cairo.liftIO $ Pango.layoutIndexToPos pangoLayout (fromIntegral n)
      Cairo.rectangle
        (realToFrac x + gx)
        (realToFrac y + gy)
        1
        (realToFrac gh)
      Cairo.fill

bend :: Curvature -> Double -> Double -> Double
bend (Curvature c) a b = realToFrac c' * (b - a) + a
  where
    c' = (c + 1) / 2

bendPoint :: Curvature -> (Double, Double) -> (Double, Double) -> (Double, Double)
bendPoint c (ax, ay) (bx, by) = (bend c ax bx, bend c ay by)

instance RenderElement g (PrimCurve g) where
  renderElement (PrimCurve extents gcurvature gcolor gdirection) getG (Offset x y) = do
    setSourceColor (getG gcolor)
    let
      curvature = getG gcurvature
      direction = getG gdirection
      Extents (realToFrac -> w) (realToFrac -> h) = extents
      x' = realToFrac x
      y' = realToFrac y
      (x1, x2) = if directionLeftToRight direction then (0, w) else (w, 0)
      (y1, y2) = if directionTopToBottom direction then (0, h) else (h, 0)
      p1 = bendPoint curvature (w/2, y1) (x1, h/2)
      p2 = bendPoint curvature (w/2, y2) (x2, h/2)
      p3 = (x2, y2)
      curveThrough pStart pMid pEnd =
        Cairo.curveTo
          (x' + fst pStart) (y' + snd pStart)
          (x' + fst pMid)   (y' + snd pMid)
          (x' + fst pEnd)   (y' + snd pEnd)
    Cairo.moveTo (x' + x1) (y' + y1)
    curveThrough p1 p2 p3
    Cairo.stroke

instance RenderElement g (PrimCircle g)   where
  renderElement (Circle cc c) getG (Offset x y) = do
      setSourceColor $ getG cc
      let dc = realToFrac c
      Cairo.arc (dc + fromInteger x) (dc + fromInteger y) dc 0 180
      Cairo.fill
      Cairo.stroke
