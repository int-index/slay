module Slay.Cairo.Prim.PangoText
  ( PangoText(..),
    primTextPango,
    renderElementPangoText
  ) where

import Numeric.Natural
import Data.Foldable (for_)

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as Text

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import Slay.Core

import Slay.Cairo.Prim.Text
import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PangoText g =
  PangoText
    { ptextExtents :: Extents,
      ptextColor :: g Color,
      ptextCursor :: g (Maybe Natural),
      ptextLayout :: Pango.PangoLayout
    }

primFontPango :: Font -> IO Pango.FontDescription
primFontPango font = do
  pangoFont <- Pango.fontDescriptionNew
  pangoFont `Pango.fontDescriptionSetFamily` Text.unpack (fontFamily font)
  pangoFont `Pango.fontDescriptionSetSize` realToFrac (fontSize font)
  pangoFont `Pango.fontDescriptionSetWeight` (case fontWeight font of
      FontWeightNormal -> Pango.WeightNormal
      FontWeightBold -> Pango.WeightBold)
  return pangoFont

primTextPango :: Cairo.Matrix -> PrimText g -> PangoText g
primTextPango matrix (PrimText font gcolor content cursor) = unsafePerformIO $ do
  pangoFont <- primFontPango font
  pangoContext <- Pango.cairoCreateContext Nothing
  pangoContext `Pango.contextSetFontDescription` pangoFont
  pangoContext `Pango.contextSetMatrix` matrix
  pangoLayout <- Pango.layoutEmpty pangoContext
  pangoLayout `Pango.layoutSetText` Text.unpack content
  pangoLayout `Pango.layoutSetFontDescription` Just pangoFont
  (_, Pango.PangoRectangle _ _ w h) <-
    Pango.layoutGetExtents pangoLayout
  let extents = Extents (unsafeToUnsigned w) (unsafeToUnsigned h)
  return $ PangoText extents gcolor cursor pangoLayout
{-# NOINLINE primTextPango #-}

renderElementPangoText :: PangoText g -> (forall x. g x -> x) -> Offset -> Cairo.Render ()
renderElementPangoText (PangoText _ color gcursor pangoLayout) getG (Offset x y) = do
  Cairo.moveTo x y
  setSourceColor (getG color)
  Pango.showLayout pangoLayout
  for_ (getG gcursor) $ \n -> do
    Pango.PangoRectangle gx gy _ gh <-
      Cairo.liftIO $ Pango.layoutIndexToPos pangoLayout (fromIntegral n)
    Cairo.rectangle (x + gx) (y + gy) 1 (gh)
    Cairo.fill

instance RenderElement g (PangoText g) where
  renderElement = renderElementPangoText
