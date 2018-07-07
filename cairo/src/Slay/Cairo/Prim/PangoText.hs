module Slay.Cairo.Prim.PangoText
  ( PangoText(..),
    primTextPango,
    renderElementPangoText
  ) where

import Numeric.Natural
import Data.Foldable (for_)

import System.IO.Unsafe (unsafePerformIO)
import Data.Text as Text
import qualified Data.LruCache.IO as LRU

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import Slay.Core

import Slay.Cairo.Prim.Text
import Slay.Cairo.Prim.Color
import Slay.Cairo.Render

data PangoText g =
  PangoText
    { ptextExtents :: Extents,
      ptextFont :: Font,
      ptextColor :: g Color,
      ptextContent :: Text,
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

layoutCacheHndl :: LRU.LruHandle (Font, Text) (Pango.PangoLayout, Extents)
layoutCacheHndl = unsafePerformIO (LRU.newLruHandle 1000)
{-# NOINLINE layoutCacheHndl #-}

primTextPango :: Cairo.Matrix -> PrimText g -> PangoText g
primTextPango matrix (PrimText font gcolor content cursor) = unsafePerformIO $ do
  (layout, extents) <- LRU.cached layoutCacheHndl (font, content) $ do
    pangoFont <- primFontPango font
    pangoContext <- Pango.cairoCreateContext Nothing
    pangoContext `Pango.contextSetFontDescription` pangoFont
    pangoContext `Pango.contextSetMatrix` matrix
    pangoLayout <- Pango.layoutEmpty pangoContext
    pangoLayout `Pango.layoutSetText` Text.unpack content
    pangoLayout `Pango.layoutSetFontDescription` Just pangoFont
    (_, Pango.PangoRectangle _ _ w h) <-
      Pango.layoutGetExtents pangoLayout
    let e = Extents (unsafeToUnsigned w) (unsafeToUnsigned h)
    return (pangoLayout, e)
  return $ PangoText extents font gcolor content cursor layout
{-# NOINLINE primTextPango #-}

surfaceCacheHndl :: LRU.LruHandle (Font, Color, Text) Cairo.Surface
surfaceCacheHndl = unsafePerformIO (LRU.newLruHandle 1000)
{-# NOINLINE surfaceCacheHndl #-}

renderElementPangoText :: (forall x. g x -> x) -> (Offset, Extents, PangoText g) -> Cairo.Render ()
renderElementPangoText getG (Offset x y, Extents w h, PangoText _ font gcolor content gcursor pangoLayout) = do
  let color = getG gcolor
  -- TODO: take the transformation matrix into account, otherwise the text
  -- is scaled after rendering and becomes blurry
  surface <- Cairo.liftIO $ LRU.cached surfaceCacheHndl (font, color, content) $ do
    s <- Cairo.createImageSurface Cairo.FormatARGB32 (ceil w) (ceil h)
    Cairo.renderWith s $ do
      setSourceColor color
      Pango.showLayout pangoLayout
    return s
  Cairo.setSourceSurface surface x y
  Cairo.paint
  setSourceColor color
  Cairo.moveTo x y
  for_ (getG gcursor) $ \n -> do
    Pango.PangoRectangle gx gy _ gh <-
      Cairo.liftIO $ Pango.layoutIndexToPos pangoLayout (fromIntegral n)
    Cairo.rectangle (x + gx) (y + gy) 1 (gh)
    Cairo.fill

instance RenderElement g (PangoText g) where
  renderElement = renderElementPangoText
