module Slay.Cairo.Prim.Text
  ( FontWeight(..),
    Font(..),
    text
  ) where

import Data.Fixed
import Data.Text
import Data.Hashable
import Numeric.Natural
import GHC.Generics (Generic)
import Data.Foldable (for_)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.LruCache.IO as LRU
import Data.Text as Text

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import Inj
import Slay.Core
import Slay.Cairo.Element
import Slay.Cairo.Prim.Color

data FontWeight =
  FontWeightBold | FontWeightNormal
  deriving (Eq, Ord, Show, Generic)

instance Hashable FontWeight

instance p ~ FontWeight => Inj p FontWeight

data Font =
  Font
    { fontFamily :: Text,
      fontSize :: Centi,
      fontWeight :: FontWeight
    } deriving (Eq, Ord, Show, Generic)

instance Hashable Font

instance p ~ Font => Inj p Font

text ::
  forall g a.
  Inj (CairoElement g) a =>
  Font ->
  g Color ->
  Text ->
  g (Maybe Natural) ->
  a
text font gcolor content gcursor =
  inj CairoElement
    { cairoElementExtents = extents,
      cairoElementRender = render }
  where
    (extents, pangoLayout) = primTextPango font content

    render :: Offset -> (forall x. g x -> x) -> Cairo.Render ()
    render (Offset x y) getG = do
      let Extents w h = extents
      let color = getG gcolor
      -- TODO: take the transformation matrix into account, otherwise the text
      -- is scaled after rendering and becomes blurry
      surface <- Cairo.liftIO $ LRU.cached surfaceCacheHndl (font, color, content) $ do
        s <- Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)
        Cairo.renderWith s $ do
          setSourceColor color
          Pango.showLayout pangoLayout
        return s
      Cairo.setSourceSurface surface (fromIntegral x) (fromIntegral y)
      Cairo.paint
      setSourceColor color
      Cairo.moveTo (fromIntegral x) (fromIntegral y)
      for_ (getG gcursor) $ \n -> do
        Pango.PangoRectangle gx gy _ gh <-
          Cairo.liftIO $ Pango.layoutIndexToPos pangoLayout (fromIntegral n)
        Cairo.rectangle
          (fromIntegral x + gx)
          (fromIntegral y + gy)
          1
          gh
        Cairo.fill

primFontPango :: Font -> IO Pango.FontDescription
primFontPango font = do
  pangoFont <- Pango.fontDescriptionNew
  pangoFont `Pango.fontDescriptionSetFamily` Text.unpack (fontFamily font)
  pangoFont `Pango.fontDescriptionSetSize` realToFrac (fontSize font)
  pangoFont `Pango.fontDescriptionSetWeight` (case fontWeight font of
      FontWeightNormal -> Pango.WeightNormal
      FontWeightBold -> Pango.WeightBold)
  return pangoFont

layoutCacheHndl :: LRU.LruHandle (Font, Text) (Extents, Pango.PangoLayout)
layoutCacheHndl = unsafePerformIO (LRU.newLruHandle 1000)
{-# NOINLINE layoutCacheHndl #-}

primTextPango :: Font -> Text -> (Extents, Pango.PangoLayout)
primTextPango font content = unsafePerformIO $ do
  LRU.cached layoutCacheHndl (font, content) $ do
    pangoFont <- primFontPango font
    pangoContext <- Pango.cairoCreateContext Nothing
    pangoContext `Pango.contextSetFontDescription` pangoFont
    -- pangoContext `Pango.contextSetMatrix` matrix
    pangoLayout <- Pango.layoutEmpty pangoContext
    pangoLayout `Pango.layoutSetText` Text.unpack content
    pangoLayout `Pango.layoutSetFontDescription` Just pangoFont
    (_, Pango.PangoRectangle _ _ w h) <-
      Pango.layoutGetExtents pangoLayout
    let e = Extents (ceiling w) (ceiling h)
    return (e, pangoLayout)
{-# NOINLINE primTextPango #-}

surfaceCacheHndl :: LRU.LruHandle (Font, Color, Text) Cairo.Surface
surfaceCacheHndl = unsafePerformIO (LRU.newLruHandle 1000)
{-# NOINLINE surfaceCacheHndl #-}
