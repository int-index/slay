module Slay.Vty.Render
  ( renderImageElements
  ) where

import Graphics.Vty as Vty
import Slay.Core

renderImageElements :: [Positioned Image] -> Picture
renderImageElements [] = picForImage emptyImage
renderImageElements els = picForLayers (renderElement <$> els)
  where
    renderElement (At Offset{..} image) =
      translate (fromInteger offsetX) (fromInteger offsetY) image
