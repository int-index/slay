module Slay.Vty.Render
  ( renderImageElements
  ) where

import Graphics.Vty as Vty
import Slay.Core

renderImageElements :: [(Offset, Image)] -> Picture
renderImageElements [] = picForImage emptyImage
renderImageElements els = picForLayers (renderElement <$> els)
  where
    renderElement (Offset{..}, image) =
      translate (fromInteger offsetX) (fromInteger offsetY) image
