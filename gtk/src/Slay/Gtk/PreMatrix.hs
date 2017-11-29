module Slay.Gtk.PreMatrix
  ( PreMatrix(..),
    pmScaleL,
    pmRotateL,
    pmOffsetL,
    CachedPreMatrix(..),
    cachedPreMatrix
  ) where

import Data.Fixed
import Lens.Micro.Platform

import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Gtk.Util

data PreMatrix = PreMatrix
  { pmScale :: Centi,
    pmRotate :: Integer, -- 1/12
    pmOffset :: (Integer, Integer)
  }

makeLensesWith postfixLFields ''PreMatrix

data CachedPreMatrix = CachedPreMatrix
  { cpmPreMatrix :: PreMatrix,
    cpmMatrix1 :: Cairo.Matrix, -- cached result of preMatrix1 (no offset)
    cpmMatrix2 :: Cairo.Matrix  -- cached result of preMatrix2 (with offset)
  }

cachedPreMatrix :: PreMatrix -> CachedPreMatrix
cachedPreMatrix preMatrix = CachedPreMatrix
  { cpmPreMatrix = preMatrix,
    cpmMatrix1 = matrix1,
    cpmMatrix2 = matrix2
  }
  where
    matrix1 = prepareMatrix1 preMatrix
    matrix2 = prepareMatrix2 preMatrix matrix1

prepareMatrix1 :: PreMatrix -> Cairo.Matrix
prepareMatrix1 PreMatrix{..} =
  Matrix.rotate (pi * realToFrac pmRotate / 12) $
  Matrix.scale (realToFrac pmScale) (realToFrac pmScale) $
  Matrix.identity

prepareMatrix2 :: PreMatrix -> Cairo.Matrix -> Cairo.Matrix
prepareMatrix2 PreMatrix{..} =
  Matrix.translate (realToFrac $ fst pmOffset) (realToFrac $ snd pmOffset)
