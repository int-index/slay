{-# LANGUAGE StrictData #-}

module Slay.Cairo.Element
  ( CairoElement(..),
    cairoPositionedElementRender,
    CairoRender(..)
  ) where

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Core

-- A monadic action to draw a picture.
newtype CairoRender g =
  CairoRender { cairoRender :: (forall x. g x -> x) -> Cairo.Render () }

instance Semigroup (CairoRender g) where
  r1 <> r2 =
    CairoRender $ \getG -> do
      cairoRender r1 getG
      cairoRender r2 getG

data CairoElement g =
  CairoElement
    { cairoElementExtents :: Extents,
      cairoElementBaseline :: Baseline,
      cairoElementRender :: Offset -> CairoRender g
    }

cairoPositionedElementRender :: Positioned (CairoElement g) -> CairoRender g
cairoPositionedElementRender (At o e) = cairoElementRender e o

instance HasExtents (CairoElement g) where
  extentsOf = cairoElementExtents

instance HasBaseline (CairoElement g) where
  baselineOf = cairoElementBaseline

instance g1 ~ g2 => Inj (CairoElement g1) (CairoElement g2) where
  inj = id
