module Slay.Cairo.Element
  ( CairoElementRender,
    CairoElement(..)
  ) where

import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Slay.Core

type CairoElementRender g =
  Offset ->
  (forall x. g x -> x) ->
  Cairo.Render ()

data CairoElement g =
  CairoElement
    { cairoElementExtents :: Extents,
      cairoElementRender ::
        Offset ->
        (forall x. g x -> x) ->
        Cairo.Render ()
    }

instance HasExtents (CairoElement g) where
  extentsOf = cairoElementExtents

instance g1 ~ g2 => Inj (CairoElement g1) (CairoElement g2) where
  inj = id
