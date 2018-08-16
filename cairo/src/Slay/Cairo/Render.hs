module Slay.Cairo.Render (renderElements) where

import Data.Foldable (traverse_)

import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Core
import Slay.Cairo.Element

renderElements ::
  Foldable f =>
  (forall x. g x -> x) ->
  f (Positioned (CairoElement g)) ->
  Cairo.Render ()
renderElements getG =
  traverse_ $ \(At offset cairoElement) ->
    cairoElementRender cairoElement offset getG
