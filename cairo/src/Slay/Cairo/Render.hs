module Slay.Cairo.Render
  ( RenderElement(..),
    SomeRenderElement(..),
    renderElements
  ) where

import Data.Foldable (traverse_)

import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Core

renderElements ::
  Foldable f =>
  RenderElement g a =>
  (forall x. g x -> x) ->
  f (Offset, Extents, a) ->
  Cairo.Render ()
renderElements getG = traverse_ (renderElement getG)

class RenderElement g a | a -> g where
  renderElement ::
    (forall x. g x -> x) ->
    (Offset, Extents, a) ->
    Cairo.Render ()

data SomeRenderElement g = forall a. RenderElement g a => SomeRenderElement a

instance RenderElement g (SomeRenderElement g) where
  renderElement getG (offset, extents, SomeRenderElement a) =
    renderElement getG (offset, extents, a)
