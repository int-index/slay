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
  f (Offset, a) ->
  Cairo.Render ()
renderElements getG = traverse_ (\(v, a) -> renderElement a getG v)

class RenderElement g a | a -> g where
  renderElement :: a -> (forall x. g x -> x) -> Offset -> Cairo.Render ()

data SomeRenderElement g = forall a. RenderElement g a => SomeRenderElement a

instance RenderElement g (SomeRenderElement g) where
  renderElement (SomeRenderElement a) = renderElement a
