module Slay.Vty.Prim
  ( Prim(..),
    image,
    space,
    string,
    empty
  ) where

import qualified Graphics.Vty as Vty
import Data.String

import Inj
import Slay.Core

data Prim =
  Prim
    { primExtents :: Extents,
      primImage :: Vty.Image
    }
  deriving (Eq, Show)

instance p ~ Prim => Inj p Prim

image :: Inj Prim a => Vty.Image -> a
image img = inj (Prim e img)
  where
    e = vtyImageExtents img

space :: Inj Prim a => Extents -> a
space e = inj (Prim e img)
  where
    Extents w h = e
    img = Vty.backgroundFill (toSigned w) (toSigned h)

string :: Inj Prim a => Vty.Attr -> String -> a
string attr str = image (Vty.string attr str)

empty :: Inj Prim a => a
empty = inj (Prim e img)
  where
    e = Extents 0 0
    img = Vty.emptyImage

instance IsString Prim where
  fromString = string Vty.defAttr

vtyImageExtents :: Vty.Image -> Extents
vtyImageExtents img =
  Extents
    { extentsW = unsafeToUnsigned (Vty.imageWidth img),
      extentsH = unsafeToUnsigned (Vty.imageHeight img) }
