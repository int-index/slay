module Slay.Gtk.Util
  ( postfixLFields,
    getExcess,
    boundingBox,
    snap,
    snapExtents,
    setBackground
  ) where

import Data.Semigroup
import Data.List.NonEmpty as NonEmpty
import Language.Haskell.TH.Syntax
import Lens.Micro.Platform

import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Cairo

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ appendSuffixL
  where appendSuffixL _ _ s = [TopName . mkName $ nameBase s ++ "L"]

getExcess :: Double -> Double -> Double
getExcess vacant actual = max 0 (vacant - actual)

boundingBox :: Ord a => NonEmpty (a, a) -> (a, a, a, a)
boundingBox xs = (l,r,t,b)
  where
    ((Min l, Min t), (Max r, Max b)) =
      sconcat $ (\(x, y) -> ((Min x, Min y), (Max x, Max y))) <$> xs

snap :: Double -> Double
snap = fromInteger . ceiling

snap' :: Unsigned -> Unsigned
snap' = unsafeToUnsigned . snap . toSigned

snapExtents :: Extents -> Extents
snapExtents (Extents w h) = Extents (snap' w) (snap' h)

setBackground :: Color -> Cairo.Render (Double, Double)
setBackground background = do
  (x1, y1, x2, y2) <- Cairo.clipExtents
  let viewport@(w, h) = (x2 - x1, y2 - y1)
  viewport <$ do
    Cairo.rectangle 0 0 w h
    setSourceColor background
    Cairo.fill
