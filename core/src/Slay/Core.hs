module Slay.Core
  ( Offset(..)
  , offsetAdd
  , offsetSub
  , offsetMax
  , offsetZero
  , Extents(..)
  , extentsOffset
  , View
  , Collage()
  , collageSingleton
  , collageCompose
  , collageExtents
  , Layout()
  , hoistLayout
  , mkLayout
  , layoutElements
  ) where

import Data.Monoid (Endo(..), (<>))

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import Data.Kind
import Data.Proxy
import Data.Reflection

import Slay.Number

data Offset =
  Offset
    { offsetX :: Signed,
      offsetY :: Signed
    } deriving (Eq, Ord, Show)

offsetOp ::
  (Signed -> Signed -> Signed) ->
  (Offset -> Offset -> Offset)
offsetOp (#) o1 o2 =
  Offset
    { offsetX = offsetX o1 # offsetX o2,
      offsetY = offsetY o1 # offsetY o2 }

offsetAdd, offsetSub, offsetMax :: Offset -> Offset -> Offset
offsetAdd = offsetOp (+)
offsetSub = offsetOp (-)
offsetMax = offsetOp max

offsetNegate :: Offset -> Offset
offsetNegate (Offset x y) = Offset (negate x) (negate y)

offsetZero :: Offset
offsetZero = Offset 0 0

unsafeOffsetExtents :: Offset -> Extents
unsafeOffsetExtents (Offset x y) = Extents (unsafeToUnsigned x) (unsafeToUnsigned y)

data Extents =
  Extents
    { extentsW :: Unsigned,
      extentsH :: Unsigned
    } deriving (Eq, Ord, Show)

extentsOp ::
  (Unsigned -> Unsigned -> Unsigned) ->
  (Extents -> Extents -> Extents)
extentsOp (#) o1 o2 =
  Extents
    { extentsW = extentsW o1 # extentsW o2,
      extentsH = extentsH o1 # extentsH o2 }

extentsAdd, extentsMax :: Extents -> Extents -> Extents
extentsAdd = extentsOp (+)
extentsMax = extentsOp max

extentsOffset :: Extents -> Offset
extentsOffset (Extents w h) = Offset (toSigned w) (toSigned h)

type View' s e a = Reifies s (e -> (Extents, a))

class    View' s e a => View s e a
instance View' s e a => View s e a

data Collage (s :: Type) a =
  CollageSingleton
    Extents -- bounding box
    a
  |
  CollageCompose
    Extents -- bounding box
    (Offset, Collage s a) -- collage below, with offset[1]
    (Offset, Collage s a) -- collage above, with offset[1]
    -- [1] the offset is from the topleft corner of the bounding box and
    -- is always positive
  deriving (Functor)

collageExtents :: Collage s a -> Extents
collageExtents = \case
  CollageSingleton e _ -> e
  CollageCompose e _ _ -> e

collageElements ::
  Collage s a ->
  NonEmpty (Offset, a)
collageElements collage =
  NonEmpty.fromList $ collageElements' offsetZero collage `appEndo` []

type DList a = Endo [a]

collageElements' ::
  Offset ->
  Collage s a ->
  DList (Offset, a)
collageElements' offset = \case
  CollageSingleton _ a -> Endo ((offset, a):)
  CollageCompose _ (o1, c1) (o2, c2) ->
    collageElements' (offsetAdd o1 offset) c1 <>
    collageElements' (offsetAdd o2 offset) c2

collageSingleton ::
  forall s e a.
  View s e a =>
  e ->
  Collage s a
collageSingleton m =
  let
    view = reflect (Proxy :: Proxy s)
    (e, a) = view m
  in
    CollageSingleton e a

collageCompose ::
  View s e a =>
  Offset ->
  Collage s a ->
  Collage s a ->
  Collage s a
collageCompose offset c1 c2 = CollageCompose extents (o1, c1) (o2, c2)
  where
    o1 = offsetMax offsetZero (offsetNegate offset)
    o2 = offsetMax offsetZero offset
    e1 = unsafeOffsetExtents o1 `extentsAdd` collageExtents c1
    e2 = unsafeOffsetExtents o2 `extentsAdd` collageExtents c2
    extents = extentsMax e1 e2

-- `Layout (ReaderT ctx f) e` may perform better than `ctx -> Layout f e`
-- because it's possible to have sharing for parts of the collage that don't
-- depend on `ctx`.
data Layout f e = Layout (forall a. (e -> (Extents, a)) -> f (CollageWithView a))

instance Functor (Layout f) where
  fmap f (Layout mkCollage) =
    Layout (\view -> mkCollage (view . f))

type f ~> g = forall a. f a -> g a

hoistLayout :: (f ~> g) -> (Layout f ~> Layout g)
hoistLayout f (Layout mkCollage) =
  Layout (\view -> f (mkCollage view))

mkLayout ::
  Functor f =>
  (forall s a. View s e a => f (Collage s a)) ->
  Layout f e
mkLayout collage =
  Layout $ \view -> reify view (\p -> mkCollageWithView p <$> collage)

layoutElements ::
  Functor f =>
  (e -> (Extents, a)) ->
  Layout f e ->
  f (NonEmpty (Offset, a))
layoutElements view (Layout mkCollage) =
  mkCollage view <&> \case
    CollageWithView collage -> collageElements collage
  where
    (<&>) = flip (<$>)

data CollageWithView a =
  forall s. CollageWithView (Collage s a)

deriving instance Functor CollageWithView

mkCollageWithView :: Proxy s -> Collage s a -> CollageWithView a
mkCollageWithView _ collage = CollageWithView collage
