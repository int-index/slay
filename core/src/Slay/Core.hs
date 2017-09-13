{- |

The purpose of a layouting engine is to take a description of a layout
expressed using primitives (rectangles, circles, lines of text, etc) and
combinators (horizontal/vertical composition, layering, centering, etc) and
compute absolute coordinates for primitives on a 2-dimensional plane.

This module contains the core of the layouting engine. It's parametrized by the
coordinate types using Backpack.

The basic use of the library is to build a 'Collage' with the 'HasView'
constraint in the context, then turn it into a 'Layout' using 'mkLayout', and
then collect the elements with their absolute coordinates using
'layoutElements'.

To build a collage, there are three primary combinators:

  * 'collageSingleton' to wrap a single element
  * 'collageCompose' to layer one collage atop another, with a relative offset
  * 'collageExtents' to get the size of a collage

Other combinators, such as horizontal and vertical composition, can be
expressed in terms of these three.

-}

module Slay.Core
  (
    -- * Coordinates
    module Slay.Number,

    -- * Offset
    Offset(..),
    offsetAdd,
    offsetSub,
    offsetMax,
    offsetZero,
    Extents(..),
    extentsOffset,

    -- * View
    View,
    HasView,

    -- * Collage
    Collage(),
    collageSingleton,
    collageCompose,
    collageExtents,

    -- * Layout
    Layout(),
    hoistLayout,
    mkLayout,
    layoutElements

  ) where

import Data.Monoid (Endo(..), (<>))

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import Data.Kind
import Data.Proxy
import Data.Reflection

import Slay.Number

-- | The position of a primitive (relative or absolute).
data Offset =
  Offset
    { offsetX :: Signed,
      offsetY :: Signed
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to offsets,
--   applying it to both dimensions.
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

-- | The size of a primitive.
data Extents =
  Extents
    { extentsW :: Unsigned,
      extentsH :: Unsigned
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to extents,
--   applying it to both dimensions.
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

-- | A view is a function that converts an element of a layout
-- to a backend-specific primitive and its bounding box.

-- The @e@ type parameter stands for \"element\" and will be instantiated to
-- some user-defined sum-type of elements, whereas the @a@ type parameter
-- represents the backend-specific type of drawing primitives.
type View e a = e -> (Extents, a)

-- | The 'HasView' constraint in a function means that the function has access
-- to a view function.
class    Reifies s (View e a) => HasView s e a
instance Reifies s (View e a) => HasView s e a

-- | Get the view function from the environment.
getView :: HasView s e a => Proxy s -> View e a
getView = reflect

-- | A collage of elements. Can be either a single element or a combination of
-- two sub-collages with relative offsets from a point. Structurally, it's a
-- binary tree of layers. After a collage is built, it can be converted to a
-- non-empty list of elements coupled with their absolute coordinates using the
-- 'collageElements' function, or it can be turned into a layout using the
-- 'mkLayout' function.
--
-- Here's a visualisation of a collage with two rectangular elements:
--
-- @
--      top-left corner
--     /
--    *   +---+
--        |   |
--    +-+ +---+
--    | |
--    +-+     *
--             \\
--             bottom-right corner
-- @
--
-- The bounding box (extents) of a collage is a vector from its top-left corner
-- to the bottom-right corner. Although the bounding box can be computed, it's
-- stored as a separate field for performance. Therefore, getting the extents of
-- a collage with the 'collageExtents' function is a constant-time operation.
--
data Collage (s :: Type) a =
  CollageSingleton
    Extents -- bounding box
    a
  |
  CollageCompose
    Extents -- bounding box
    (Offset, Collage s a) -- collage below, with offset[1]
    (Offset, Collage s a) -- collage above, with offset[1]
    -- [1] the offset is from the top left corner of the bounding box and
    -- is always non-negative
  deriving (Functor)

-- | Get the bounding box of a collage in constant time.
collageExtents :: Collage s a -> Extents
collageExtents = \case
  CollageSingleton e _ -> e
  CollageCompose e _ _ -> e

-- | Get a non-empty list of primitives with absolute positions,
-- ordered by z-index (ascending).
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

-- | Construct a collage from a single element.
collageSingleton ::
  forall s e a.
  HasView s e a =>
  e ->
  Collage s a
collageSingleton m =
  let
    view = getView (Proxy :: Proxy s)
    (e, a) = view m
  in
    CollageSingleton e a

-- | Combine a pair of collages by placing one atop another
-- with an offset. For instance, an @'Offset' a b@ would yield
-- the following result:
--
-- @
-- +------------+ collage below (first argument)
-- |     ^      |
-- |     |b     |
-- |  a  v      |
-- | \<-\> +------------+ collage above (second argument)
-- |     |            |
-- +-----|            |
--       |            |
--       +------------+
-- @
collageCompose ::
  HasView s e a =>
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

-- | @'Layout' f e@ is a wrapper around @HasView s e a => f (Collage s a)@.
-- Internally, it takes 'View' as an explicit argument instead of a 'HasView'
-- constraint, and uses continuation-passing style to encode existential
-- quantification of the 's' parameter.
--
-- The @f@ parameter is not strictly necessary, as @'Layout' f e@ is (almost)
-- the same as @f (Layout Identity e)@. However, @Layout (ReaderT ctx f) e@ may
-- perform better than @ctx -> Layout f e@ because it's possible to have sharing
-- for parts of the collage that don't depend on @ctx@.
--
-- 'Layout' is a bifunctor from @H^H * H@ to @H@, see 'hoistLayout' and 'fmap'.
--
data Layout f e = Layout (forall a r. View e a -> (forall s. Collage s a -> r) -> f r)

instance Functor (Layout f) where
  fmap f (Layout mkCollage) =
    Layout (\view -> mkCollage (view . f))

type f ~> g = forall a. f a -> g a

-- | Map over the functorial context of a layout.
hoistLayout :: (f ~> g) -> (Layout f ~> Layout g)
hoistLayout f (Layout mkCollage) =
  Layout (\view cont -> f (mkCollage view cont))

-- | Construct a layout from a collage.
mkLayout ::
  Functor f =>
  (forall s a. HasView s e a => f (Collage s a)) ->
  Layout f e
mkLayout collage =
  Layout $ \view cont -> reify view (\p -> h p cont <$> collage)
  where
    h :: Proxy s -> (Collage s a -> r) -> Collage s a -> r
    h _ cont = cont

-- | Collect the elements of a layout. Analogous to 'collageElements', but takes
-- the view function as an explicit parameter.
layoutElements ::
  Functor f =>
  View e a ->
  Layout f e ->
  f (NonEmpty (Offset, a))
layoutElements view (Layout mkCollage) = mkCollage view collageElements
