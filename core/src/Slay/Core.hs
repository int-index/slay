{- |

The purpose of a layouting engine is to take a description of a layout
expressed using primitives (rectangles, circles, lines of text, etc) and
combinators (horizontal/vertical composition, layering, centering, etc) and
compute absolute coordinates for primitives on a 2-dimensional plane.

This module contains the core of the layouting engine. It's parametrized by the
coordinate types using Backpack.

The basic use of the library is to build a 'Collage' with the '-/'
constraint in the context, then extract its elements with 'collageElements'.

To build a collage, there are three primary combinators:

  * 'collageSingleton' to wrap a single element
  * 'collageCompose' to layer one collage atop another, with a relative offset
  * 'collageExtents' to get the size of a collage

Other combinators, such as horizontal and vertical composition, can be
expressed in terms of these three.

In case there is a need for a functorial context, instead of using
'collageElements', turn the collage into a 'Layout' with 'mkLayout', and then
collect the elements with their absolute coordinates using 'layoutElements'.

-}

{-# LANGUAGE
    AllowAmbiguousTypes,
    ConstraintKinds,
    DeriveFunctor,
    FlexibleInstances,
    FunctionalDependencies,
    GADTs,
    LambdaCase,
    PartialTypeSignatures,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies,
    UndecidableInstances
#-}

module Slay.Core
  (
    -- * Coordinates
    module Slay.Number,

    -- * Offset
    Offset(..),
    offsetAdd,
    offsetSub,
    offsetMax,
    offsetNegate,
    offsetZero,
    unsafeOffsetExtents,

    -- * Extents
    Extents(..),
    extentsAdd,
    extentsMax,
    extentsOffset,

    -- * View
    View,
    HasView(Element),
    type (-/),
    withView,
    ElementRefl(..),

    -- * Collage
    Collage(..),
    collageSingleton,
    collageCompose,
    collageExtents,
    collageWidth,
    collageHeight,
    collageElements,

    -- * CollageRep
    CollageRep(),
    collageRepElements,
    collageRepExtents,

    -- * Layout
    Layout(..),
    mkLayout,
    layoutElements,
    hoistLayout,

    -- * Injection
    Inj(..)

  ) where

import Data.Monoid (Endo(..), (<>))
import Data.Functor.Identity
import Data.String
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Unsafe.Coerce (unsafeCoerce)

import Slay.Number

-- | The position of a primitive (relative or absolute).
data Offset =
  Offset
    { offsetX :: Signed,
      offsetY :: Signed
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to offsets,
-- applying it to both dimensions.
offsetOp ::
  (Signed -> Signed -> Signed) ->
  (Offset -> Offset -> Offset)
offsetOp (#) o1 o2 =
  Offset
    { offsetX = offsetX o1 # offsetX o2,
      offsetY = offsetY o1 # offsetY o2 }

-- | Offset pointwise addition.
--
-- >>> offsetAdd (Offset 10 20) (Offset 1 2)
-- Offset {offsetX = 11, offsetY = 22}
--
offsetAdd :: Offset -> Offset -> Offset
offsetAdd = offsetOp (+)

-- | Offset pointwise subtraction.
--
-- >>> offsetSub (Offset 10 20) (Offset 1 2)
-- Offset {offsetX = 9, offsetY = 18}
--
offsetSub :: Offset -> Offset -> Offset
offsetSub = offsetOp (-)

-- | Offset pointwise maximum.
--
-- >>> offsetMax (Offset 10 1) (Offset 2 20)
-- Offset {offsetX = 10, offsetY = 20}
--
offsetMax :: Offset -> Offset -> Offset
offsetMax = offsetOp max

-- | Offset pointwise negation.
--
-- >>> offsetNegate (Offset 5 -10)
-- Offset {offsetX = -5, offsetY = 10}
--
offsetNegate :: Offset -> Offset
offsetNegate (Offset x y) = Offset (negate x) (negate y)

-- | Zero offset.
--
-- prop> offsetAdd offsetZero a = a
-- prop> offsetAdd a offsetZero = a
-- prop> offsetSub a offsetZero = a
-- prop> offsetSub offsetZero a = offsetNegate a
--
-- Note that 'offsetZero' is /not/ an identity element for 'offsetMax'
-- becasue an offset can be negative.
offsetZero :: Offset
offsetZero = Offset 0 0

-- | Convert an offset to extents.
-- Precondition: offset is non-negative, otherwise the function
-- throws @Underflow :: ArithException@.
unsafeOffsetExtents :: Offset -> Extents
unsafeOffsetExtents (Offset x y) = Extents (unsafeToUnsigned x) (unsafeToUnsigned y)

-- | The size of a primitive.
data Extents =
  Extents
    { extentsW :: Unsigned,
      extentsH :: Unsigned
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to extents,
-- applying it to both dimensions.
extentsOp ::
  (Unsigned -> Unsigned -> Unsigned) ->
  (Extents -> Extents -> Extents)
extentsOp (#) o1 o2 =
  Extents
    { extentsW = extentsW o1 # extentsW o2,
      extentsH = extentsH o1 # extentsH o2 }

-- | Extents pointwise addition.
--
-- >>> extentsAdd (Extents 10 20) (Extents 1 2)
-- Extents {extentsX = 11, extentsY = 22}
--
extentsAdd :: Extents -> Extents -> Extents
extentsAdd = extentsOp (+)

-- | Extents pointwise maximum.
--
-- >>> extentsMax (Extents 10 1) (Extents 2 20)
-- Extents {extentsX = 10, extentsY = 20}
--
extentsMax :: Extents -> Extents -> Extents
extentsMax = extentsOp max

-- | Convert extents to an offset.
extentsOffset :: Extents -> Offset
extentsOffset (Extents w h) = Offset (toSigned w) (toSigned h)

-- | A view is a function that converts an element of a layout
-- to a backend-specific primitive and its bounding box.

-- The @e@ type parameter stands for \"element\" and will be instantiated to
-- some user-defined sum-type of elements, whereas the @a@ type parameter
-- represents the backend-specific type of drawing primitives.
type View e a = e -> (Extents, a)

-- | The 'HasView' constraint in a function context means that the function has access
-- to a view function.
class HasView s e | s -> e where
  type Element s
  getView :: View e (Element s)

-- | An infix synonym for 'HasView'.
type (-/) = HasView

-- | Link the element type with its reflection tag.
-- This is a low-level primitive needed for 'withView'.
data ElementRefl s a where
  ElementRefl :: ElementRefl s (Element s)

newtype WithView s e a r = WithView (s -/ e => ElementRefl s a -> r)

-- | Reflect the view function into types.
-- This is a low-level primitive, consider using 'Layout' instead.
withView :: (forall s. s -/ e => ElementRefl s a -> r) -> (View e a -> r)
withView wv view = unsafeCoerce (WithView wv) view ElementRefl

-- | The internal representation of a collage.
data CollageRep a =
  CollageSingleton
    Extents -- bounding box
    a
  |
  CollageCompose
    Extents -- bounding box
    (Offset, CollageRep a) -- collage below, with offset[1]
    (Offset, CollageRep a) -- collage above, with offset[1]
    -- [1] the offset is from the top left corner of the bounding box and
    -- is always non-negative
  deriving (Functor)

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
newtype Collage s = CollageRep { getCollageRep :: CollageRep (Element s) }

getViewOf :: s -/ e => Collage s -> View e (Element s)
getViewOf (_ :: Collage s) = getView @s

-- | Get the bounding box of a collage in constant time.
collageRepExtents :: CollageRep a -> Extents
collageRepExtents = \case
  CollageSingleton e _ -> e
  CollageCompose e _ _ -> e

-- | Get the bounding box of a collage in constant time.
collageExtents :: Collage s -> Extents
collageExtents = collageRepExtents . getCollageRep

-- | Get the width of a collage in constant time.
collageWidth :: Collage s -> Unsigned
collageWidth = extentsW . collageExtents

-- | Get the height of a collage in constant time.
collageHeight :: Collage s -> Unsigned
collageHeight = extentsH . collageExtents

-- | Get a non-empty list of primitives with absolute positions,
-- ordered by z-index (ascending).
collageRepElements ::
  CollageRep a ->
  NonEmpty (Offset, a)
collageRepElements collageRep =
  NonEmpty.fromList $ collageRepElements' offsetZero collageRep `appEndo` []

type DList a = Endo [a]

collageRepElements' ::
  Offset ->
  CollageRep a ->
  DList (Offset, a)
collageRepElements' offset = \case
  CollageSingleton _ a -> Endo ((offset, a):)
  CollageCompose _ (o1, c1) (o2, c2) ->
    collageRepElements' (offsetAdd o1 offset) c1 <>
    collageRepElements' (offsetAdd o2 offset) c2

-- | Construct a collage from a single element.
collageSingleton ::
  s -/ e =>
  e ->
  Collage s
collageSingleton m =
  let
    view = getViewOf collage
    (e, a) = view m
    collage = CollageRep (CollageSingleton e a)
  in
    collage

instance (s -/ e, IsString e) => IsString (Collage s) where
  fromString = collageSingleton . fromString

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
  s -/ e =>
  Offset ->
  Collage s ->
  Collage s ->
  Collage s
collageCompose offset c1 c2 =
    CollageRep $ CollageCompose extents
      (o1, getCollageRep c1)
      (o2, getCollageRep c2)
  where
    o1 = offsetMax offsetZero (offsetNegate offset)
    o2 = offsetMax offsetZero offset
    e1 = unsafeOffsetExtents o1 `extentsAdd` collageExtents c1
    e2 = unsafeOffsetExtents o2 `extentsAdd` collageExtents c2
    extents = extentsMax e1 e2

-- | Collect the elements of a collage (see also: 'collageRepElements').
collageElements ::
  View e a ->
  (forall s. s -/ e => Collage s) ->
  NonEmpty (Offset, a)
collageElements view collage =
  runIdentity $ layoutElements view $ mkLayout (Identity collage)

-- | @'Layout' f e@ is a wrapper around @s '-/' e => f ('Collage' s)@.
-- Internally, it takes 'View' as an explicit argument instead of a 'HasView'
-- constraint.
--
-- 'Layout' is a bifunctor from @H^H * H@ to @H@, see 'hoistLayout' and 'fmap'.
--
newtype Layout f e =
  Layout
    { runLayout :: forall a. View e a -> f (CollageRep a) }
  deriving (Functor)

type f ~> g = forall a. f a -> g a

-- | Map over the functorial context of a layout.
hoistLayout :: (f ~> g) -> (Layout f ~> Layout g)
hoistLayout f (Layout mkCollage) = Layout (f . mkCollage)

-- | Construct a layout from a collage.
mkLayout ::
  Functor f =>
  (forall s. s -/ e => f (Collage s)) ->
  Layout f e
mkLayout collage =
  Layout $ withView $
    \(ElementRefl :: ElementRefl s a) ->
      -- TODO: optimize with fmapCoerce
      getCollageRep <$> collage @s

-- | Collect the elements of a layout (see also: 'collageRepElements').
layoutElements ::
  Functor f =>
  View e a ->
  Layout f e ->
  f (NonEmpty (Offset, a))
layoutElements view layout =
  collageRepElements <$> runLayout layout view

-- | Inject @p@ into a sum type @a@.
class Inj p a where
  inj :: p -> a

instance Inj a a where
  inj = id

instance Inj p a => Inj p (Maybe a) where
  inj = Just . inj

instance (s -/ e, Inj p e) => Inj p (Collage s) where
  inj = collageSingleton . inj
