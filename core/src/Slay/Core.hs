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
    DeriveFoldable,
    DeriveTraversable,
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
    offsetMin,
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
    collageComposeN,
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

    -- * LRTB
    LRTB(..)


  ) where

import Data.Monoid (Endo(..))
import Data.Functor.Identity
import Data.String (IsString(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (sconcat)
import Unsafe.Coerce (unsafeCoerce)

import Inj
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

-- | Offset pointwise minimum.
--
-- >>> offsetMin (Offset 10 1) (Offset 2 20)
-- Offset {offsetX = 2, offsetY = 1}
--
offsetMin :: Offset -> Offset -> Offset
offsetMin = offsetOp min

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
-- Note that 'offsetZero' is /not/ an identity element for 'offsetMin' or
-- 'offsetMax' becasue an offset can be negative.
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
    (NonEmpty (Offset, CollageRep a))
    -- 1. elements are ordered by z-index (ascending)
    -- 2. the offsets are from the top left corner of the bounding box and
    --    are always non-negative
  deriving (Functor)

-- | A collage of elements. Can be either a single element or a combination of
-- several sub-collages with relative offsets from a point. Structurally, it's a
-- rose tree of layers. After a collage is built, it can be converted to a
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

-- | Combine a pair of collages by placing one atop another. No offset. This
-- is a special case of 'collageCompose'.
instance Semigroup (Collage a) where
  c1 <> c2 =
    CollageRep $ CollageCompose e xs
    where
      e = extentsMax (collageExtents c1) (collageExtents c2)
      xs =
        (offsetZero, getCollageRep c1) :|
        (offsetZero, getCollageRep c2) : []

getViewOf :: s -/ e => Collage s -> View e (Element s)
getViewOf (_ :: Collage s) = getView @s

-- | Get the bounding box of a collage in constant time.
collageRepExtents :: CollageRep a -> Extents
collageRepExtents = \case
  CollageSingleton e _ -> e
  CollageCompose e _ -> e

-- | Get the bounding box of a collage in constant time.
collageExtents :: Collage s -> Extents
collageExtents = collageRepExtents . getCollageRep

-- | Get the width of a collage in constant time.
collageWidth :: Collage s -> Unsigned
collageWidth = extentsW . collageExtents

-- | Get the height of a collage in constant time.
collageHeight :: Collage s -> Unsigned
collageHeight = extentsH . collageExtents

-- | Get a non-empty list of primitives with absolute positions and computed
-- extents, ordered by z-index (ascending).
collageRepElements ::
  CollageRep a ->
  NonEmpty (Offset, Extents, a)
collageRepElements collageRep =
  NonEmpty.fromList $ collageRepElements' offsetZero collageRep `appEndo` []

type DList a = Endo [a]

collageRepElements' ::
  Offset ->
  CollageRep a ->
  DList (Offset, Extents, a)
collageRepElements' offset = \case
  CollageSingleton extents a -> Endo ((offset, extents, a):)
  CollageCompose _ xs ->
    let toElements (o, c) = collageRepElements' (offsetAdd o offset) c
    in sconcat $ fmap @NonEmpty toElements xs

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
--
-- This is a special case of 'collageComposeN'.
--
collageCompose ::
  Offset ->
  Collage s ->
  Collage s ->
  Collage s
collageCompose offset c1 c2 =
  collageComposeN $
    (offsetZero, c1) :|
    (offset, c2) :
    []

-- reimplementation of 'traverse1' with t~NonEmpty and f~(acc,) to avoid
-- a dependency on 'semigroupoids'
traverse1_NonEmpty_Writer ::
  forall a b acc.
  Semigroup acc =>
  (a -> (acc, b)) ->
  NonEmpty a ->
  (acc, NonEmpty b)
traverse1_NonEmpty_Writer f (a :| as) =
  let (acc, b) = f a
  in go acc (b:|) as
  where
    go acc endo [] = (acc, endo [])
    go acc endo (a' : as') =
      let (acc', b') = f a'
      in go (acc <> acc') (endo . (b':)) as'

data CollageComposeAccum = CollageComposeAccum Extents Offset

instance Semigroup CollageComposeAccum where
  CollageComposeAccum e1 o1 <> CollageComposeAccum e2 o2 =
    CollageComposeAccum (extentsMax e1 e2) (offsetMin o1 o2)

-- | A generalization of 'collageCompose' to take a non-empty list of
-- sub-collages instead of a pair.
--
-- The offsets are considered relative, so moving all sub-collages by the same
-- offset has no effect. A consequence of that is that when the input is a
-- singleton list, the offset is simply discarded.
--
collageComposeN :: NonEmpty (Offset, Collage s) -> Collage s
collageComposeN ((_, collage) :| []) =
  -- This special case is an optimization and does not affect the semantics.
  collage
collageComposeN elements =
  CollageRep $ CollageCompose resultExtents resultElements
  where
    (CollageComposeAccum resultExtents minOffset, resultElements) =
      traverse1_NonEmpty_Writer processElement elements

    processElement ::
      (Offset, Collage s) ->
      (CollageComposeAccum, (Offset, CollageRep (Element s)))
    processElement (offset, CollageRep collageRep) =
      let
        -- normalized offset, guaranteed to be non-negative
        offset' = offsetSub offset minOffset
        extents' =
          extentsAdd
            (unsafeOffsetExtents offset')
            (collageRepExtents collageRep)
        element' = (offset', collageRep)
        acc = CollageComposeAccum extents' offset
      in
        (acc, element')

-- | Collect the elements of a collage (see also: 'collageRepElements').
collageElements ::
  View e a ->
  (forall s. s -/ e => Collage s) ->
  NonEmpty (Offset, Extents, a)
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
  f (NonEmpty (Offset, Extents, a))
layoutElements view layout =
  collageRepElements <$> runLayout layout view

instance (s -/ e, Inj p e) => Inj p (Collage s) where
  inj = collageSingleton . inj

-- | A value for each side: left, right, top, bottom.
data LRTB a = LRTB
  { left :: a,
    right :: a,
    top :: a,
    bottom :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative LRTB where
  pure a = LRTB a a a a
  LRTB lf rf tf bf <*> LRTB la ra ta ba =
    LRTB (lf la) (rf ra) (tf ta) (bf ba)

instance (Inj p' a, p ~ LRTB p') => Inj p (LRTB a) where
  inj (LRTB l r t b) = LRTB (inj l) (inj r) (inj t) (inj b)
