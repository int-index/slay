{- |

The purpose of a layouting engine is to take a description of a layout
expressed using primitives (rectangles, circles, lines of text, etc) and
combinators (horizontal/vertical composition, layering, centering, etc) and
compute absolute coordinates for primitives on a 2-dimensional plane.

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

We represent coordinates and distances in device units (pixels or characters)
using types without a fractional component ('Natural' and 'Integer'). The reason
for this is to guarantee that the resulting collage can be rendered without
undesired anti-aliasing, as our focus is user interfaces and not abstract
vector graphics.

-}

{-# LANGUAGE
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    FlexibleInstances,
    MultiParamTypeClasses,
    GADTs,
    LambdaCase,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    UndecidableInstances
#-}

module Slay.Core
  (
    -- * Offset
    Offset(..),
    offsetAdd,
    offsetSub,
    offsetMin,
    offsetMax,
    offsetNegate,
    offsetZero,
    unsafeOffsetExtents,

    -- * Positioned
    Positioned(..),

    -- * Extents
    Extents(..),
    extentsAdd,
    extentsMax,
    extentsOffset,
    HasExtents(..),

    -- * Collage
    Collage,
    collageSingleton,
    collageCompose,
    collageComposeN,
    collageExtents,
    collageWidth,
    collageHeight,
    collageElements,

    -- * LRTB
    LRTB(..)

  ) where

import Numeric.Natural (Natural)
import Data.Monoid (Endo(..))
import Data.String (IsString(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (sconcat)

import Inj

-- | The position of a primitive (relative or absolute).
data Offset =
  Offset
    { offsetX :: Integer,
      offsetY :: Integer
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to offsets,
-- applying it to both dimensions.
offsetOp ::
  (Integer -> Integer -> Integer) ->
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
unsafeOffsetExtents (Offset x y) = Extents (fromInteger x) (fromInteger y)

-- | Positioned item.
data Positioned a = At { positionedOffset :: Offset, positionedItem :: a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The size of a primitive.
data Extents =
  Extents
    { extentsW :: Natural,
      extentsH :: Natural
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to extents,
-- applying it to both dimensions.
extentsOp ::
  (Natural -> Natural -> Natural) ->
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
extentsOffset (Extents w h) = Offset (toInteger w) (toInteger h)

class HasExtents a where
  extentsOf :: a -> Extents

-- | The elements of a collage.
data CollageTree a =
  CollageSingleton a |
  CollageCompose (NonEmpty (Offset, CollageTree a))
    -- 1. elements are ordered by z-index (ascending)
    -- 2. the offsets are from the top left corner of the bounding box of the
    --    current subcollage and are always non-negative

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
data Collage a =
  Collage Extents (CollageTree a)

-- | Get the bounding box of a collage in constant time.
collageExtents :: Collage a -> Extents
collageExtents (Collage e _) = e

collageTree :: Collage a -> CollageTree a
collageTree (Collage _ c) = c

-- | Get the width of a collage in constant time.
collageWidth :: Collage a -> Natural
collageWidth = extentsW . collageExtents

-- | Get the height of a collage in constant time.
collageHeight :: Collage a -> Natural
collageHeight = extentsH . collageExtents

-- | Get a non-empty list of primitives with absolute positions and computed
-- extents, ordered by z-index (ascending).
--
-- The input offset is the position for the top-left corner of the collage.
collageElements ::
  Offset ->
  Collage a ->
  NonEmpty (Positioned a)
collageElements offset collage =
  NonEmpty.fromList $ collageTreeElements offset (collageTree collage) `appEndo` []

type DList a = Endo [a]

collageTreeElements ::
  Offset ->
  CollageTree a ->
  DList (Positioned a)
collageTreeElements offset = \case
  CollageSingleton a -> Endo (At offset a:)
  CollageCompose xs ->
    let toElements (o, c) = collageTreeElements (offsetAdd o offset) c
    in sconcat $ fmap @NonEmpty toElements xs

-- | Construct a collage from a single element.
collageSingleton :: HasExtents a => a -> Collage a
collageSingleton a =
  let extents = extentsOf a
  in Collage extents (CollageSingleton a)

instance (HasExtents a, IsString a) => IsString (Collage a) where
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
  Collage a ->
  Collage a ->
  Collage a
collageCompose offset c1 c2 =
  positionedItem . collageComposeN $
    At offsetZero c1 :|
    At offset c2 :
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
-- Offset common between all elements is factored out into the position of the
-- resulting collage.
--
collageComposeN ::
  NonEmpty (Positioned (Collage a)) ->
  Positioned (Collage a)
collageComposeN (positionedCollage :| []) =
  -- This special case is an optimization and does not affect the semantics.
  positionedCollage
collageComposeN elements =
  At minOffset resultCollage
  where
    resultCollage =
      Collage resultExtents (CollageCompose resultElements)

    (CollageComposeAccum resultExtents minOffset, resultElements) =
      traverse1_NonEmpty_Writer processElement elements

    processElement ::
      Positioned (Collage a) ->
      (CollageComposeAccum, (Offset, CollageTree a))
    processElement (At offset collage) =
      let
        -- normalized offset, guaranteed to be non-negative
        offset' = offsetSub offset minOffset
        extents' =
          extentsAdd
            (unsafeOffsetExtents offset')
            (collageExtents collage)
        element' = (offset', collageTree collage)
        acc = CollageComposeAccum extents' offset
      in
        (acc, element')

instance Semigroup (Positioned (Collage a)) where
  a <> b = sconcat (a :| b : [])
  sconcat = collageComposeN

instance (HasExtents a, Inj p a) => Inj p (Collage a) where
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
