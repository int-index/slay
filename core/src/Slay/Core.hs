{- |

The purpose of a layouting engine is to take a description of a layout
expressed using primitives (rectangles, circles, lines of text, etc) and
combinators (horizontal/vertical composition, layering, centering, etc) and
compute absolute coordinates for primitives on a 2-dimensional plane.

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
    FlexibleContexts,
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
    extentsWithOffset,
    HasExtents(..),
    heightOf,
    widthOf,

    -- * Margin
    Margin(..),
    marginZero,
    marginMax,

    -- * Baseline
    Baseline(..),
    baselineMin,
    baselineWithOffset,
    HasBaseline(..),

    -- * Collage
    Collage,
    collageSingleton,
    collageCompose,
    collageComposeN,
    collageExtents,
    collageWidth,
    collageHeight,
    collageMargin,
    collageBaseline,
    collageWithMargin,
    collageElements,

    -- * Decoration
    Decoration(..),
    collageDecorate,

    -- * LRTB
    LRTB(..),
    lrtb

  ) where

import Numeric.Natural (Natural)
import Data.String (IsString(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (foldl')
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (sconcat)

import Inj

-- | The position of an item (relative or absolute).
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

-- | The size of an item.
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

-- | Compute the extents for a subcollage in a composition.
--
-- Precondition: offset is non-negative, otherwise the function
-- throws @Underflow :: ArithException@.
extentsWithOffset :: Offset -> Extents -> Extents
extentsWithOffset offset = extentsAdd (unsafeOffsetExtents offset)

-- | A class of items that have extents.
class HasExtents a where
  extentsOf :: a -> Extents

heightOf, widthOf :: HasExtents a => a -> Natural
heightOf = extentsH . extentsOf
widthOf = extentsW . extentsOf

-- | A minimum recommended distance from an item to any other item.
data Margin =
  Margin
    { marginLeft :: Natural,
      marginRight :: Natural,
      marginTop :: Natural,
      marginBottom :: Natural
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to margins,
-- applying it to all four directions.
marginOp ::
  (Natural -> Natural -> Natural) ->
  (Margin -> Margin -> Margin)
marginOp (#) m1 m2 =
  Margin
    { marginLeft = marginLeft m1 # marginLeft m2,
      marginRight = marginRight m1 # marginRight m2,
      marginTop = marginTop m1 # marginTop m2,
      marginBottom = marginBottom m1 # marginBottom m2 }

-- | Zero margin.
marginZero :: Margin
marginZero = Margin 0 0 0 0

-- | Margin pointwise maximum.
--
-- >>> marginMax (Margin 1 10 2 20) (Margin 3 4 5 6)
-- Margin {marginLeft = 3, marginRight = 10, marginTop = 5, marginBottom = 20}
--
marginMax :: Margin -> Margin -> Margin
marginMax = marginOp max

-- | The imaginary line upon which the topmost line of text rests, expressed as
-- a distance from the top edge. May not be present if the item does not
-- contain text.
data Baseline = NoBaseline | Baseline Natural
  deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to baselines.
baselineOp ::
  (Natural -> Natural -> Natural) ->
  (Baseline -> Baseline -> Baseline)
baselineOp _ NoBaseline l2 = l2
baselineOp _ l1 NoBaseline = l1
baselineOp (#) (Baseline l1) (Baseline l2) = Baseline (l1 # l2)

-- | Baseline minimum.
--
-- >>> baselineMin (Baseline 10) (Baseline 20)
-- Baseline 10
--
-- >>> baselineMin (Baseline 20) NoBaseline
-- Baseline 20
--
baselineMin :: Baseline -> Baseline -> Baseline
baselineMin = baselineOp min

-- | Compute the baseline for a subcollage in a composition.
--
-- Precondition: offset is non-negative, otherwise the function
-- throws @Underflow :: ArithException@.
baselineWithOffset :: Offset -> Baseline -> Baseline
baselineWithOffset _ NoBaseline = NoBaseline
baselineWithOffset offset (Baseline l1) =
  Baseline (l1 + fromInteger (offsetY offset))

-- | A class of items that have a baseline.
class HasBaseline a where
  baselineOf :: a -> Baseline

-- | A collage of elements. Can be created from a single element with
-- 'collageSingleton' or from a combination of several subcollages with
-- relative offsets from a point with 'collageComposeN'. After a collage is
-- built, it can be converted to a non-empty list of elements coupled with
-- their absolute coordinates using the 'collageElements' function.
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
-- to the bottom-right corner.
--
data Collage a =
  Collage Margin Extents Baseline (CollageBuilder a)

instance HasExtents (Collage a) where
  extentsOf = collageExtents

instance HasBaseline (Collage a) where
  baselineOf = collageBaseline

-- | Get the bounding box of a collage in constant time.
collageExtents :: Collage a -> Extents
collageExtents (Collage _ e _ _) = e

-- | Get the margin of a collage in constant time.
collageMargin :: Collage a -> Margin
collageMargin (Collage m _ _ _) = m

-- | Get the baseline in constant time.
collageBaseline :: Collage a -> Baseline
collageBaseline (Collage _ _ l _) = l

collageBuilder :: Collage a -> CollageBuilder a
collageBuilder (Collage _ _ _ b) = b

-- | Get the width of a collage in constant time.
collageWidth :: Collage a -> Natural
collageWidth = extentsW . collageExtents

-- | Get the height of a collage in constant time.
collageHeight :: Collage a -> Natural
collageHeight = extentsH . collageExtents

-- | Set the margins of a collage to the pointwise maximum of their current
-- value and the specified new value. Taking the maximum ensures we do not
-- erase the margins computed from subcollages.
collageWithMargin :: Margin -> Collage a -> Collage a
collageWithMargin m' (Collage m e l b) =
  Collage (marginMax m' m) e l b

-- | Get a non-empty list of primitives with absolute positions and computed
-- extents, ordered by z-index (ascending).
--
-- O(n) - linear in the amount of elements.
--
-- The input offset is the position for the top-left corner of the collage.
collageElements ::
  Offset ->
  Collage a ->
  NonEmpty (Positioned a)
collageElements offset (Collage _ _ _ b) =
  fromDNonEmpty (buildCollage b offset)

-- Non-empty difference list
newtype DNonEmpty a = DNonEmpty ([a] -> NonEmpty a)

instance Semigroup (DNonEmpty a) where
  DNonEmpty f <> DNonEmpty g = DNonEmpty (f . NonEmpty.toList . g)

fromDNonEmpty :: DNonEmpty a -> NonEmpty a
fromDNonEmpty (DNonEmpty f) = f []

newtype CollageBuilder a =
  CollageBuilder { buildCollage :: Offset -> DNonEmpty (Positioned a) }

collageBuilderSingleton :: a -> CollageBuilder a
collageBuilderSingleton a =
  CollageBuilder { buildCollage = \offset -> DNonEmpty (At offset a :|) }

-- Elements are ordered by z-index (ascending)
collageBuilderCompose ::
  NonEmpty (Positioned (CollageBuilder a)) ->
  CollageBuilder a
collageBuilderCompose xs =
  CollageBuilder
    { buildCollage = \offset ->
        let
          toElements :: Positioned (CollageBuilder a) -> DNonEmpty (Positioned a)
          toElements (At o b) = buildCollage b (offsetAdd offset o)
        in
          foldMap_NonEmpty toElements xs
    }

-- | Construct a collage from a single element.
collageSingleton :: (HasExtents a, HasBaseline a) => a -> Collage a
collageSingleton a =
  Collage marginZero (extentsOf a) (baselineOf a) (collageBuilderSingleton a)

instance (HasExtents a, HasBaseline a, IsString a) => IsString (Collage a) where
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

-- reimplementation of 'foldMap1' with t~NonEmpty, to avoid
-- a dependency on 'semigroupoids'
foldMap_NonEmpty :: Semigroup s => (a -> s) -> NonEmpty a -> s
foldMap_NonEmpty f (s :| ss) = foldl' (\acc x -> acc <> f x) (f s) ss

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

data MarginPoints =
  MarginPoints
    Offset
    Offset

instance Semigroup MarginPoints where
  MarginPoints p1 q1 <> MarginPoints p2 q2 =
    MarginPoints (offsetMin p1 p2) (offsetMax q1 q2)

toMarginPoints :: Offset -> Extents -> Margin -> MarginPoints
toMarginPoints offset extents margin = MarginPoints p q
  where
    p = offsetAdd offset marginTopLeftOffset
    q = offsetAdd offset marginBottomRightOffset
    marginTopLeftOffset =
      Offset
        { offsetX = (negate . toInteger) (marginLeft margin),
          offsetY = (negate . toInteger) (marginTop margin) }
    marginBottomRightOffset =
      Offset
        { offsetX = toInteger (marginRight margin) + toInteger (extentsW extents),
          offsetY = toInteger (marginBottom margin) + toInteger (extentsH extents) }

fromMarginPoints :: Extents -> MarginPoints -> Margin
fromMarginPoints extents marginPoints =
  Margin
    { marginLeft = (intNatCeil . negate) (offsetX p),
      marginRight = intNatCeil (offsetX q - offsetX eOffset),
      marginTop = (intNatCeil . negate) (offsetY p),
      marginBottom = intNatCeil (offsetY q - offsetY eOffset) }
  where
    eOffset = extentsOffset extents
    MarginPoints p q = marginPoints

    intNatCeil :: Integer -> Natural
    intNatCeil = fromInteger . max 0

data CollageComposeAccum = CollageComposeAccum MarginPoints Extents Baseline Offset

instance Semigroup CollageComposeAccum where
  CollageComposeAccum mp1 e1 l1 o1 <> CollageComposeAccum mp2 e2 l2 o2 =
    CollageComposeAccum (mp1 <> mp2) (extentsMax e1 e2) (baselineMin l1 l2) (offsetMin o1 o2)

-- | A generalization of 'collageCompose' to take a non-empty list of
-- subcollages instead of a pair.
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
      Collage resultMargin resultExtents resultBaseline (collageBuilderCompose resultElements)

    resultMargin = fromMarginPoints resultExtents resultMarginPoints

    (CollageComposeAccum resultMarginPoints resultExtents resultBaseline minOffset, resultElements) =
      traverse1_NonEmpty_Writer processElement elements

    processElement ::
      Positioned (Collage a) ->
      (CollageComposeAccum, Positioned (CollageBuilder a))
    processElement (At offset collage) =
      let
        extents = collageExtents collage
        margin = collageMargin collage
        baseline = collageBaseline collage
        -- normalized offset, guaranteed to be non-negative
        offset' = offsetSub offset minOffset
        extents' = extentsWithOffset offset' extents
        baseline' = baselineWithOffset offset' baseline
        marginPoints = toMarginPoints offset' extents margin
        element' = At offset' (collageBuilder collage)
        acc = CollageComposeAccum marginPoints extents' baseline' offset
      in
        (acc, element')

instance Semigroup (Positioned (Collage a)) where
  a <> b = sconcat (a :| b : [])
  sconcat = collageComposeN

instance (HasExtents a, HasBaseline a, Inj p a) => Inj p (Collage a) where
  inj = collageSingleton . inj

-- | A decoration is an item in a collage does affect its layout. That is,
-- adding a decoration it has no effect on the extents, margins, or the
-- baseline of a collage.
data Decoration a =
  DecorationBelow a | DecorationAbove a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Add a decoration to a collage.
collageDecorate ::
  Decoration (Positioned (Collage a)) ->
  Collage a ->
  Collage a
collageDecorate d (Collage m e l b) =
  let
    toCB = fmap @Positioned collageBuilder
    bAt0 = At offsetZero b
    bs = case d of
      DecorationBelow d' -> toCB d' :| bAt0 : []
      DecorationAbove d' -> bAt0 :| toCB d' : []
    b' = collageBuilderCompose bs
  in
    Collage m e l b'

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
  inj = fmap inj

-- | Construct and inject an 'LRTB' value.
lrtb :: forall p a. Inj (LRTB p) a => p -> p -> p -> p -> a
lrtb l r t b = inj (LRTB l r t b)
