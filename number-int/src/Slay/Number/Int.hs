{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, UndecidableInstances #-}

module Slay.Number.Int
  ( Signed
  , Unsigned
  , unsafeToUnsigned
  , toSigned
  , ceil
  ) where

import Control.Exception
import Inj

type Signed = Int
type Unsigned = NonNegative

instance Inj p Int => Inj p NonNegative where
  inj = unsafeToUnsigned . inj

unsafeToUnsigned :: Int -> NonNegative
unsafeToUnsigned = unsafeToNonNegative

toSigned :: NonNegative -> Int
toSigned = getNonNegative

newtype NonNegative = NonNegative { getNonNegative :: Int }
  deriving (Eq, Ord, Show, Real, Enum, Integral)

unsafeToNonNegative :: Int -> NonNegative
unsafeToNonNegative d =
  if d >= 0 then NonNegative d else throw Underflow

instance Num NonNegative where
  NonNegative a + NonNegative b = NonNegative (a + b)
  NonNegative a - NonNegative b = unsafeToNonNegative (a - b)
  NonNegative a * NonNegative b = NonNegative (a * b)
  negate _ = throw Underflow
  abs = id
  signum (NonNegative a) = NonNegative (signum a)
  fromInteger = unsafeToNonNegative . fromInteger

instance Bounded NonNegative where
  minBound = NonNegative 0
  maxBound = NonNegative maxBound

ceil :: Integral i => NonNegative -> i
ceil = fromIntegral . getNonNegative
