{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Slay.Number.Int
  ( Signed
  , Unsigned
  , unsafeToUnsigned
  , toSigned
  ) where

import Control.Exception

type Signed = Int
type Unsigned = NonNegative

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