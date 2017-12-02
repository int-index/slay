{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Slay.Number.Double
  ( Signed
  , Unsigned
  , unsafeToUnsigned
  , toSigned
  ) where

import Control.Exception

type Signed = Double
type Unsigned = NonNegative

unsafeToUnsigned :: Double -> NonNegative
unsafeToUnsigned = unsafeToNonNegative

toSigned :: NonNegative -> Double
toSigned = getNonNegative

newtype NonNegative = NonNegative { getNonNegative :: Double }
  deriving (Eq, Ord, Show, Real)

unsafeToNonNegative :: Double -> NonNegative
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

instance Fractional NonNegative where
  NonNegative a / NonNegative b = NonNegative (a / b)
  recip (NonNegative a) = NonNegative (recip a)
  fromRational = unsafeToNonNegative . fromRational
