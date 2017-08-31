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
  deriving (Eq, Ord, Show, Num, Real)

unsafeToNonNegative :: Double -> NonNegative
unsafeToNonNegative d =
  if d >= 0 then NonNegative d else throw Underflow
