module Slay.Number.Integer where

import Numeric.Natural

type Signed = Integer
type Unsigned = Natural

unsafeToUnsigned :: Integer -> Natural
unsafeToUnsigned = fromInteger

toSigned :: Natural -> Integer
toSigned = toInteger
