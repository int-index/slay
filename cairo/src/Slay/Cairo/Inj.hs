module Slay.Cairo.Inj
  ( Inj(..)
  ) where

import Slay.Core

class Inj p a where
  inj :: p -> a

instance Inj a a where
  inj = id

instance Inj p a => Inj p (Maybe a) where
  inj = Just . inj

instance (s -/ e, Inj p e) => Inj p (Collage s) where
  inj = collageSingleton . inj

