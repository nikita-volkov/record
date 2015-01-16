module Record.Field where

import BasePrelude
import GHC.TypeLits
import Record.Lens (Lens)


newtype Field (n :: Symbol) a = Field a

class Owner n a where
  type Value n a
  setField :: Proxy n -> Value n a -> a -> a
  getField :: Proxy n -> a -> Value n a

lens :: Owner n a => Proxy n -> Lens a (Value n a)
lens n =
  \f a -> fmap (\v -> setField n v a) (f (getField n a))
