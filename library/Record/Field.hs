module Record.Field where

import BasePrelude
import GHC.TypeLits
import Record.Lens (Lens)


class Owner (n :: Symbol) v a | n a -> v where
  setField :: Proxy n -> v -> a -> a
  getField :: Proxy n -> a -> v

lens :: Owner n v a => Proxy n -> Lens a v
lens n =
  \f a -> fmap (\v -> setField n v a) (f (getField n a))


newtype Field (n :: Symbol) v = Field v

instance Owner n v (Field n v) where
  setField _ v _ = Field v
  getField _ (Field v) = v

instance Owner n1 v1 (Field n1 v1, Field n2 v2) where
  setField _ v (f1, f2) = (Field v, f2)
  getField _ (Field v, _) = v

instance Owner n2 v2 (Field n1 v1, Field n2 v2) where
  setField _ v (f1, f2) = (f1, Field v)
  getField _ (_, Field v) = v
