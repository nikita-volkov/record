module Record.Types where

import BasePrelude
import GHC.TypeLits
import Record.Lens (Lens)


class FieldOwner (n :: Symbol) v a | n a -> v where
  setField :: Proxy n -> v -> a -> a
  getField :: Proxy n -> a -> v

lens :: FieldOwner n v a => Proxy n -> Lens a v
lens n =
  \f a -> fmap (\v -> setField n v a) (f (getField n a))


instance FieldOwner "_1" v1 (v1, v2) where
  setField _ v (v1, v2) = (v, v2)
  getField _ (v, _) = v


data Record1 (n1 :: Symbol) v1 =
  Record1 v1

data Record2 (n1 :: Symbol) v1 (n2 :: Symbol) v2 =
  Record2 v1 v2

data Record3 (n1 :: Symbol) v1 (n2 :: Symbol) v2 (n3 :: Symbol) v3 =
  Record3 v1 v2 v3

instance FieldOwner n1 v1 (Record2 n1 v1 n2 v2) where
  setField _ v (Record2 v1 v2) = Record2 v v2
  getField _ (Record2 v1 v2) = v1

