-- |
-- A minimal subset of the "lens" functionality,
-- which is completely compatible with it.
module Record.Lens where

import BasePrelude
import Data.Functor.Identity


type Lens s a = 
  forall f. Functor f => (a -> f a) -> (s -> f s)

view :: Lens s a -> s -> a
view l =
  getConst . l Const

set :: Lens s a -> a -> s -> s
set l a =
  runIdentity . l (Identity . const a)

over :: Lens s a -> (a -> a) -> s -> s
over l f =
  runIdentity . l (Identity . f)
