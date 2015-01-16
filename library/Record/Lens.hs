-- |
-- A minimal subset of the "lens" functionality,
-- which is completely compatible with it.
module Record.Lens where

import BasePrelude


type Lens s a = 
  forall f. Functor f => (a -> f a) -> (s -> f s)



