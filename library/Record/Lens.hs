-- |
-- A minimal subset of the "lens" library functionality,
-- which is completely compatible with it.
-- 
-- Unless you use the "lens" library itself, 
-- this module is your interface to manipulating the record fields.
module Record.Lens where

import BasePrelude
import Data.Functor.Identity


-- |
-- A reference to from a datastructure @s@ to some part @a@,
-- which can be used to manipulate that particular part.
type Lens s a = 
  forall f. Functor f => (a -> f a) -> (s -> f s)

-- |
-- Given a lens to a subpart and a datastructure,
-- produce the value of the referred subpart.
view :: Lens s a -> s -> a
view l =
  getConst . l Const

-- |
-- Given a lens to a subpart, a new value for it and a datastructure,
-- produce an updated datastructure.
set :: Lens s a -> a -> s -> s
set l a =
  runIdentity . l (Identity . const a)

-- |
-- Given a lens to a subpart, and an update function for it,
-- produce a function, which updates the datastructure.
over :: Lens s a -> (a -> a) -> s -> s
over l f =
  runIdentity . l (Identity . f)
