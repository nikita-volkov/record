{-# LANGUAGE CPP, UndecidableInstances #-}
-- |
-- The contents of this module may seem a bit overwhelming.
-- Don't worry,
-- all it does is just cover instances and datatypes of records and tuples of
-- huge arities.
--
-- You don't actually need to ever use this module,
-- since all the functionality you may need is presented
-- by the quasiquoters exported in the root module.
module Record where

import BasePrelude hiding (Proxy)
import Data.Functor.Identity
import GHC.TypeLits
import Foreign.Storable
import Foreign.Ptr (plusPtr)
import Control.Lens.Basic
import qualified Record.TH as TH


-- |
-- Defines a lens to manipulate some value of a type by a type-level name,
-- using the string type literal functionality.
--
-- Instances are provided for all records and for tuples of arity of up to 24.
--
-- Here's how you can use it with tuples:
--
-- >trd :: Field "3" a a' v v' => a -> v
-- >trd = view (fieldLens (undefined :: FieldName "3"))
-- 
-- The function above will get you the third item of any tuple, which has it.
class Field (n :: Symbol) a a' v v' | n a -> v, n a' -> v', n a v' -> a', n a' v -> a, n a v -> v' where
  setField :: FieldName n -> v' -> a -> a'
  getField :: FieldName n -> a -> v

-- |
-- A simplified field constraint,
-- which excludes the possibility of type-changing updates.
type Field' n a v =
  Field n a a v v

-- |
-- A specialised version of "Data.Proxy.Proxy".
-- Defined for compatibility with \"base-4.6\",
-- since @Proxy@ was only defined in \"base-4.7\".
data FieldName (t :: Symbol)

fieldLens :: Field n a a' v v' => FieldName n -> Lens a a' v v'
fieldLens n =
  \f a -> fmap (\v -> setField n v a) (f (getField n a))

-- Generate Record types
return $ TH.recordTypeDec <$> [False, True] <*> [1 .. 42]
