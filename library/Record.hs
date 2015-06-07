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
-- A specialised version of "Data.Proxy.Proxy".
-- Defined for compatibility with \"base-4.6\",
-- since @Proxy@ was only defined in \"base-4.7\".
data FieldName (t :: Symbol)

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
class Field (n :: Symbol) a a' v v' | n a -> v, n a' -> v', n a v' -> a', n a' v -> a where
  fieldLens :: FieldName n -> Lens a a' v v'

instance Field "1" (Identity v1) (Identity v1') v1 v1' where
  fieldLens = const $ \f -> fmap Identity . f . runIdentity

-- Generate the tuple instances of the Field class:
return $ do
  arity <- [2 .. 24]
  fieldIndex <- [1 .. arity]
  return $ TH.tupleFieldInstanceDec arity fieldIndex

-- |
-- A simplified field constraint,
-- which excludes the possibility of type-changing updates.
type Field' n a v =
  Field n a a v v


-- * Record types and instances
-------------------------

-- Generate the record types and instances:
return $ do
  arity <- [1 .. 24]
  strict <- [False, True]
  let
    recordType =
      TH.recordTypeDec strict arity
    recordFieldInstances =
      do
        fieldIndex <- [1 .. arity]
        return $ TH.recordFieldInstanceDec strict arity fieldIndex
    storableInstance =
      TH.recordStorableInstanceDec strict arity
    in recordType : storableInstance : recordFieldInstances
