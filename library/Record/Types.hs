{-# LANGUAGE CPP #-}
-- |
-- The contents of this module may seem a bit overwhelming. 
-- Don't worry,
-- all it does is just cover instances and datatypes of records and tuples of
-- huge arities.
-- 
-- You don't actually need to ever use this module,
-- since all the functionality you may need is presented 
-- by the quasiquoters exported in the root module.
module Record.Types where

import BasePrelude hiding (Proxy)
import Data.Functor.Identity
import GHC.TypeLits
import Record.Lens (Lens)
import Language.Haskell.TH


-- |
-- Defines a way to access some value of a type as field,
-- using the string type literal functionality.
-- 
-- Instances are provided to all records and to tuples of arity of up to 24.
-- 
-- Here's how you can use it with tuples:
-- 
-- >trd :: FieldOwner "3" v a => a -> v
-- >trd = getField (Field :: Field "3")
-- 
-- The function above will get you the third item of any tuple, which has it.
class FieldOwner (n :: Symbol) v a | n a -> v where
  setField :: Field n -> v -> a -> a
  getField :: Field n -> a -> v

-- |
-- Generate a lens using the 'FieldOwner' instance.
-- 
-- >ageLens :: FieldOwner "age" v a => Lens a v
-- >ageLens = lens (Field :: Field "age") 
lens :: FieldOwner n v a => Field n -> Lens a v
lens n =
  \f a -> fmap (\v -> setField n v a) (f (getField n a))

-- |
-- A specialised version of "Data.Proxy.Proxy".
-- Defined for compatibility with \"base-4.6\", 
-- since @Proxy@ was only defined in \"base-4.7\".
data Field (t :: Symbol) = Field

-- * Record Types
-------------------------

-- Generate Record types
return $ flip map [1 .. 24] $ \arity ->
  let
    typeName =
      mkName $ "Record" <> show arity
    varBndrs =
      do
        i <- [1 .. arity]
        let
          n = KindedTV (mkName ("n" <> show i)) (ConT ''Symbol)
          v = PlainTV (mkName ("v" <> show i))
          in [n, v]
    conTypes =
      do
        i <- [1 .. arity]
        return $ (,) (NotStrict) (VarT (mkName ("v" <> show i)))
    derivingNames =
#if MIN_VERSION_base(4,7,0)
      [''Show, ''Eq, ''Ord, ''Typeable, ''Generic]
#else
      [''Show, ''Eq, ''Ord, ''Generic]
#endif
    in
      DataD [] typeName varBndrs [NormalC typeName conTypes] derivingNames


-- Generate Record FieldOwner instances
return $ do
  arity <- [1 .. 24]
  nIndex <- [1 .. arity]
  return $
    let
      typeName =
        mkName $ "Record" <> show arity
      selectedNVarName =
        mkName $ "n" <> show nIndex
      selectedVVarName =
        mkName $ "v" <> show nIndex
      recordType =
        foldl (\a i -> AppT (AppT a (VarT (mkName ("n" <> show i))))
                            (VarT (mkName ("v" <> show i))))
              (ConT typeName)
              [1 .. arity]
      setFieldLambda =
        LamE [VarP vVarName, ConP typeName (fmap VarP indexedVVarNames)] exp
        where
          vVarName =
            mkName "v"
          indexedVVarNames =
            fmap (\i -> mkName ("v" <> show i)) [1..arity]
          exp =
            foldl (\a i -> AppE a (VarE (mkName (if i == nIndex then "v" else "v" <> show i))))
                  (ConE typeName)
                  [1 .. arity]
      getFieldLambda =
        LamE [ConP typeName vPatterns] (VarE (vVarName))
        where
          vVarName =
            mkName "v"
          vPatterns =
            flip map [1 .. arity] $ \i ->
              if i == nIndex
                then VarP vVarName
                else WildP
      in
        head $ unsafePerformIO $ runQ $
        [d|
          instance FieldOwner $(varT selectedNVarName)
                              $(varT selectedVVarName)
                              $(pure recordType)
                              where
            setField = const $ $(pure setFieldLambda)
            getField = const $ $(pure getFieldLambda)
        |]

        
instance FieldOwner "1" v1 (Identity v1) where
  setField _ v _ = Identity v
  getField _ = runIdentity


-- Generate FieldOwner instances for tuples
return $ do
  arity <- [2 .. 24]
  nIndex <- [1 .. arity]
  return $
    let
      typeName =
        tupleTypeName arity
      conName =
        tupleDataName arity
      selectedVVarName =
        mkName $ "v" <> show nIndex
      tupleType =
        foldl (\a i -> AppT a (VarT (mkName ("v" <> show i))))
              (ConT typeName)
              [1 .. arity]
      setFieldLambda =
        LamE [VarP vVarName, ConP conName (fmap VarP indexedVVarNames)] exp
        where
          vVarName =
            mkName "v"
          indexedVVarNames =
            fmap (\i -> mkName ("v" <> show i)) [1..arity]
          exp =
            foldl (\a i -> AppE a (VarE (mkName (if i == nIndex then "v" else "v" <> show i))))
                  (ConE conName)
                  [1 .. arity]
      getFieldLambda =
        LamE [ConP conName vPatterns] (VarE (vVarName))
        where
          vVarName =
            mkName "v"
          vPatterns =
            flip map [1 .. arity] $ \i ->
              if i == nIndex
                then VarP vVarName
                else WildP
      in
        head $ unsafePerformIO $ runQ $
        [d|
          instance FieldOwner $(pure (LitT (StrTyLit (show nIndex))))
                              $(varT selectedVVarName)
                              $(pure tupleType)
                              where
            setField = const $ $(pure setFieldLambda)
            getField = const $ $(pure getFieldLambda)
        |]
