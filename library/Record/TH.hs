{-# LANGUAGE CPP #-}
module Record.TH where

import BasePrelude hiding (Proxy)
import Data.Functor.Identity
import GHC.TypeLits
import Foreign.Storable
import Foreign.Ptr (plusPtr)
import Language.Haskell.TH hiding (classP)


classP :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classP n tl = foldl AppT (ConT n) tl
#else
classP = ClassP
#endif

recordTypeDec :: Bool -> Int -> Dec
recordTypeDec strict arity =
  DataD [] name varBndrs [NormalC name conTypes] []
  where
    name =
      recordName strict arity
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
        return $ (,) strictness (VarT (mkName ("v" <> show i)))
      where
        strictness =
          if strict then IsStrict else NotStrict

recordName :: Bool -> Int -> Name
recordName strict arity =
  mkName $ prefix <> "Record" <> show arity
  where
    prefix =
      if strict then "Strict" else "Lazy"

recordFieldInstanceDec :: Bool -> Int -> Int -> Dec
recordFieldInstanceDec strict =
  fieldInstanceDec (FieldInstanceDecMode_Record strict)

tupleFieldInstanceDec :: Int -> Int -> Dec
tupleFieldInstanceDec arity fieldIndex =
  fieldInstanceDec FieldInstanceDecMode_Tuple arity fieldIndex

data FieldInstanceDecMode =
  FieldInstanceDecMode_Tuple |
  FieldInstanceDecMode_Record Bool

fieldInstanceDec :: FieldInstanceDecMode -> Int -> Int -> Dec
fieldInstanceDec mode arity fieldIndex =
  InstanceD [] headType decs
  where
    headType =
      foldl1 AppT 
      [classType, VarT selectedNVarName, recordType, recordPrimeType, VarT selectedVVarName, VarT selectedVPrimeVarName]
      where
        classType =
          ConT (mkName "Field")
    typeName =
      case mode of
        FieldInstanceDecMode_Tuple -> tupleTypeName arity
        FieldInstanceDecMode_Record strict -> recordName strict arity
    conName =
      case mode of
        FieldInstanceDecMode_Tuple -> tupleDataName arity
        FieldInstanceDecMode_Record strict -> recordName strict arity
    selectedNVarName =
      mkName $ "n" <> show fieldIndex
    selectedVVarName =
      mkName $ "v" <> show fieldIndex
    selectedVPrimeVarName =
      mkName $ "v" <> show fieldIndex <> "'"
    recordType =
      foldl (\a i -> AppT (addNVar a i)
                          (VarT (mkName ("v" <> show i))))
            (ConT typeName)
            [1 .. arity]
    recordPrimeType =
      foldl (\a i -> AppT (addNVar a i)
                          (VarT (if i == fieldIndex then selectedVPrimeVarName
                                                    else mkName ("v" <> show i))))
            (ConT typeName)
            [1 .. arity]
    addNVar =
      case mode of
        FieldInstanceDecMode_Tuple -> \a i -> a
        FieldInstanceDecMode_Record _ -> \a i -> AppT a (VarT (mkName ("n" <> show i)))
    decs =
      [fieldLensDec]
      where
        fieldLensDec =
          FunD (mkName "fieldLens") [Clause patterns (NormalB exp) []]
          where
            patterns =
              [WildP, VarP fVarName, ConP conName (fmap VarP indexedVVarNames)]
            fVarName =
              mkName "f"
            indexedVVarNames =
              fmap (\i -> mkName ("v" <> show i)) [1..arity]
            exp =
              AppE (AppE (VarE 'fmap) (consLambda))
                   (AppE (VarE fVarName) (VarE selectedVVarName))
              where
                consLambda =
                  LamE [VarP selectedVPrimeVarName] exp
                  where
                    exp =
                      foldl AppE (ConE conName) $
                      map VarE $
                      map (\(i, n) -> if i == fieldIndex then selectedVPrimeVarName
                                                         else mkName ("v" <> show i)) $
                      zip [1 .. arity] indexedVVarNames

recordStorableInstanceDec :: Bool -> Int -> Dec
recordStorableInstanceDec strict arity =
  InstanceD context (AppT (ConT (mkName "Storable")) recordType)
            [sizeOfFun, inlineFun "sizeOf", alignmentFun, inlineFun "alignment"
            , peekFun, inlineFun "peek", pokeFun, inlineFun "poke"]
  where
    name = recordName strict arity
    recordType =
      foldl (\a i -> AppT (AppT a (VarT (mkName ("n" <> show i))))
                          (VarT (mkName ("v" <> show i))))
            (ConT name)
            [1 .. arity]
    context = map (\i -> classP (mkName "Storable")  [VarT (mkName ("v" <> show i))])
              [1 .. arity]
    nameE = VarE . mkName
    -- The sum of the sizes of all types
    sizeOfFun' n = foldr (\a b -> AppE (AppE (nameE "+") a) b) (LitE (IntegerL 0)) $
                   map (\i -> AppE
                              (nameE "sizeOf")
                              (SigE (nameE "undefined")
                                    (VarT (mkName ("v" <> show i)))))
                   [1..n]
    sizeOfFun = FunD (mkName "sizeOf")
                [Clause [WildP]
                 (NormalB (sizeOfFun' arity)) []]
    -- Set the alignment to the maximum alignment of the types
    alignmentFun = FunD (mkName "alignment")
                   [(Clause [WildP]
                     (NormalB (AppE (nameE "maximum") $ ListE $
                               map (\i -> AppE
                                          (nameE "sizeOf")
                                          (SigE (nameE "undefined")
                                                (VarT (mkName ("v" <> show i)))))
                               [1..arity])) [])]
    -- Peek every variable, remember to add the size of the elements already seen to the ptr
    peekFun = FunD (mkName "peek")
              [(Clause [VarP (mkName "ptr")]
                  (NormalB (DoE $ map (\i -> BindS
                                             (BangP (VarP (mkName ("x" <> show i))))
                                                    (AppE (nameE "peek")
                                                          (AppE (AppE (nameE "plusPtr")
                                                                      (nameE "ptr"))
                                                                (sizeOfFun' (i - 1))))) [1..arity]
                                 ++ [NoBindS (AppE (nameE "return")
                                             (foldl (\a i -> AppE a (nameE ("x" <> show i)))
                                             (ConE name) [1 .. arity]))])) [])]
    typePattern = ConP name (map (\i -> VarP (mkName ("v" <> show i))) [1..arity])
    pokeFun = FunD (mkName "poke")
              [(Clause [VarP (mkName "ptr"), typePattern]
                 (NormalB (DoE $ map (\i -> NoBindS
                                            (AppE
                                             (AppE (VarE (mkName "poke"))
                                                   (AppE (AppE (nameE "plusPtr")
                                                                 (nameE "ptr"))
                                                          (sizeOfFun' (i - 1))))
                                             (nameE ("v" <> show i)))) [1..arity])) [])]
    inlineFun name = PragmaD $ InlineP (mkName name) Inline FunLike AllPhases
