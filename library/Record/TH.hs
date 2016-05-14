{-# LANGUAGE CPP #-}
module Record.TH where

import BasePrelude hiding (Proxy)
import GHC.TypeLits
import Language.Haskell.TH hiding (classP)


classP :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classP n tl = foldl AppT (ConT n) tl
#else
classP = ClassP
#endif

recordTypeDec :: Bool -> Int -> Dec
recordTypeDec strict arity =
#if MIN_VERSION_template_haskell(2,11,0)  
  DataD [] name varBndrs Nothing [NormalC name conTypes] derivingNames
#else
  DataD [] name varBndrs [NormalC name conTypes] derivingNames
#endif
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
#if MIN_VERSION_template_haskell(2,11,0)          
          if strict then Bang NoSourceUnpackedness SourceStrict else Bang NoSourceUnpackedness NoSourceStrictness
#else
          if strict then IsStrict else NotStrict
#endif

    derivingNames =
#if MIN_VERSION_template_haskell(2,11,0)
      map ConT
#endif
      
#if MIN_VERSION_base(4,7,0)
      [''Show, ''Eq, ''Ord, ''Typeable, ''Generic]
#else
      [''Show, ''Eq, ''Ord, ''Generic]
#endif

recordName :: Bool -> Int -> Name
recordName strict arity =
  mkName $ recordNameString strict arity

recordNameString :: Bool -> Int -> String
recordNameString strict arity =
  prefix <> "Record" <> show arity
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
                                          (nameE "alignment")
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

recordConFunDecs :: Bool -> Int -> [Dec]
recordConFunDecs strict arity =
  [inline, signature, fun]
  where
    inline =
      PragmaD (InlineP name Inline FunLike AllPhases)
    signature =
      SigD name type_
      where
        type_ =
          ForallT varBndrs [] $
          foldr AppT recordType $
          map (AppT ArrowT) $
          interleave nameProxyTypes valueVariableTypes
          where
            varBndrs =
              map PlainTV $
              interleave nameVariableNames valueVariableNames
            recordType =
              foldl' AppT (ConT (recordName strict arity)) $ 
              interleave nameVariableTypes valueVariableTypes
            valueVariableTypes =
              map VarT valueVariableNames
            valueVariableNames =
              map (\i -> mkName ("v" <> show i)) [1 .. arity]
            nameVariableTypes =
              map VarT nameVariableNames
            nameVariableNames =
              map (\i -> mkName ("n" <> show i)) [1 .. arity]
            nameProxyTypes =
              map (AppT (ConT (mkName "FieldName"))) nameVariableTypes
            interleave a b =
              join $ zipWith (\a b -> [a, b]) a b
    fun =
      FunD name [Clause [] (NormalB (recordConLambdaExp strict arity)) []]
    name =
      mkName string
      where
        string =
          onHead toLower (recordNameString strict arity)
          where
            onHead f =
              \case
                a : b -> f a : b
                [] -> []

-- |
-- Allows to specify field names at value-level.
-- Useful for type-inference.
-- 
-- E.g., in
-- 
-- >(\_ v1 _ v2 -> StrictRecord2 v1 v2) :: Types.FieldName n1 -> v1 -> Types.FieldName n2 -> v2 -> StrictRecord2 n1 v1 n2 v2
-- 
-- we can set the name signatures by passing
-- the name-proxies to this lambda.
recordConLambdaExp :: Bool -> Int -> Exp
recordConLambdaExp strict arity =
  SigE exp t
  where
    name =
      recordName strict arity
    exp =
      LamE pats exp
      where
        pats =
          concat $ flip map [1 .. arity] $ \i -> [WildP, VarP (mkName ("v" <> show i))]
        exp =
          foldl AppE (ConE name) (map (\i -> VarE (mkName ("v" <> show i))) [1 .. arity])
    t =
      fnType name
      where
        fnType conName =
          ForallT varBndrs [] $
          foldr1 (\l r -> AppT (AppT ArrowT l) r)
                 (argTypes <> pure (resultType conName))
        varBndrs =
          concat $ flip map [1 .. arity] $ \i ->
            PlainTV (mkName ("n" <> show i)) :
            PlainTV (mkName ("v" <> show i)) :
            []
        argTypes =
          concat $ flip map [1 .. arity] $ \i -> 
            AppT (ConT (mkName "FieldName")) (VarT (mkName ("n" <> show i))) :
            VarT (mkName ("v" <> show i)) :
            []
        resultType conName =
          foldl AppT (ConT conName) $ concat $ flip map [1 .. arity] $ \i ->
            VarT (mkName ("n" <> show i)) :
            VarT (mkName ("v" <> show i)) :
            []

