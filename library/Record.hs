module Record
(
  record,
  lens,
  -- ** Shorthands
  r,
  l,
)
where

import BasePrelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Record.Types as Types
import qualified Record.Lens as Lens
import qualified Record.Parser as Parser
import qualified Data.Text as T


-- | A shorthand alias to 'record'.
r :: QuasiQuoter
r = record

-- | A shorthand alias to 'lens'.
l :: QuasiQuoter
l = lens

-- |
-- A quasiquoter, which generates record expressions and types,
-- depending on the context it's used in.
-- 
-- Here is how you can use it to declare types:
-- 
-- >type Person = 
-- >  [record| {name :: String, birthday :: {year :: Int, month :: Int, day :: Int}} |]
-- 
-- To declare functions:
-- 
-- >getAge :: [record| {name :: String, age :: Int} |] -> Int
-- 
-- To declare values:
-- 
-- >person :: Person
-- >person =
-- >  [record| {name = "Grigori Yakovlevich Perelman", 
-- >            birthday = {year = 1966, month = 6, day = 13}} |]
-- 
record :: QuasiQuoter
record =
  QuasiQuoter
    (exp)
    (const $ fail "Pattern context is not supported")
    (type')
    (const $ fail "Declaration context is not supported")
  where
    exp =
      join . fmap (either fail return . renderExp) .
      either (fail . showString "Parser failure: ") return .
      Parser.run (Parser.qq Parser.exp) . fromString
    type' =
      join . fmap (either fail return . renderType) .
      either (fail . showString "Parser failure: ") return .
      Parser.run (Parser.qq Parser.type') . fromString

-- |
-- A quasiquoter, which generates a 'Lens.Lens'.
-- Lens is your interface to accessing and modifying the fields of a record.
-- 
-- Here is how you can use it:
-- 
-- >getPersonBirthdayYear :: Person -> Int
-- >getPersonBirthdayYear =
-- >  Record.Lens.view ([lens|birthday|] . [lens|year|])
-- 
-- For your convenience you can compose lenses from inside of the quotation:
-- 
-- >setPersonBirthdayYear :: Int -> Person -> Person
-- >setPersonBirthdayYear =
-- >  Record.Lens.set [lens|birthday.year|]
-- 
-- You can also use this function to manipulate tuples of arity up to 24:
-- 
-- >mapThirdElement :: (Char -> Char) -> (Int, String, Char) -> (Int, String, Char)
-- >mapThirdElement =
-- >  Record.Lens.over [lens|3|]
lens :: QuasiQuoter
lens =
  QuasiQuoter
    (exp)
    (const $ fail "Pattern context is not supported")
    (const $ fail "Type context is not supported")
    (const $ fail "Declaration context is not supported")
  where
    exp =
      either (fail . showString "Parser failure: ") return .
      fmap renderLens .
      Parser.run (Parser.qq Parser.lens) . fromString

renderLens :: Parser.Lens -> Exp
renderLens =
  foldl1 composition .
  fmap renderSingleLens
  where
    composition a b = 
      UInfixE a (VarE '(.)) b

renderSingleLens :: T.Text -> Exp
renderSingleLens =
  AppE (VarE 'Types.fieldLens) .
  SigE (ConE 'Types.Field) .
  AppT (ConT ''Types.Field) .
  LitT . StrTyLit . T.unpack

renderRecordType :: Parser.RecordType -> Either String Type
renderRecordType l =
  checkDuplicateLabels >> getRecordTypeName >>= constructType
  where
    checkDuplicateLabels =
      maybe (return ()) (Left . showString "Duplicate labels: " . show) $
      mfilter (not . null) . Just $ 
      map (fst . head) $
      filter ((> 1) . length) $
      groupWith fst l
    getRecordTypeName =
      maybe (Left (showString "Record arity " . shows arity . shows " is not supported" $ ""))
            (Right) $
      recordTypeNameByArity arity
      where
        arity = length l
    constructType n =
      foldl (\a (l, t) -> AppT <$> (AppT <$> a <*> pure (textLitT l)) <*> (renderType t))
            (pure (ConT n))
            (sortWith fst l)
      where
        textLitT =
          LitT . StrTyLit . T.unpack

recordTypeNameByArity :: Int -> Maybe Name
recordTypeNameByArity arity =
  fmap head $ mfilter (not . null) $ Just $
  drop (pred arity) $
  [
    ''Types.Record1, ''Types.Record2, ''Types.Record3, ''Types.Record4, 
    ''Types.Record5, ''Types.Record6, ''Types.Record7, ''Types.Record8, 
    ''Types.Record9, ''Types.Record10, ''Types.Record11, ''Types.Record12, 
    ''Types.Record12, ''Types.Record13, ''Types.Record14, ''Types.Record15, 
    ''Types.Record16, ''Types.Record17, ''Types.Record18, ''Types.Record19, 
    ''Types.Record20, ''Types.Record21, ''Types.Record22, ''Types.Record22, 
    ''Types.Record23, ''Types.Record24
  ]

recordConNameByArity :: Int -> Maybe Name
recordConNameByArity arity =
  fmap head $ mfilter (not . null) $ Just $
  drop (pred arity) $
  [
    'Types.Record1, 'Types.Record2, 'Types.Record3, 'Types.Record4, 
    'Types.Record5, 'Types.Record6, 'Types.Record7, 'Types.Record8, 
    'Types.Record9, 'Types.Record10, 'Types.Record11, 'Types.Record12, 
    'Types.Record12, 'Types.Record13, 'Types.Record14, 'Types.Record15, 
    'Types.Record16, 'Types.Record17, 'Types.Record18, 'Types.Record19, 
    'Types.Record20, 'Types.Record21, 'Types.Record22, 'Types.Record22, 
    'Types.Record23, 'Types.Record24
  ]

renderType :: Parser.Type -> Either String Type
renderType =
  \case
    Parser.Type_App a b  -> AppT <$> renderType a <*> renderType b
    Parser.Type_Var n    -> return $ VarT (mkName (T.unpack n))
    Parser.Type_Con n    -> return $ ConT (mkName (T.unpack n))
    Parser.Type_Tuple a  -> return $ TupleT a
    Parser.Type_Arrow    -> return $ ArrowT
    Parser.Type_List     -> return $ ListT
    Parser.Type_Record a -> renderRecordType a

renderExp :: Parser.Exp -> Either String Exp
renderExp =
  \case
    Parser.Exp_Record r   -> renderRecordExp r
    Parser.Exp_Var n      -> return $ VarE (mkName (T.unpack n))
    Parser.Exp_Con n      -> return $ ConE (mkName (T.unpack n))
    Parser.Exp_TupleCon a -> return $ ConE (tupleDataName a)
    Parser.Exp_Nil        -> return $ ConE ('[])
    Parser.Exp_Lit l      -> return $ LitE (renderLit l)
    Parser.Exp_App a b    -> AppE <$> renderExp a <*> renderExp b
    Parser.Exp_List l     -> ListE <$> traverse renderExp l
    Parser.Exp_Sig e t    -> SigE <$> renderExp e <*> renderType t

renderRecordExp :: Parser.RecordExp -> Either String Exp
renderRecordExp l =
  checkDuplicateLabels >> getConLambda >>= constructExp
  where
    checkDuplicateLabels =
      maybe (return ()) (Left . showString "Duplicate labels: " . show) $
      mfilter (not . null) . Just $ 
      map (fst . head) $
      filter ((> 1) . length) $
      groupWith fst l
    getConLambda =
      maybe (Left (showString "Record arity " . shows arity . shows " is not supported" $ ""))
            (Right) $
      conLambdaExp arity
      where
        arity = length l
    constructExp lam =
      foldl (\a (n, e) -> AppE <$> (AppE <$> a <*> pure (proxy n)) <*> renderExp e)
            (pure lam)
            (sortWith fst l)
      where
        proxy n =
          SigE (ConE 'Types.Field) 
               (AppT (ConT ''Types.Field) (LitT (StrTyLit (T.unpack n))))

renderLit :: Parser.Lit -> Lit
renderLit =
  \case
    Parser.Lit_Char c -> CharL c
    Parser.Lit_String t -> StringL (T.unpack t)
    Parser.Lit_Integer i -> IntegerL i
    Parser.Lit_Rational r -> RationalL r

-- |
-- Allows to specify names in types signatures,
-- leaving the value type resolution to the compiler.
-- 
-- E.g.,
-- 
-- >(\_ v1 _ v2 -> Record2 v1 v2) :: Types.Field n1 -> v1 -> Types.Field n2 -> v2 -> Record2 n1 v1 n2 v2
-- 
-- We can set the name signatures by passing
-- proxies with explicit signatures to this lambda.
conLambdaExp :: Int -> Maybe Exp
conLambdaExp arity =
  SigE <$> exp <*> t
  where
    exp =
      LamE <$> pure pats <*> exp
      where
        pats =
          concat $ flip map [1 .. arity] $ \i -> [WildP, VarP (mkName ("v" <> show i))]
        exp =
          foldl AppE <$> (ConE <$> recordConNameByArity arity) <*> 
                         pure (map (\i -> VarE (mkName ("v" <> show i))) [1 .. arity])
    t =
      fnType <$> recordTypeNameByArity arity
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
            AppT (ConT ''Types.Field) (VarT (mkName ("n" <> show i))) :
            VarT (mkName ("v" <> show i)) :
            []
        resultType conName =
          foldl AppT (ConT conName) $ concat $ flip map [1 .. arity] $ \i ->
            VarT (mkName ("n" <> show i)) :
            VarT (mkName ("v" <> show i)) :
            []


