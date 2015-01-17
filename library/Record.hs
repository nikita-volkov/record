module Record where

import BasePrelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Record.Types as Types
import qualified Record.Lens as Lens
import qualified Record.Parser as Parser
import qualified Data.Text as T


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
  AppE (VarE 'Types.lens) .
  SigE (ConE 'Proxy) .
  AppT (ConT ''Proxy) .
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
recordTypeNameByArity =
  \case
    1 -> Just ''Types.Record1
    2 -> Just ''Types.Record2
    3 -> Just ''Types.Record3
    _ -> Nothing

recordConNameByArity :: Int -> Maybe Name
recordConNameByArity =
  \case
    1 -> Just 'Types.Record1
    2 -> Just 'Types.Record2
    3 -> Just 'Types.Record3
    _ -> Nothing
  
renderType :: Parser.Type -> Either String Type
renderType =
  \case
    Parser.Type_App a b  -> AppT <$> renderType a <*> renderType b
    Parser.Type_Var n    -> Right $ VarT (mkName (T.unpack n))
    Parser.Type_Con n    -> Right $ ConT (mkName (T.unpack n))
    Parser.Type_Tuple a  -> Right $ TupleT a
    Parser.Type_Arrow    -> Right $ ArrowT
    Parser.Type_List     -> Right $ ListT
    Parser.Type_Record a -> renderRecordType a

renderExp :: Parser.Exp -> Either String Exp
renderExp =
  \case
    Parser.Exp_Record r -> renderRecordExp r
    Parser.Exp_Var n -> return $ VarE (mkName (T.unpack n))
    Parser.Exp_Con n -> return $ ConE (mkName (T.unpack n))
    Parser.Exp_TupleCon a -> return $ ConE (tupleDataName a)
    Parser.Exp_Nil -> return $ ConE ('[])
    Parser.Exp_Lit l -> return $ LitE (renderLit l)
    Parser.Exp_App a b -> AppE <$> renderExp a <*> renderExp b
    Parser.Exp_List l -> ListE <$> traverse renderExp l
    Parser.Exp_Sig e t -> SigE <$> renderExp e <*> renderType t

renderRecordExp :: Parser.RecordExp -> Either String Exp
renderRecordExp l =
  checkDuplicateLabels >> getRecordConName >>= constructExp
  where
    checkDuplicateLabels =
      maybe (return ()) (Left . showString "Duplicate labels: " . show) $
      mfilter (not . null) . Just $ 
      map (fst . head) $
      filter ((> 1) . length) $
      groupWith fst l
    getRecordConName =
      maybe (Left (showString "Record arity " . shows arity . shows " is not supported" $ ""))
            (Right) $
      recordConNameByArity arity
      where
        arity = length l
    constructExp n =
      foldl (\a (l, e) -> AppE <$> a <*> renderExp e)
            (pure (ConE n))
            (sortWith fst l)

renderLit :: Parser.Lit -> Lit
renderLit =
  \case
    Parser.Lit_Char c -> CharL c
    Parser.Lit_String t -> StringL (T.unpack t)
    Parser.Lit_Integer i -> IntegerL i
    Parser.Lit_Rational r -> RationalL r


