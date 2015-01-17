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
    (const $ fail "Expression context is not supported")
    (const $ fail "Pattern context is not supported")
    type'
    (const $ fail "Declaration context is not supported")
  where
    type' =
      fmap renderRecordType .
      either (fail . showString "Parser failure: ") return .
      Parser.run Parser.recordQQ . fromString

lens :: QuasiQuoter
lens =
  undefined


renderRecordType :: Parser.RecordType -> Type
renderRecordType l =
  foldl 
    (\a (n, v) -> AppT (AppT a (textLitT n)) (renderType v))
    (ConT (recordTypeNameByArity (length l))) 
    l
  where
    textLitT =
      LitT . StrTyLit . T.unpack

recordTypeNameByArity :: Int -> Name
recordTypeNameByArity =
  \case
    1 -> ''Types.Record1
    2 -> ''Types.Record2
    3 -> ''Types.Record3
    n -> error $ "Unsupported record arity " <> show n

renderType :: Parser.Type -> Type
renderType =
  \case
    Parser.AppType a b -> AppT (renderType a) (renderType b)
    Parser.VarType n -> VarT (mkName (T.unpack n))
    Parser.ConType n -> ConT (mkName (T.unpack n))
    Parser.TupleType a -> TupleT a
    Parser.ArrowType -> ArrowT
    Parser.ListType -> ListT
    Parser.RecordType a -> renderRecordType a



