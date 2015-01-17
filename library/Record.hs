module Record where

import BasePrelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Record.Field as Field
import qualified Record.Lens as Lens
import qualified Record.Parser as Parser
import qualified Data.Text as T


-- |
-- type User = [record| (name :: String, birthday :: (year :: Int, month :: Int, day :: Int)) |]
-- type User = (Field "name" String, Field "birthday" (Field "year" Int, Field "month" Int, Field "day" Int))
-- type User = forall a. (Owner "name" String a, ...) => a
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
  foldl AppT (TupleT (length l)) $ map field $ l
  where
    field (n, t) =
      ConT ''Field.Field `AppT`
      LitT (StrTyLit (T.unpack n)) `AppT`
      renderType t

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



