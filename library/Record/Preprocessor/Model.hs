module Record.Preprocessor.Model where

import Record.Prelude


data AST =
  AST_InCurlies [AST] |
  AST_StringLit String |
  AST_QuasiQuote QuasiQuote |
  AST_Char Char
  deriving (Show)

type QuasiQuote =
  (,) QualifiedIdent String

type QualifiedIdent =
  (,) ([] String) String

-- | Abstract syntax forest
type ASF =
  [] AST

data Context =
  Context_Type |
  Context_Exp |
  Context_Pat |
  Context_Decl
  deriving (Show)
