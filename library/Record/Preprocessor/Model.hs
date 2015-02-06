module Record.Preprocessor.Model where

import Record.Prelude


data ContextAST =
  ContextAST_InCurlies [ContextAST] |
  ContextAST_StringLit String |
  ContextAST_QuasiQuote QuasiQuote |
  ContextAST_Char Char
  deriving (Show)

type QuasiQuote =
  (QualifiedIdent, String)

type QualifiedIdent =
  ([String], String)

-- | Abstract syntax forest
type ContextASF =
  [ContextAST]

data Context =
  Context_Type |
  Context_Exp |
  Context_Pat |
  Context_Decl
  deriving (Show)
