module Record.Preprocessor.Model where

import Record.Prelude


data AST =
  AST_InCurlies [AST] |
  AST_StringLit String |
  AST_QuasiQuote QuasiQuote |
  AST_Other String
  deriving (Show)

type QuasiQuote =
  (,) QualifiedIdent String

type QualifiedIdent =
  (,) ([] String) String

-- | Abstract syntax forest
type ASF =
  [] AST

data Context =
  Context_RecordType |
  Context_RecordExp |
  Context_RecordPat
  deriving (Show)

type ContextMap =
  HashMap Label Context

data LabeledAST =
  LabeledAST_Label [AST] Label |
  LabeledAST_StringLit String |
  LabeledAST_QuasiQuote QuasiQuote |
  LabeledAST_Other String

type Label = 
  String
