module Record.Preprocessor.Model where

import Record.Prelude


type Label = 
  String

type LabeledAST =
  (,) Label AST

data AST =
  AST_InCurlies ASF |
  AST_StringLit String |
  AST_QuasiQuote QuasiQuote |
  AST_Other String
  deriving (Show)

type QuasiQuote =
  (,) String String

-- | Abstract syntax forest
type ASF =
  [AST]

data ASFType =
  ASFType_RecordType |
  ASFType_RecordExp |
  ASFType_RecordPat
  deriving (Show)

type ASFTypeMap =
  HashMap Label ASFType
