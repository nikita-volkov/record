module Record.Preprocessor.Model where

import Record.Prelude


data PlaceholderAST =
  PlaceholderAST_InCurlies [PlaceholderAST] |
  PlaceholderAST_StringLit String |
  PlaceholderAST_QuasiQuote QuasiQuote |
  PlaceholderAST_Char Char
  deriving (Show)

type QuasiQuote =
  (QualifiedIdent, String)

type QualifiedIdent =
  ([String], String)

-- | Abstract syntax forest
type PlaceholderASF =
  [PlaceholderAST]

data Context =
  Context_Type |
  Context_Exp |
  Context_Pat |
  Context_Decl
  deriving (Show)

data TypeAST =
  TypeAST_RecordType RecordType |
  TypeAST_InRoundies TypeASF |
  TypeAST_InSquarelies TypeASF |
  TypeAST_StringLit String |
  TypeAST_QuasiQuote QuasiQuote |
  TypeAST_Char Char

type TypeASF =
  [TypeAST]

type RecordType =
  [(String, TypeASF)]


