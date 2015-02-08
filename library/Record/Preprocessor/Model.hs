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

data ExpAST =
  ExpAST_RecordExp RecordExp |
  ExpAST_InRoundies ExpASF |
  ExpAST_InSquarelies ExpASF |
  ExpAST_StringLit String |
  ExpAST_QuasiQuote QuasiQuote |
  ExpAST_Char Char

type ExpASF =
  [ExpAST]

type RecordExp =
  [(String, Maybe ExpAST)]

data PatAST =
  PatAST_RecordPat RecordPat |
  PatAST_InRoundies PatASF |
  PatAST_InSquarelies PatASF |
  PatAST_StringLit String |
  PatAST_QuasiQuote QuasiQuote |
  PatAST_Char Char

type PatASF =
  [PatAST]

type RecordPat =
  [(String, Maybe PatAST)]
