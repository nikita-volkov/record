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

newtype TypeAST =
  TypeAST (GeneralAST RecordType)

type TypeASF =
  [TypeAST]

type RecordType =
  [(String, TypeASF)]

newtype ExpAST =
  ExpAST (GeneralAST RecordExp)

type ExpASF =
  [ExpAST]

type RecordExp =
  [(String, Maybe ExpAST)]

newtype PatAST =
  PatAST (GeneralAST RecordPat)

type PatASF =
  [PatAST]

type RecordPat =
  [(String, Maybe PatAST)]

-- |
-- The most general Haskell syntax tree abstraction,
-- which allows to parse an injected syntax without breaking contexts.
data GeneralAST a =
  GeneralAST_Injection a |
  GeneralAST_StringLit String |
  GeneralAST_QuasiQuote QuasiQuote |
  GeneralAST_InRoundies [GeneralAST a] |
  GeneralAST_InSquarelies [GeneralAST a] |
  GeneralAST_Char Char


