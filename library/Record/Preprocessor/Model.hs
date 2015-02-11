module Record.Preprocessor.Model where

import Record.Prelude



data CursorOffset =
  CursorOffset Int Int
  deriving (Show, Ord, Eq)

instance Monoid CursorOffset where
  mempty = 
    CursorOffset 0 0
  mappend (CursorOffset l1 c1) (CursorOffset l2 c2) =
    if l2 <= 0
      then CursorOffset (l1 + l2) (c1 + c2)
      else CursorOffset (l1 + l2) c2


data PlaceholderAST =
  PlaceholderAST_InCurlies CursorOffset [PlaceholderAST] |
  PlaceholderAST_StringLit String |
  PlaceholderAST_QuasiQuote QuasiQuote |
  PlaceholderAST_Char Char
  deriving (Show)

type QuasiQuote =
  (QualifiedIdent, String)

type QualifiedIdent =
  ([String], String)



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


