module Record.Preprocessor.Model where

import Record.Prelude


data Context =
  Context_Type |
  Context_Exp |
  Context_Pat |
  Context_Decl
  deriving (Show)


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


type QuasiQuote =
  (QualifiedIdent, String)

type QualifiedIdent =
  ([String], String)


-- |
-- The most general Haskell syntax tree abstraction,
-- which allows to parse an injected syntax without breaking contexts.
data GeneralAST a =
  GeneralAST_Injection a |
  GeneralAST_StringLit String |
  GeneralAST_QuasiQuote QuasiQuote |
  GeneralAST_InCurlies [GeneralAST a] |
  GeneralAST_InRoundies [GeneralAST a] |
  GeneralAST_InSquarelies [GeneralAST a] |
  GeneralAST_Char Char
  deriving (Show)


type PlaceholderAST =
  GeneralAST Placeholder

data Placeholder =
  Placeholder_InLazyBraces [PlaceholderAST] |
  Placeholder_InStrictBraces [PlaceholderAST]
  deriving (Show)


type TypeAST =
  GeneralAST TypeExtension

data TypeExtension =
  TypeExtension_Record Bool [(String, [TypeAST])]


newtype ExpAST =
  ExpAST (GeneralAST RecordExp)

type RecordExp =
  [(String, Maybe ExpAST)]


newtype PatAST =
  PatAST (GeneralAST RecordPat)

type RecordPat =
  [(String, Maybe PatAST)]


