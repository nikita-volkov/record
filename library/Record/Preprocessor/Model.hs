module Record.Preprocessor.Model where

import Record.Prelude


data Level =
  Level_Type |
  Level_Exp |
  Level_Pat |
  Level_Decl
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
-- An AST with disambiguated contexts and an interspersed extension type.
data DecontextedAST a =
  DecontextedAST_Injection a |
  DecontextedAST_StringLit String |
  DecontextedAST_QuasiQuote QuasiQuote |
  DecontextedAST_InCurlies [DecontextedAST a] |
  DecontextedAST_InRoundies [DecontextedAST a] |
  DecontextedAST_InSquarelies [DecontextedAST a] |
  DecontextedAST_Char Char
  deriving (Show, Functor)


data UnleveledAST =
  UnleveledAST_InLazyBraces [DecontextedAST UnleveledAST] |
  UnleveledAST_InStrictBraces [DecontextedAST UnleveledAST]
  deriving (Show)


data TypeAST =
  TypeAST_Record Bool [(String, [DecontextedAST TypeAST])]
  deriving (Show)


data ExpAST a =
  ExpAST_Record Bool (RecordExpBody a)
  deriving (Show, Functor)

data RecordExpBody a =
  RecordExpBody_Positional [Maybe [DecontextedAST a]] |
  RecordExpBody_Named [(String, Maybe [DecontextedAST a])]
  deriving (Show, Functor)


data PatAST =
  PatAST_Record Bool [Either String [DecontextedAST PatAST]]
  deriving (Show)


data AST =
  AST_Type TypeAST |
  AST_Exp (ExpAST AST) |
  AST_Pat PatAST
  deriving (Show)
