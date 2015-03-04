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


data AmbiguousAST =
  AmbiguousAST_InLazyBraces [DecontextedAST AmbiguousAST] |
  AmbiguousAST_InStrictBraces [DecontextedAST AmbiguousAST]
  deriving (Show)


data TypeAST =
  TypeAST_Record Bool [(String, [DecontextedAST TypeAST])]


data ExpAST a =
  ExpAST_Record Bool (RecordExpBody a)

data RecordExpBody a =
  RecordExpBody_Positional [Maybe [DecontextedAST a]] |
  RecordExpBody_Named [(String, Maybe [DecontextedAST a])]


data PatAST =
  PatAST_Record Bool [Either String [DecontextedAST PatAST]]


