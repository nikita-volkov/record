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


data Placeholder =
  Placeholder_InLazyBraces [DecontextedAST Placeholder] |
  Placeholder_InStrictBraces [DecontextedAST Placeholder]
  deriving (Show)


data TypeExtension =
  TypeExtension_Record Bool [(String, [DecontextedAST TypeExtension])]


data ExpExtension =
  ExpExtension_Record Bool RecordExpBody

data RecordExpBody =
  RecordExpBody_Positional [Maybe [DecontextedAST ExpExtension]] |
  RecordExpBody_Named [(String, Maybe [DecontextedAST ExpExtension])]


data PatExtension =
  PatExtension_Record Bool [Either String [DecontextedAST PatExtension]]


