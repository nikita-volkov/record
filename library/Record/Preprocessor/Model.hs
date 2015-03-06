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
data Decontexted a =
  Decontexted_Injection a |
  Decontexted_StringLit String |
  Decontexted_QuasiQuote QuasiQuote |
  Decontexted_InCurlies [Decontexted a] |
  Decontexted_InRoundies [Decontexted a] |
  Decontexted_InSquarelies [Decontexted a] |
  Decontexted_Char Char
  deriving (Show, Functor)


data Unleveled =
  Unleveled_InLazyBraces [Decontexted Unleveled] |
  Unleveled_InStrictBraces [Decontexted Unleveled]
  deriving (Show)


type Placeholder =
  (CursorOffset, Unleveled)


data Type =
  Type_Record Bool [(String, [Decontexted Type])]
  deriving (Show)


data Exp a =
  Exp_Record Bool (RecordExpBody a)
  deriving (Show, Functor)

data RecordExpBody a =
  RecordExpBody_Positional [Maybe [Decontexted a]] |
  RecordExpBody_Named [(String, Maybe [Decontexted a])]
  deriving (Show, Functor)


data Pat =
  Pat_Record Bool [Either String [Decontexted Pat]]
  deriving (Show)


data AST =
  AST_Type Type |
  AST_Exp (Exp AST) |
  AST_Pat Pat
  deriving (Show)
