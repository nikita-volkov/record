module Record.Preprocessor.Model where

import Record.Prelude
import qualified Record.Preprocessor.Position as Position


data Level =
  Level_Type |
  Level_Exp |
  Level_Pat |
  Level_Decl
  deriving (Show)


type QuasiQuote =
  (QualifiedIdent, String)

type QualifiedIdent =
  ([String], String)


-- |
-- An AST with disambiguated contexts and an interspersed extension type.
data Decontexted a =
  Decontexted_Injection a |
  Decontexted_CharLit String |
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
  (Position.Position, Unleveled)


data Type =
  Type_Record Bool [(String, [Decontexted Type])]
  deriving (Show)


data Exp a =
  Exp_Record Bool [(String, Maybe [Decontexted a])]
  deriving (Show, Functor)


data Pat =
  Pat_Record Bool [Either String [Decontexted Pat]]
  deriving (Show)


data AST =
  AST_Type Type |
  AST_Exp (Exp AST) |
  AST_Pat Pat
  deriving (Show)
