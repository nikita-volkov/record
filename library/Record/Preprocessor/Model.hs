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
-- An extendable Haskell AST.
data Haskell a =
  Haskell_Extension a |
  Haskell_CharLit String |
  Haskell_StringLit String |
  Haskell_QuasiQuote QuasiQuote |
  Haskell_InCurlies [Haskell a] |
  Haskell_InRoundies [Haskell a] |
  Haskell_InSquarelies [Haskell a] |
  Haskell_Char Char
  deriving (Show, Functor, Foldable, Traversable)


data Unleveled =
  Unleveled_InLazyBraces [Haskell Unleveled] |
  Unleveled_InStrictBraces [Haskell Unleveled]
  deriving (Show)


type Placeholder =
  (Position.Position, Unleveled)


data Type =
  Type_Record Bool [(String, [Haskell Type])]
  deriving (Show)


data Exp a =
  Exp_Record Bool [(String, Maybe [Haskell a])]
  deriving (Show, Functor, Foldable, Traversable)


data Pat =
  Pat_Record Bool [Either String [Haskell Pat]]
  deriving (Show)


data AST =
  AST_Type Type |
  AST_Exp (Exp AST) |
  AST_Pat Pat
  deriving (Show)
