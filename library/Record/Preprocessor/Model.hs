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
  Haskell_InCurlies (HaskellForest a) |
  Haskell_InRoundies (HaskellForest a) |
  Haskell_InSquarelies (HaskellForest a) |
  Haskell_Char Char
  deriving (Show, Functor, Foldable, Traversable)

type HaskellForest a =
  [Haskell a]


data UnleveledExtension =
  UnleveledExtension_InBraces Bool (HaskellForest UnleveledExtension)
  deriving (Show)


type Placeholder =
  (Position.Position, UnleveledExtension)


data Type =
  Type_Record Bool [(String, (HaskellForest Type))]
  deriving (Show)


data Exp a =
  Exp_Record Bool [(String, Maybe a)]
  deriving (Show, Functor, Foldable, Traversable)


data Pat =
  Pat_Record Bool [Either String (HaskellForest Pat)]
  deriving (Show)


data Extension =
  Extension_Type Type |
  Extension_Exp (Exp (HaskellForest Extension))
  deriving (Show)
