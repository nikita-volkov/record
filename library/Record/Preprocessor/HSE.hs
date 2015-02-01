module Record.Preprocessor.HSE where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Language.Haskell.Exts as E
import qualified Data.HashMap.Strict as HashMap
import qualified Record.Preprocessor.HSE.ASFTypeAssocs as ASFTypeAssocs


data Mode =
  Mode_Module

data Context =
  Context_Type |
  Context_Exp |
  Context_Pat |
  Context_Decl

-- |
-- Parses the code using "haskell-src-exts", reifying the AST types.
reifyASFTypeMap :: Mode -> String -> E.ParseResult (HashMap Label ASFType)
reifyASFTypeMap =
  \case
    Mode_Module -> 
      fmap (HashMap.fromList . ASFTypeAssocs.module_) . E.parseModule
      -- where
      --   onResult = 
