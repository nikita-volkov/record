module Record.Preprocessor.HSE where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Language.Haskell.Exts as E
import qualified Data.HashMap.Strict as HashMap
import qualified Record.Preprocessor.HSE.LabelContextAssocs as LabelContextAssocs


data Mode =
  Mode_Module

runParseResult :: E.ParseResult a -> Either String a
runParseResult =
  \case
    E.ParseOk a -> return a
    E.ParseFailed l m -> Left $ m <> "; Location: " <> show l

-- |
-- Parses the code using "haskell-src-exts", reifying the AST types.
reifyContextMap :: Mode -> String -> E.ParseResult (HashMap Label Context)
reifyContextMap =
  \case
    Mode_Module -> 
      fmap (HashMap.fromList . LabelContextAssocs.module_) . E.parseModule
      -- where
      --   onResult = 
