module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parsing as Parsing
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE


process :: String -> Either String String
process =
  traverseASTs HSE.Mode_Module <=< Parsing.run Parsing.asf ""
  where
    traverseASTs mode asf =
      fmap mconcat $ do
        modes <- (fmap . fmap) hseModeByContext $ contexts
        zipWithM traverseASTs modes subASFs
      where
        hseModeByContext =
          \case
            Context_RecordType -> HSE.Mode_Type
            Context_RecordExp  -> HSE.Mode_Exp
            Context_RecordPat  -> HSE.Mode_Pat
        subASFs =
          catMaybes $ flip map asf $ \case
            AST_InCurlies asf -> Just asf
            _ -> Nothing
        rendering =
          Rendering.asts asf
        contexts =
          HSE.runParseResult $ HSE.reifyContexts mode rendering


