module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parsing as Parsing
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE


process :: String -> Either String String
process =
  traverseASF HSE.Mode_Module <=< Parsing.run Parsing.asf ""
  where
    traverseASF mode asf =
      do
        modes <- (fmap . fmap) hseModeByContext $ contexts
        loop (asf, modes)
      where
        loop =
          \case 
            (,) (AST_InCurlies asf' : asf) (mode : modes) -> 
              traverseASF mode asf' >>= \x -> fmap (x <>) $ loop (asf, modes)
            (,) (ast : asf) modes -> 
              fmap (Rendering.ast ast <>) $ loop (asf, modes)
            (,) [] [] -> return ""
            (,) _ _ -> error "Unmatching ASF and modes"
        hseModeByContext =
          \case
            Context_Type -> HSE.Mode_Type
            Context_Exp  -> HSE.Mode_Exp
            Context_Pat  -> HSE.Mode_Pat
        subASFs =
          catMaybes $ flip map asf $ \case
            AST_InCurlies asf -> Just asf
            _ -> Nothing
        rendering =
          Rendering.asf asf
        contexts =
          HSE.runParseResult $ HSE.reifyContexts mode rendering


