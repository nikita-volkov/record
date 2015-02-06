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
    traverseASTs mode asts =
      do
        lASTs <- return $ labelASTs asts
        rendering <- return $ Rendering.labeledASTs lASTs
        labelTypeMap <- HSE.runParseResult $ HSE.reifyContextMap mode rendering
        
        undefined


-- |
-- Assign unique labels to ASTs.
-- These labels should be usable as valid Haskell lexemes,
-- so that "haskell-src-exts" would accept them fine.
labelASTs :: [AST] -> [LabeledAST]
labelASTs asts =
  flip evalStateT 1 $ do
    ast <- lift $ asts
    case ast of
      AST_InCurlies asts' -> fmap (LabeledAST_Label asts' . ("RECORD_PREPROCESSOR_LABEL_" <>) . show) $ 
                             state $ id &&& succ
      AST_StringLit x     -> return $ LabeledAST_StringLit x
      AST_QuasiQuote x    -> return $ LabeledAST_QuasiQuote x
      AST_Other x         -> return $ LabeledAST_Other x

      
      
