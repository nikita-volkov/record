module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model


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

      
      
