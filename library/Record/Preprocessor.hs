module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model


-- |
-- Assign unique labels to ASTs.
-- These labels should be usable as valid Haskell lexemes,
-- so that "haskell-src-exts" would accept them fine.
labelASTs :: [AST] -> [LabeledAST]
labelASTs phrases =
  flip evalStateT 1 $ do
    phrase <- lift $ phrases
    label <- fmap (("RECORD_PREPROCESSOR_LABEL_" <>) . show) $ state $ id &&& succ
    return $ (,) label phrase
