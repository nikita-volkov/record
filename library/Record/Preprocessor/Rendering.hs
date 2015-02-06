module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


asts :: [AST] -> String
asts asts =
  flip evalStateT 1 $ do
    ast <- lift $ asts
    case ast of
      AST_InCurlies _  -> join $ fmap (lift . ("(RECORD_PREPROCESSOR_LABEL_" <>) . (<> ")") . show) $ 
                          state $ id &&& succ
      AST_StringLit x  -> lift $ stringLit x
      AST_QuasiQuote x -> lift $ quasiQuote x
      AST_Other x      -> lift $ x

stringLit :: String -> String
stringLit =
  ('"' :) . (<> "\"") .
  foldMap (\case '\\' -> "\\\\"; '"' -> "\\\""; x -> return x)

quasiQuote :: QuasiQuote -> String
quasiQuote (n, q) =
  "[" <> qualifiedIdent n <> "|" <> q <> "|]"

qualifiedIdent :: QualifiedIdent -> String
qualifiedIdent (ns, n) =
  foldMap (<> ".") ns <> n

