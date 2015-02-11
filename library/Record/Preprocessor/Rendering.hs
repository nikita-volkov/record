module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


placeholderASTUsingPlaceholders :: PlaceholderAST -> String
placeholderASTUsingPlaceholders =
  \case
    PlaceholderAST_InCurlies _ _ -> "Ъ"
    PlaceholderAST_StringLit x   -> stringLit x
    PlaceholderAST_QuasiQuote x  -> quasiQuote x
    PlaceholderAST_Char x        -> return x

placeholderAST :: PlaceholderAST -> String
placeholderAST =
  \case
    PlaceholderAST_InCurlies _ x -> "{" <> foldMap placeholderAST x <> "}"
    PlaceholderAST_StringLit x   -> stringLit x
    PlaceholderAST_QuasiQuote x  -> quasiQuote x
    PlaceholderAST_Char x        -> return x

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

