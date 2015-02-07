module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


placeholderASF :: PlaceholderASF -> String
placeholderASF asf =
  do
    ast <- asf
    case ast of
      PlaceholderAST_InCurlies _  -> "RECORD_PREPROCESSOR_PLACEHOLDER"
      PlaceholderAST_StringLit x  -> stringLit x
      PlaceholderAST_QuasiQuote x -> quasiQuote x
      PlaceholderAST_Char x       -> return x

placeholderAST :: PlaceholderAST -> String
placeholderAST =
  \case
    PlaceholderAST_StringLit x  -> stringLit x
    PlaceholderAST_QuasiQuote x -> quasiQuote x
    PlaceholderAST_Char x       -> return x
    PlaceholderAST_InCurlies _  -> error "PlaceholderAST_InCurlies is not supported"

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

