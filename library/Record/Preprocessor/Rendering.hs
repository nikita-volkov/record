module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


asf :: ASF -> String
asf asf =
  do
    ast <- asf
    case ast of
      AST_InCurlies _  -> "RECORD_PREPROCESSOR_PLACEHOLDER"
      AST_StringLit x  -> stringLit x
      AST_QuasiQuote x -> quasiQuote x
      AST_Other x      -> x

ast :: AST -> String
ast =
  \case
    AST_StringLit x  -> stringLit x
    AST_QuasiQuote x -> quasiQuote x
    AST_Other x      -> x
    AST_InCurlies _  -> error "AST_InCurlies is not supported"

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

