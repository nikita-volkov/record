module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


contextASF :: ContextASF -> String
contextASF asf =
  do
    ast <- asf
    case ast of
      ContextAST_InCurlies _  -> "RECORD_PREPROCESSOR_PLACEHOLDER"
      ContextAST_StringLit x  -> stringLit x
      ContextAST_QuasiQuote x -> quasiQuote x
      ContextAST_Char x       -> return x

contextAST :: ContextAST -> String
contextAST =
  \case
    ContextAST_StringLit x  -> stringLit x
    ContextAST_QuasiQuote x -> quasiQuote x
    ContextAST_Char x       -> return x
    ContextAST_InCurlies _  -> error "ContextAST_InCurlies is not supported"

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

