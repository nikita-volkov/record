module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


labeledASTs :: [LabeledAST] -> String
labeledASTs =
  foldMap $ \case
    LabeledAST_Label _ x -> label x
    LabeledAST_StringLit x -> stringLit x
    LabeledAST_QuasiQuote x -> quasiQuote x
    LabeledAST_Other x -> x

label :: Label -> String
label = 
  ('(' :) . (<> ")") . id

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

