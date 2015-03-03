module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


placeholder :: Placeholder -> String
placeholder =
  \case
    Placeholder_InLazyBraces x -> "(~" <> foldMap (generalAST placeholder) x <> "~)"
    Placeholder_InStrictBraces x -> "(!" <> foldMap (generalAST placeholder) x <> "!)"

generalAST :: (a -> String) -> GeneralAST a -> String
generalAST injection =
  \case
    GeneralAST_Injection x -> injection x
    GeneralAST_StringLit x -> stringLit x
    GeneralAST_QuasiQuote x -> quasiQuote x
    GeneralAST_InCurlies x -> "{" <> foldMap (generalAST injection) x <> "}"
    GeneralAST_InRoundies x -> "(" <> foldMap (generalAST injection) x <> ")"
    GeneralAST_InSquarelies x -> "[" <> foldMap (generalAST injection) x <> "]"
    GeneralAST_Char x -> return x

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

typeExtension :: TypeExtension -> String
typeExtension =
  \case
    TypeExtension_Record strict fields ->
      (if strict then "StrictRecord" else "LazyRecord") <> show (length fields) <> " " <>
      intercalate " " (map renderField fields)
      where
        renderField (name, asts) =
          "\"" <> name <> "\" " <> foldMap (generalAST typeExtension) asts

