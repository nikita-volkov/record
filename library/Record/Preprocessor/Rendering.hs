module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


ambiguousAST :: AmbiguousAST -> String
ambiguousAST =
  \case
    AmbiguousAST_InLazyBraces x -> "(~" <> foldMap (decontextedAST ambiguousAST) x <> "~)"
    AmbiguousAST_InStrictBraces x -> "(!" <> foldMap (decontextedAST ambiguousAST) x <> "!)"

decontextedAST :: (a -> String) -> DecontextedAST a -> String
decontextedAST injection =
  \case
    DecontextedAST_Injection x -> injection x
    DecontextedAST_StringLit x -> stringLit x
    DecontextedAST_QuasiQuote x -> quasiQuote x
    DecontextedAST_InCurlies x -> "{" <> foldMap (decontextedAST injection) x <> "}"
    DecontextedAST_InRoundies x -> "(" <> foldMap (decontextedAST injection) x <> ")"
    DecontextedAST_InSquarelies x -> "[" <> foldMap (decontextedAST injection) x <> "]"
    DecontextedAST_Char x -> return x

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

typeAST :: TypeAST -> String
typeAST =
  \case
    TypeAST_Record strict fields ->
      (if strict then "StrictRecord" else "LazyRecord") <> show (length fields) <> " " <>
      intercalate " " (map renderField fields)
      where
        renderField (name, asts) =
          "\"" <> name <> "\" " <> foldMap (decontextedAST typeAST) asts

expAST :: ExpAST -> String
expAST =
  \case
    ExpAST_Record strict (RecordExpBody_Named sections) ->
      flip evalState 0 $ do
        sectionStrings <-
          forM sortedSections $ \(name, asts) -> case asts of
            Nothing -> do
              modify succ
              fmap varName get
            Just asts -> return $ "(" <> foldMap (decontextedAST expAST) asts <> ")"
        numArgs <- get
        let exp = (if strict then "StrictRecord" else "LazyRecord") <> show (length sections) <>
                  " " <> intercalate " " sectionStrings
        case numArgs of
          0 -> return $ exp
          n -> return $ "\\" <> intercalate " " (map varName [1 .. numArgs]) <> " -> " <> exp
      where
        varName n = 
          "ѣ" <> show n
        sortedSections =
          sortWith fst sections

