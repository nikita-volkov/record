module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


unleveled :: Unleveled -> String
unleveled =
  \case
    Unleveled_InLazyBraces x -> "(~" <> foldMap (decontexted unleveled) x <> "~)"
    Unleveled_InStrictBraces x -> "(!" <> foldMap (decontexted unleveled) x <> "!)"

decontexted :: (a -> String) -> Decontexted a -> String
decontexted injection =
  \case
    Decontexted_Injection x -> injection x
    Decontexted_CharLit x -> charLit x
    Decontexted_StringLit x -> stringLit x
    Decontexted_QuasiQuote x -> quasiQuote x
    Decontexted_InCurlies x -> "{" <> foldMap (decontexted injection) x <> "}"
    Decontexted_InRoundies x -> "(" <> foldMap (decontexted injection) x <> ")"
    Decontexted_InSquarelies x -> "[" <> foldMap (decontexted injection) x <> "]"
    Decontexted_Char x -> return x

charLit :: String -> String
charLit =
  ('\'' :) . (<> "'")

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

type_ :: Type -> String
type_ =
  \case
    Type_Record strict fields ->
      (if strict then "StrictRecord" else "LazyRecord") <> show (length fields) <> " " <>
      intercalate " " (map renderField fields)
      where
        renderField (name, asts) =
          "\"" <> name <> "\" " <> foldMap (decontexted type_) asts

exp :: (a -> String) -> Exp a -> String
exp inner =
  \case
    Exp_Record strict sections ->
      flip evalState 0 $ do
        sectionStrings <-
          forM sortedSections $ \(name, asts) -> case asts of
            Nothing -> do
              modify succ
              fmap varName get
            Just asts -> return $ "(" <> foldMap (decontexted inner) asts <> ")"
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

