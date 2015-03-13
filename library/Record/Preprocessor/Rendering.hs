module Record.Preprocessor.Rendering where

import Record.Prelude
import Record.Preprocessor.Model


unleveledExtension :: UnleveledExtension -> String
unleveledExtension =
  \case
    UnleveledExtension_InBraces False x -> "(~" <> foldMap (haskell unleveledExtension) x <> "~)"
    UnleveledExtension_InBraces True x -> "(!" <> foldMap (haskell unleveledExtension) x <> "!)"

haskell :: (a -> String) -> Haskell a -> String
haskell injection =
  \case
    Haskell_Extension x -> injection x
    Haskell_CharLit x -> charLit x
    Haskell_StringLit x -> stringLit x
    Haskell_QuasiQuote x -> quasiQuote x
    Haskell_InCurlies x -> "{" <> foldMap (haskell injection) x <> "}"
    Haskell_InRoundies x -> "(" <> foldMap (haskell injection) x <> ")"
    Haskell_InSquarelies x -> "[" <> foldMap (haskell injection) x <> "]"
    Haskell_Char x -> return x

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
          "\"" <> name <> "\" " <> foldMap (haskell type_) asts

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
            Just a -> return $ "(" <> inner a <> ")"
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

