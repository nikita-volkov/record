-- |
-- A set of composable string parsers.
module Record.Preprocessor.Parsing where

import Record.Prelude hiding (takeWhile, exp, try, many)
import Record.Preprocessor.Model
import Text.Parsec hiding ((<|>))


-- * General stuff
-------------------------

type Parse = 
  Parsec String ()

run :: Parse a -> String -> String -> Either String a
run p n =
  either (Left . show) Right .
  parse p n

labeled :: String -> Parse a -> Parse a
labeled =
  flip (<?>)


-- *
-------------------------

stringLit :: Parse String
stringLit =
  labeled "String Literal" $
  quoted '"'
  where 
    quoted q = 
      char q *> content <* char q
      where
        content =
          many char'
          where
            char' = 
              (try (char '\\' *> (try (char q) <|> char '\\'))) <|>
              (satisfy (/= q))

quasiQuote :: Parse QuasiQuote
quasiQuote =
  labeled "Quasi-quote" $
  (,) <$> opening <*> manyTill anyChar closing
  where
    opening =
      char '[' *> lowerCaseQualifiedIdent <* char '|'
    closing =
      string "|]"

lowerCaseQualifiedIdent :: Parse QualifiedIdent
lowerCaseQualifiedIdent =
  ((,) <$> many1 (upperCaseIdent <* char '.') <*> lowerCaseIdent) <|> 
  ((,) <$> pure [] <*> lowerCaseIdent)

lowerCaseIdent :: Parse String
lowerCaseIdent = 
  labeled "lowerCaseIdent" $
  (:) <$> firstChar <*> many restChar
  where
    firstChar = 
      satisfy $ \c -> isLower c || c == '_' || c == '\''
    restChar = 
      satisfy $ \c -> isAlphaNum c || c == '\'' || c == '_'

upperCaseIdent :: Parse String
upperCaseIdent =
  labeled "upperCaseIdent" $
  (:) <$>
    satisfy (\c -> isUpper c || c == '_') <*>
    (many . satisfy) (\c -> isAlphaNum c || c == '\'' || c == '_')


-- * AST
-------------------------

ast :: Parse AST
ast =
  (try $ AST_StringLit <$> stringLit) <|>
  (try $ AST_QuasiQuote <$> quasiQuote) <|>
  (try $ AST_InCurlies <$> asfBetween '{' '}') <|>
  (AST_Char <$> anyChar)

asf :: Parse ASF
asf =
  many ast <* eof

asfBetween :: Char -> Char -> Parse ASF
asfBetween opening closing =
  char opening *> manyTill ast (try (char closing))

