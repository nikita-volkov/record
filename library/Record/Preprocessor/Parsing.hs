-- |
-- A set of composable string parsers.
module Record.Preprocessor.Parsing where

import Record.Prelude hiding (takeWhile, exp, try, many)
import Record.Preprocessor.Model
import Text.Parsec hiding ((<|>))


-- * General stuff
-------------------------

type Parser = 
  Parsec String ()

run :: Parser a -> String -> String -> Either ParseError a
run =
  parse

labeled :: String -> Parser a -> Parser a
labeled =
  flip (<?>)


-- *
-------------------------

stringLit :: Parser String
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

quasiQuote :: Parser QuasiQuote
quasiQuote =
  labeled "Quasi-quote" $
  (,) <$> opening <*> manyTill anyChar closing
  where
    opening =
      char '[' *> lowerCaseQualifiedIdent <* char '|'
    closing =
      string "|]"

lowerCaseQualifiedIdent :: Parser QualifiedIdent
lowerCaseQualifiedIdent =
  ((,) <$> many1 (upperCaseIdent <* char '.') <*> lowerCaseIdent) <|> 
  ((,) <$> pure [] <*> lowerCaseIdent)

lowerCaseIdent :: Parser String
lowerCaseIdent = 
  labeled "lowerCaseIdent" $
  (:) <$> firstChar <*> many restChar
  where
    firstChar = 
      satisfy $ \c -> isLower c || c == '_' || c == '\''
    restChar = 
      satisfy $ \c -> isAlphaNum c || c == '\'' || c == '_'

upperCaseIdent :: Parser String
upperCaseIdent =
  labeled "upperCaseIdent" $
  (:) <$>
    satisfy (\c -> isUpper c || c == '_') <*>
    (many . satisfy) (\c -> isAlphaNum c || c == '\'' || c == '_')


-- * AST
-------------------------

ast :: Parser AST
ast =
  (try $ AST_StringLit <$> stringLit) <|>
  (try $ AST_QuasiQuote <$> quasiQuote) <|>
  (try $ AST_InCurlies <$> inCurlies) <|>
  (AST_Other <$> rest)
  where
    rest =
      many1 (noneOf "{}")
    inCurlies =
      char '{' *> many ast <* char '}'

asf :: Parser ASF
asf =
  many ast <* eof
