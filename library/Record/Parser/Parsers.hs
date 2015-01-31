-- |
-- A set of composable string parsers.
module Record.Parser.Parsers where

import BasePrelude hiding (takeWhile, exp, try, many)
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

quasiQuote :: Parser (String, String)
quasiQuote =
  labeled "Quasi-quote" $
  (,) <$> opening <*> manyTill anyChar closing
  where
    opening =
      char '[' *> lowerCaseName <* char '|'
    closing =
      string "|]"

lowerCaseName :: Parser String
lowerCaseName = 
  (:) <$> firstChar <*> many restChar
  where
    firstChar = 
      satisfy $ \c -> isLower c || c == '_' || c == '\''
    restChar = 
      satisfy $ \c -> isAlphaNum c || c == '\'' || c == '_'


data Phrase =
  InCurlies [Phrase] |
  Rest String
  deriving (Show)

phrase :: Parser Phrase
phrase =
  (try $ Rest <$> rest) <|>
  (InCurlies <$> inCurlies)
  where
    rest =
      many1 (noneOf "{}")
    inCurlies =
      char '{' *> many phrase <* char '}'

phrases :: Parser [Phrase]
phrases =
  many phrase <* eof
