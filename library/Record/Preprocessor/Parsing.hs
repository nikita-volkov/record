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


-- * ContextAST
-------------------------

contextAST :: Parse ContextAST
contextAST =
  (try $ ContextAST_StringLit <$> stringLit) <|>
  (try $ ContextAST_QuasiQuote <$> quasiQuote) <|>
  (try $ ContextAST_InCurlies <$> asfBetween '{' '}') <|>
  (ContextAST_Char <$> anyChar)
  where
    asfBetween opening closing =
      char opening *> manyTill contextAST (try (char closing))

contextASF :: Parse ContextASF
contextASF =
  many contextAST <* eof


-- * TypeAST
-------------------------

typeAST :: Parse TypeAST
typeAST =
  (try $ TypeAST_RecordType <$> recordType) <|>
  (try $ TypeAST_InRoundies <$> asfBetween '(' ')') <|>
  (try $ TypeAST_InSquarelies <$> asfBetween '[' ']') <|>
  (try $ TypeAST_StringLit <$> stringLit) <|>
  (try $ TypeAST_QuasiQuote <$> quasiQuote) <|>
  (TypeAST_Char <$> anyChar)
  where
    asfBetween opening closing =
      char opening *> manyTill typeAST (try (char closing))

recordType :: Parse RecordType
recordType =
  char '{' *> skipMany space *> sepBy1 field sep <* end
  where
    field =
      (,) <$> (lowerCaseIdent <* skipMany space <* string "::" <* skipMany space) <*> 
              manyTill typeAST (try (lookAhead (sep <|> end)))
    sep =
      skipMany space <* char ',' <* skipMany space
    end =
      skipMany space <* char '}'
