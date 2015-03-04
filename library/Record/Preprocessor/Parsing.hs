-- |
-- A set of composable string parsers.
module Record.Preprocessor.Parsing where

import Record.Prelude hiding (takeWhile, exp, try, many)
import Record.Preprocessor.Model
import Text.Parsec hiding ((<|>))
import Text.Parsec.Error


-- * General stuff
-------------------------

type Parse = 
  Parsec String ()

type Error =
  (CursorOffset, String)

run :: Parse a -> String -> String -> Either Error a
run p n =
  either (Left . (cursorOffset . errorPos &&& messageString . head . errorMessages)) Right .
  parse p n
  where
    cursorOffset p =
      CursorOffset (fromIntegral $ sourceLine p) (fromIntegral $ sourceColumn p)

labeled :: String -> Parse a -> Parse a
labeled =
  flip (<?>)

total :: Parse a -> Parse a
total =
  (<* eof)  

-- *
-------------------------

cursorOffset :: Parse CursorOffset
cursorOffset =
  flip fmap getPosition $ \p ->
    CursorOffset (fromIntegral $ sourceLine p) (fromIntegral $ sourceColumn p)

cursorOffsetAtEnd :: Parse CursorOffset
cursorOffsetAtEnd =
  skipMany anyChar *> cursorOffset

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


-- * DecontextedAST
-------------------------

decontextedAST :: Parse a -> Parse (DecontextedAST a)
decontextedAST injection =
  (try $ DecontextedAST_Injection <$> injection) <|>
  (try $ DecontextedAST_StringLit <$> stringLit) <|>
  (try $ DecontextedAST_QuasiQuote <$> quasiQuote) <|>
  (try $ DecontextedAST_InCurlies <$> enclosedIn '{' '}') <|>
  (try $ DecontextedAST_InRoundies <$> enclosedIn '(' ')') <|>
  (try $ DecontextedAST_InSquarelies <$> enclosedIn '[' ']') <|>
  (DecontextedAST_Char <$> anyChar)
  where
    enclosedIn opening closing =
      char opening *> manyTill (decontextedAST injection) (try (char closing))


-- *
-------------------------

ambiguousAST :: Parse AmbiguousAST
ambiguousAST =
  (try (AmbiguousAST_InLazyBraces <$> between (string "(~") (string "~)"))) <|>
  (try (AmbiguousAST_InStrictBraces <$> between (string "(!") (string "!)")))
  where
    between opening closing =
      opening *> manyTill (decontextedAST ambiguousAST) (try closing)


-- * DecontextedAST TypeAST
-------------------------

typeAST :: Parse TypeAST
typeAST =
  try (record True) <|> (record False)
  where
    record strict =
      fmap (TypeAST_Record strict) $
      string opening *> skipMany space *> sepBy1 field sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        field =
          (,) <$> (lowerCaseIdent <* skipMany space <* string "::" <* skipMany space) <*> 
                  manyTill (decontextedAST typeAST) (try (lookAhead (sep <|> end)))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


-- * Expression
-------------------------

expAST :: Parse ExpAST
expAST =
  try (record True) <|> (record False)
  where
    record strict =
      fmap (ExpAST_Record strict) $
      string opening *> skipMany space *> body <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing
        body =
          named
          where
            named =
              RecordExpBody_Named <$>
              sepBy1 (try assignment <|> placeholder) sep
              where
                assignment =
                  (,) <$> (lowerCaseIdent <* skipMany space <* string "=" <* skipMany space) <*> 
                          (Just <$> asts)
                placeholder =
                  (,) <$> lowerCaseIdent <*> pure Nothing
            asts =
              manyTill (decontextedAST expAST) (try (lookAhead (sep <|> end)))


-- * Pattern
-------------------------

patAST :: Parse PatAST
patAST =
  try (record True) <|> (record False)
  where
    record strict =
      fmap (PatAST_Record strict) $
      string opening *> skipMany space *> sepBy1 (Left <$> lowerCaseIdent <|> Right <$> asts) sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        asts =
          manyTill (decontextedAST patAST) (try (lookAhead (sep <|> end)))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


