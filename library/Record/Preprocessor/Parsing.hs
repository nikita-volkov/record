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


-- * GeneralAST
-------------------------

generalAST :: Parse a -> Parse (GeneralAST a)
generalAST injection =
  (try $ GeneralAST_Injection <$> injection) <|>
  (try $ GeneralAST_StringLit <$> stringLit) <|>
  (try $ GeneralAST_QuasiQuote <$> quasiQuote) <|>
  (try $ GeneralAST_InCurlies <$> enclosedIn '{' '}') <|>
  (try $ GeneralAST_InRoundies <$> enclosedIn '(' ')') <|>
  (try $ GeneralAST_InSquarelies <$> enclosedIn '[' ']') <|>
  (GeneralAST_Char <$> anyChar)
  where
    enclosedIn opening closing =
      char opening *> manyTill (generalAST injection) (try (char closing))


-- *
-------------------------

placeholder :: Parse Placeholder
placeholder =
  (try (Placeholder_InLazyBraces <$> between (string "(~") (string "~)"))) <|>
  (try (Placeholder_InStrictBraces <$> between (string "(!") (string "!)")))
  where
    between opening closing =
      opening *> manyTill (generalAST placeholder) (try closing)


-- * TypeAST
-------------------------

typeExtension :: Parse TypeExtension
typeExtension =
  try (record True) <|> (record False)
  where
    record strict =
      fmap (TypeExtension_Record strict) $
      string opening *> skipMany space *> sepBy1 field sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        field =
          (,) <$> (lowerCaseIdent <* skipMany space <* string "::" <* skipMany space) <*> 
                  manyTill (generalAST typeExtension) (try (lookAhead (sep <|> end)))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


-- * Expression
-------------------------

expExtension :: Parse ExpExtension
expExtension =
  try (record True) <|> (record False)
  where
    record strict =
      fmap (ExpExtension_Record strict) $
      string opening *> skipMany space *> sepBy1 (try assignment <|> placeholder) sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        assignment =
          (,) <$> (lowerCaseIdent <* skipMany space <* string "=" <* skipMany space) <*> 
                  (Just <$> manyTill (generalAST expExtension) (try (lookAhead (sep <|> end))))
        placeholder =
          (,) <$> lowerCaseIdent <*> pure Nothing
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


-- * Pattern
-------------------------

patExtension :: Parse PatExtension
patExtension =
  try (record True) <|> (record False)
  where
    record strict =
      fmap (PatExtension_Record strict) $
      string opening *> skipMany space *> sepBy1 (Left <$> lowerCaseIdent <|> Right <$> asts) sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        asts =
          manyTill (generalAST patExtension) (try (lookAhead (sep <|> end)))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


