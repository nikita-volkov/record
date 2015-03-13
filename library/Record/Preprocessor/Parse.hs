-- |
-- A set of composable string parsers.
module Record.Preprocessor.Parse where

import Record.Prelude hiding (takeWhile, exp, try, many)
import Record.Preprocessor.Model
import Text.Parsec hiding ((<|>))
import Text.Parsec.Error
import qualified Record.Preprocessor.Position as Position


-- * General stuff
-------------------------

type Parse = 
  Parsec String ()

type Error =
  (Position.Position, String)

run :: Parse a -> String -> String -> Either Error a
run p n =
  either (Left . (cursorOffset . errorPos &&& intercalate "; " . fmap messageString . errorMessages)) Right .
  parse p n
  where
    cursorOffset p =
      (,) (pred $ fromIntegral $ sourceLine p) (pred $ fromIntegral $ sourceColumn p)

labeled :: String -> Parse a -> Parse a
labeled =
  flip (<?>)

total :: Parse a -> Parse a
total =
  (<* eof)  

withCursorOffset :: Parse a -> Parse (Position.Position, a)
withCursorOffset p =
  compose <$> cursorOffset <*> p <*> cursorOffset
  where
    compose start a end =
      (Position.subtract start end, a)

-- *
-------------------------

cursorOffset :: Parse Position.Position
cursorOffset =
  labeled "cursorOffset" $
  flip fmap getPosition $ \p ->
    (,) (pred $ fromIntegral $ sourceLine p) (pred $ fromIntegral $ sourceColumn p)

cursorOffsetAtEnd :: Parse Position.Position
cursorOffsetAtEnd =
  skipMany anyChar *> cursorOffset

charLit :: Parse String
charLit =
  labeled "Char Literal" $
  char '\'' *> content <* char '\''
  where
    content =
      try escapeSequence <|> 
      fmap pure (noneOf "'\\")
      where
        escapeSequence =
          ('\\' :) <$> (char '\\' *> (fmap pure (char '\'') <|> many1 sequenceChar))
          where
            sequenceChar =
              satisfy $ \c -> c /= '\'' && c /= '\\' && not (isSpace c)

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
  labeled "lowerCaseQualifiedIdent" $
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


-- * Haskell
-------------------------

haskell :: Parse a -> Parse (Haskell a)
haskell injection =
  labeled "haskell" $
  (try $ Haskell_Extension <$> injection) <|>
  (try $ Haskell_CharLit <$> charLit) <|>
  (try $ Haskell_StringLit <$> stringLit) <|>
  (try $ Haskell_QuasiQuote <$> quasiQuote) <|>
  (try $ Haskell_InCurlies <$> enclosedIn '{' '}') <|>
  (try $ Haskell_InRoundies <$> enclosedIn '(' ')') <|>
  (try $ Haskell_InSquarelies <$> enclosedIn '[' ']') <|>
  (Haskell_Char <$> anyChar)
  where
    enclosedIn opening closing =
      char opening *> manyTill (haskell injection) (try (char closing))


-- *
-------------------------

unleveledExtension :: Parse UnleveledExtension
unleveledExtension =
  labeled "unleveledExtension" $
  (try (UnleveledExtension_InBraces False <$> between (string "(~") (string "~)"))) <|>
  (UnleveledExtension_InBraces True <$> between (string "(!") (string "!)"))
  where
    between opening closing =
      opening *> manyTill (haskell unleveledExtension) (try closing)


-- *
-------------------------

placeholder :: Parse Placeholder
placeholder =
  labeled "placeholder" $
  (,) <$> cursorOffset <*> unleveledExtension


-- * Haskell Type
-------------------------

type_ :: Parse Type
type_ =
  labeled "type_" $
  try (record True) <|> (record False)
  where
    record strict =
      fmap (Type_Record strict) $
      string opening *> skipMany space *> sepBy1 field sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        field =
          (,) <$> (lowerCaseIdent <* skipMany space <* string "::" <* skipMany space) <*> 
                  manyTill (haskell type_) (lookAhead (try sep <|> try end))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


-- * Expression
-------------------------

exp :: Parse (Exp (HaskellForest Placeholder))
exp =
  labeled "exp" $
  try (record True) <|> (record False)
  where
    record strict =
      fmap (Exp_Record strict) $
      string opening *> skipMany space *> body <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing
        body =
          sepBy1 (try assignment <|> nonAssignment) sep
          where
            assignment =
              (,) <$> (lowerCaseIdent <* skipMany space <* string "=" <* skipMany space) <*> 
                      (Just <$> asts)
            nonAssignment =
              (,) <$> lowerCaseIdent <*> pure Nothing
            asts =
              manyTill (haskell placeholder) (lookAhead (try sep <|> try end))


-- * Pattern
-------------------------

pat :: Parse Pat
pat =
  labeled "pat" $
  try (record True) <|> (record False)
  where
    record strict =
      fmap (Pat_Record strict) $
      string opening *> skipMany space *> sepBy1 (Left <$> lowerCaseIdent <|> Right <$> asts) sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        asts =
          manyTill (haskell pat) (try (lookAhead (sep <|> end)))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing

