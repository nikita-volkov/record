-- |
-- A set of composable string parsers.
module Record.Preprocessor.Parse where

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
      CursorOffset (pred $ fromIntegral $ sourceLine p) (pred $ fromIntegral $ sourceColumn p)

labeled :: String -> Parse a -> Parse a
labeled =
  flip (<?>)

total :: Parse a -> Parse a
total =
  (<* eof)  

withCursorOffset :: Parse a -> Parse (CursorOffset, a)
withCursorOffset p =
  compose <$> cursorOffset <*> p <*> cursorOffset
  where
    compose start a end =
      (diffCursorOffset start end, a)
    diffCursorOffset (CursorOffset l1 c1) (CursorOffset l2 c2) =
      if l1 <= l2
        then CursorOffset (l2 - l1) c2
        else CursorOffset 0 (max (c2 - c1) 0)

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


-- * Decontexted
-------------------------

decontexted :: Parse a -> Parse (Decontexted a)
decontexted injection =
  (try $ Decontexted_Injection <$> injection) <|>
  (try $ Decontexted_StringLit <$> stringLit) <|>
  (try $ Decontexted_QuasiQuote <$> quasiQuote) <|>
  (try $ Decontexted_InCurlies <$> enclosedIn '{' '}') <|>
  (try $ Decontexted_InRoundies <$> enclosedIn '(' ')') <|>
  (try $ Decontexted_InSquarelies <$> enclosedIn '[' ']') <|>
  (Decontexted_Char <$> anyChar)
  where
    enclosedIn opening closing =
      char opening *> manyTill (decontexted injection) (try (char closing))


-- *
-------------------------

unleveled :: Parse Unleveled
unleveled =
  (try (Unleveled_InLazyBraces <$> between (string "(~") (string "~)"))) <|>
  (try (Unleveled_InStrictBraces <$> between (string "(!") (string "!)")))
  where
    between opening closing =
      opening *> manyTill (decontexted unleveled) (try closing)


-- *
-------------------------

placeholder :: Parse Placeholder
placeholder =
  (,) <$> cursorOffset <*> unleveled


-- * Decontexted Type
-------------------------

type_ :: Parse Type
type_ =
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
                  manyTill (decontexted type_) (try (lookAhead (sep <|> end)))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


-- * Expression
-------------------------

exp :: Parse (Exp Unleveled)
exp =
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
              manyTill (decontexted unleveled) (try (lookAhead (sep <|> end)))


-- * Pattern
-------------------------

pat :: Parse Pat
pat =
  try (record True) <|> (record False)
  where
    record strict =
      fmap (Pat_Record strict) $
      string opening *> skipMany space *> sepBy1 (Left <$> lowerCaseIdent <|> Right <$> asts) sep <* end
      where
        (opening, closing) = 
          if strict then ("(!", "!)") else ("(~", "~)")
        asts =
          manyTill (decontexted pat) (try (lookAhead (sep <|> end)))
        sep =
          skipMany space <* char ',' <* skipMany space
        end =
          skipMany space <* string closing


