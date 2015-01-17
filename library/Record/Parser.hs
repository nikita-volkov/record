module Record.Parser where

import BasePrelude hiding (takeWhile)
import Data.Text (Text)
import Data.Attoparsec.Text
import qualified Data.Text as T


data Type =
  Type_App Type Type |
  Type_Var Text |
  Type_Con Text |
  Type_Tuple Int |
  Type_Arrow |
  Type_List |
  Type_Record RecordType

type QualifiedName =
  [Text]

type RecordType =
  [(Text, Type)]


run :: Parser a -> Text -> Either String a
run p t =
  onResult $ parse p t
  where
    onResult =
      \case
        Fail _ contexts message -> Left $ showString message . showString ". Contexts: " .
                                          shows contexts $ "."
        Done _ a -> Right a
        Partial c -> onResult (c "")

labeled :: String -> Parser a -> Parser a
labeled =
  flip (<?>)

qq :: Parser a -> Parser a
qq p =
  skipSpace *> p <* skipSpace <* endOfInput

type' :: Parser Type
type' =
  labeled "type'" $
  appType <|> nonAppType
  where
    appType =
      fmap (foldl1 Type_App) $
      sepBy1 nonAppType (skipMany1 space)
    nonAppType =
      varType <|> conType <|> tupleConType <|> tupleType <|> listConType <|> 
      arrowType <|> (Type_Record <$> recordType) <|> inBraces type'
      where
        varType =
          fmap Type_Var $ lowerCaseName
        conType =
          fmap Type_Con $ upperCaseName
        tupleConType =
          fmap Type_Tuple $ 
            char '(' *> (length <$> many1 (skipSpace *> char ',')) <* skipSpace <* char ')'
        tupleType =
          fmap (\l -> foldl Type_App (Type_Tuple (length l)) l) $
            char '(' *> skipSpace *>
            sepBy1 type' (skipSpace *> char ',' <* skipSpace)
            <* skipSpace <* char ')'
        listConType =
          Type_List <$ string "[]"
        arrowType =
          Type_Arrow <$ string "->"

recordType :: Parser RecordType
recordType =
  labeled "recordType" $
    char '{' *> skipSpace *> sepBy1' field (skipSpace *> char ',' <* skipSpace) <* skipSpace <* char '}'
  where
    field =
      (,) <$> (lowerCaseName <* skipSpace <* string "::" <* skipSpace) <*> type'

qualifiedName1 :: Parser QualifiedName
qualifiedName1 =
  ((\a b -> a <> pure b) <$> many1 (upperCaseName <* char '.') <*> lowerCaseName) <|> 
  (pure <$> lowerCaseName)

inBraces :: Parser a -> Parser a
inBraces p =
  char '(' *> skipSpace *> p <* skipSpace <* char ')'

lowerCaseName :: Parser Text
lowerCaseName =
  labeled "lowerCaseName" $
    T.cons <$>
      satisfy (\c -> isLower c || c == '_') <*>
      takeWhile (\c -> isAlphaNum c || c == '\'' || c == '_')

upperCaseName :: Parser Text
upperCaseName =
  labeled "upperCaseName" $
    T.cons <$>
      satisfy (\c -> isUpper c || c == '_') <*>
      takeWhile (\c -> isAlphaNum c || c == '\'' || c == '_')

stringLit :: Parser Text
stringLit =
  quoted '"'
  where 
    quoted q = 
      T.pack <$> (char q *> content <* char q)
      where
        content =
          many char'
          where
            char' = 
              (char '\\' *> (char q <|> char '\\')) <|>
              (notChar q)

charLit :: Parser Char
charLit =
  char '\'' *> ((char '\\' *> (char '\'' <|> char '\\')) <|> notChar '\'') <* char '\''


type Lens =
  [Text]

lens :: Parser Lens
lens =
  labeled "lens" $
    sepBy1 lowerCaseName (char '.')

