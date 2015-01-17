module Record.Parser where

import BasePrelude hiding (takeWhile)
import Data.Text (Text)
import Data.Attoparsec.Text
import qualified Data.Text as T


data Type =
  AppType Type Type |
  VarType Text |
  ConType Text |
  TupleType Int |
  ArrowType |
  ListType |
  RecordType RecordType

type QualifiedName =
  [Text]

type RecordType =
  [(Text, Type)]


run :: Parser a -> Text -> Either String a
run p t =
  parseOnly p t

recordQQ :: Parser RecordType
recordQQ =
  skipSpace *> recordType <* skipSpace <* endOfInput

type' :: Parser Type
type' =
  appType <|> nonAppType
  where
    appType =
      fmap (foldl1 AppType) $
      sepBy1 nonAppType (skipMany1 space)
    nonAppType =
      varType <|> conType <|> tupleConType <|> tupleType <|> listConType <|> 
      arrowType <|> (RecordType <$> recordType) <|> inBraces type'
      where
        varType =
          fmap VarType $ name1
        conType =
          fmap ConType $ name2
        tupleConType =
          fmap TupleType $ 
            char '(' *> (length <$> many1 (skipSpace *> char ',')) <* skipSpace <* char ')'
        tupleType =
          fmap (\l -> foldl AppType (TupleType (length l)) l) $
            char '(' *> skipSpace *>
            sepBy1 type' (skipSpace *> char ',' <* skipSpace)
            <* skipSpace <* char ')'
        listConType =
          ListType <$ string "[]"
        arrowType =
          ArrowType <$ string "->"

recordType :: Parser RecordType
recordType =
  char '(' *> skipSpace *> sepBy1' field (skipSpace *> char ',' <* skipSpace) <* skipSpace <* char ')'
  where
    field =
      (,) <$> (name1 <* skipSpace <* string "::" <* skipSpace) <*> type'

qualifiedName1 :: Parser QualifiedName
qualifiedName1 =
  ((\a b -> a <> pure b) <$> many1 (name2 <* char '.') <*> name1) <|> 
  (pure <$> name1)

inBraces :: Parser a -> Parser a
inBraces p =
  char '(' *> skipSpace *> p <* skipSpace <* char ')'

name1 :: Parser Text
name1 =
  T.cons <$>
    satisfy (\c -> isLower c) <*>
    takeWhile (\c -> isAlphaNum c || c == '\'' || c == '_')

name2 :: Parser Text
name2 =
  T.cons <$>
    satisfy (\c -> isUpper c) <*>
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
