module Record.Parser where

import BasePrelude hiding (takeWhile, exp)
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
      varType <|> conType <|> tupleConType <|> tupleType <|> listConType <|> listType <|>
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
        listType =
          fmap (Type_App Type_List) $
            char '[' *> skipSpace *> type' <* skipSpace <* char ']'
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

symbolicIdent :: Parser Text
symbolicIdent =
  labeled "symbolicIdent" $
    (\t -> "(" <> t <> ")") <$> (char '(' *> takeWhile1 isSymbol <* char ')')
  where
    isSymbol =
      flip elem ("!#$%&*+./<=>?@\\^|-~" :: [Char])

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
    sepBy1 (lowerCaseName <|> takeWhile1 isDigit) (char '.')


data Exp =
  Exp_Record RecordExp |
  Exp_Var Text |
  Exp_Con Text |
  Exp_TupleCon Int |
  Exp_Nil |
  Exp_Lit Lit |
  Exp_App Exp Exp |
  Exp_List [Exp] |
  Exp_Sig Exp Type

type RecordExp =
  [(Text, Exp)]

data Lit =
  Lit_Char Char |
  Lit_String Text |
  Lit_Integer Integer |
  Lit_Rational Rational

exp :: Parser Exp
exp =
  labeled "exp" $
    sig <|> nonSig
  where
    sig =
      Exp_Sig <$> nonSig <*> (skipSpace *> string "::" *> skipSpace *> type')
    nonSig =
      app <|> nonApp
      where
        app =
          fmap (foldl1 Exp_App) $
          sepBy1 nonApp (skipMany1 space)
        nonApp =
          record <|>
          var <|>
          con <|>
          tupleCon <|>
          nil <|>
          Exp_Lit <$> lit <|>
          tuple <|>
          list <|>
          inBraces exp
          where
            record =
              Exp_Record <$> (char '{' *> skipSpace *> fields <* skipSpace <* char '}')
              where
                fields =
                  sepBy1 field (skipSpace *> char ',' <* skipSpace)
                  where
                    field =
                      (,) <$> lowerCaseName <*> (skipSpace *> char '=' *> skipSpace *> exp)
            var =
              Exp_Var <$> (lowerCaseName <|> symbolicIdent)
            con =
              Exp_Con <$> (upperCaseName <|> symbolicIdent)
            tupleCon =
              Exp_TupleCon . length <$> 
              (char '(' *> many1 (char ',') <* char ')')
            nil =
              Exp_Nil <$ string "[]"
            tuple =
              fmap (\l -> foldl Exp_App (Exp_TupleCon (length l)) l) $
                char '(' *> skipSpace *>
                sepBy1 exp (skipSpace *> char ',' <* skipSpace)
                <* skipSpace <* char ')'
            list =
              fmap Exp_List $
                char '[' *> skipSpace *> 
                  sepBy1 exp (skipSpace *> char ',' <* skipSpace) <*
                  skipSpace <* char ']'

lit :: Parser Lit
lit =
  Lit_Char <$> charLit <|>
  Lit_String <$> stringLit <|>
  Lit_Rational <$> rational <|>
  Lit_Integer <$> decimal


