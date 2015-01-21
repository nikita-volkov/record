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
  deriving (Show)

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

-- |
-- Run a parser on a given input,
-- lifting its errors to the context parser.
-- 
-- Consider it a subparser.
parser :: Parser a -> Text -> Parser a
parser p t =
  either fail return $
  run p t

labeled :: String -> Parser a -> Parser a
labeled =
  flip (<?>)

qq :: Parser a -> Parser a
qq p =
  skipSpace *> p <* skipSpace <* endOfInput

-- |
-- >>> run type' "(Int, Char)"
-- Right (Type_App (Type_App (Type_Tuple 2) (Type_Con "Int")) (Type_Con "Char"))
-- 
-- >>> run type' "(,) Int Int"
-- Right (Type_App (Type_App (Type_Tuple 2) (Type_Con "Int")) (Type_Con "Int"))
-- 
-- >>> run type' "(,)"
-- Right (Type_Tuple 2)
-- 
-- >>> run type' "()"
-- Right (Type_Tuple 0)
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
          Type_Tuple 0 <$ string "()" <|>
          Type_Tuple . succ . length <$> (char '(' *> many (char ',') <* char ')')
        tupleType =
          do
            char '('
            skipSpace
            h <- type' <* skipSpace <* char ',' <* skipSpace
            t <- sepBy1 type' (skipSpace *> char ',' <* skipSpace)
            skipSpace
            char ')'
            return $ foldl Type_App (Type_Tuple (1 + length t)) $ h : t
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
  deriving (Show)

type RecordExp =
  [(Text, Exp)]

data Lit =
  Lit_Char Char |
  Lit_String Text |
  Lit_Integer Integer |
  Lit_Rational Rational
  deriving (Show)

-- |
-- 
-- >>> run exp "(,)"
-- Right (Exp_TupleCon 2)
-- 
-- >>> run exp "(1,2)"
-- Right (Exp_App (Exp_App (Exp_TupleCon 2) (Exp_Lit (Lit_Integer 1))) (Exp_Lit (Lit_Integer 2)))
-- 
-- >>> run exp "(1)"
-- Right (Exp_Lit (Lit_Integer 1))
-- 
-- >>> run exp "()"
-- Right (Exp_TupleCon 0)
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
              Exp_TupleCon 0 <$ string "()" <|>
              Exp_TupleCon . succ . length <$> (char '(' *> many (char ',') <* char ')')
            nil =
              Exp_Nil <$ string "[]"
            tuple =
              do
                char '('
                skipSpace
                h <- exp <* skipSpace <* char ',' <* skipSpace
                t <- sepBy1 exp (skipSpace *> char ',' <* skipSpace)
                skipSpace
                char ')'
                return $ foldl Exp_App (Exp_TupleCon (1 + length t)) $ h : t
            list =
              fmap Exp_List $
                char '[' *> skipSpace *> 
                  sepBy1 exp (skipSpace *> char ',' <* skipSpace) <*
                  skipSpace <* char ']'

-- |
-- 
-- Integers get parsed as integers:
-- 
-- >>> run lit "2"
-- Right (Lit_Integer 2)
-- 
-- Rationals get parsed as rationals:
-- 
-- >>> run lit "2.0"
-- Right (Lit_Rational (2 % 1))
-- 
-- >>> run lit "3e2"
-- Right (Lit_Rational (300 % 1))
lit :: Parser Lit
lit =
  Lit_Char <$> charLit <|>
  Lit_String <$> stringLit <|>
  Lit_Rational <$> rationalNotDecimal <|>
  Lit_Integer <$> decimal
  where
    rationalNotDecimal =
      match rational >>= \(t, r) ->
        case run (decimal <* endOfInput) t of
          Left _ -> return r
          _ -> mzero

