-- |
-- A set of composable string parsers.
module Record.Parser.Parsers where

import BasePrelude hiding (takeWhile, exp)
import Data.Text (Text)
import Data.Attoparsec.Text
import qualified Data.Text as T


-- * General stuff
-------------------------

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


-- *
-------------------------

skipStringLiteral :: Parser ()
skipStringLiteral =
  quoted '"' <|> fail "Not a string literal"
  where 
    quoted q = 
      char q *> content <* char q
      where
        content =
          skipMany char'
          where
            char' = 
              (char '\\' *> (char q <|> char '\\')) <|>
              (notChar q)

skipQuasiQuote :: Parser ()
skipQuasiQuote =
  undefined
