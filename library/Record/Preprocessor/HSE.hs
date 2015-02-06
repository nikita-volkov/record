module Record.Preprocessor.HSE where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Language.Haskell.Exts as E
import qualified Data.HashMap.Strict as HashMap
import qualified Record.Preprocessor.HSE.Contexts as Contexts


data Mode =
  Mode_Module |
  Mode_Type |
  Mode_Exp |
  Mode_Pat

runParseResult :: E.ParseResult a -> Either String a
runParseResult =
  \case
    E.ParseOk a -> return a
    E.ParseFailed l m -> Left $ m <> "; Location: " <> show l

-- |
-- Parses the code using "haskell-src-exts", reifying the AST contexts.
reifyContexts :: Mode -> String -> E.ParseResult [Context]
reifyContexts =
  \case
    Mode_Module -> fmap Contexts.module_ . E.parseModuleWithMode parseMode
    Mode_Type   -> fmap Contexts.type_ . E.parseTypeWithMode parseMode
    Mode_Exp    -> fmap Contexts.exp . E.parseExpWithMode parseMode
    Mode_Pat    -> fmap Contexts.pat . E.parsePatWithMode parseMode

parseMode :: E.ParseMode
parseMode =
  E.defaultParseMode {
    E.extensions = map E.EnableExtension [minBound .. maxBound]
  }
