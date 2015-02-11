module Record.Preprocessor.HSE
(
  E.ParseResult(..),
  Mode(..),
  reifyContexts,
  srcLocToCursorOffset,
)
where

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

srcLocToCursorOffset :: E.SrcLoc -> CursorOffset
srcLocToCursorOffset (E.SrcLoc _ l c) =
  CursorOffset (fromIntegral l) (fromIntegral c)
