module Record.Preprocessor.HSE
(
  E.ParseResult(..),
  reifyLevels,
  srcLocToCursorOffset,
)
where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Language.Haskell.Exts as E
import qualified Data.HashMap.Strict as HashMap
import qualified Record.Preprocessor.HSE.Levels as Levels


-- |
-- Parses the code using "haskell-src-exts", reifying the AST levels.
reifyLevels :: Level -> String -> E.ParseResult [Level]
reifyLevels =
  \case
    Level_Decl   -> fmap Levels.module_ . E.parseModuleWithMode parseMode
    Level_Type   -> fmap Levels.type_ . E.parseTypeWithMode parseMode
    Level_Exp    -> fmap Levels.exp . E.parseExpWithMode parseMode
    Level_Pat    -> fmap Levels.pat . E.parsePatWithMode parseMode

parseMode :: E.ParseMode
parseMode =
  E.defaultParseMode {
    E.extensions = map E.EnableExtension [minBound .. maxBound]
  }

srcLocToCursorOffset :: E.SrcLoc -> CursorOffset
srcLocToCursorOffset (E.SrcLoc _ l c) =
  CursorOffset (fromIntegral l) (fromIntegral c)
