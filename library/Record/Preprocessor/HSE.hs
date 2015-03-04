module Record.Preprocessor.HSE
(
  E.ParseResult(..),
  Mode(..),
  reifyLevels,
  srcLocToCursorOffset,
)
where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Language.Haskell.Exts as E
import qualified Data.HashMap.Strict as HashMap
import qualified Record.Preprocessor.HSE.Levels as Levels


data Mode =
  Mode_Module |
  Mode_Type |
  Mode_Exp |
  Mode_Pat

-- |
-- Parses the code using "haskell-src-exts", reifying the AST levels.
reifyLevels :: Mode -> String -> E.ParseResult [Level]
reifyLevels =
  \case
    Mode_Module -> fmap Levels.module_ . E.parseModuleWithMode parseMode
    Mode_Type   -> fmap Levels.type_ . E.parseTypeWithMode parseMode
    Mode_Exp    -> fmap Levels.exp . E.parseExpWithMode parseMode
    Mode_Pat    -> fmap Levels.pat . E.parsePatWithMode parseMode

parseMode :: E.ParseMode
parseMode =
  E.defaultParseMode {
    E.extensions = map E.EnableExtension [minBound .. maxBound]
  }

srcLocToCursorOffset :: E.SrcLoc -> CursorOffset
srcLocToCursorOffset (E.SrcLoc _ l c) =
  CursorOffset (fromIntegral l) (fromIntegral c)
