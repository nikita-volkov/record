module Record.Preprocessor.LevelAwareParsing where

import Record.Prelude hiding (takeWhile, exp, try, many)
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parsing as Parsing


type Parse = 
  StateT [Level] Parsing.Parse

run :: Parse a -> [Level] -> Parsing.Parse a
run =
  undefined

fetchLevel :: Parse (Maybe Level)
fetchLevel =
  state $ uncons >>> maybe (Nothing, []) (Just *** id)
