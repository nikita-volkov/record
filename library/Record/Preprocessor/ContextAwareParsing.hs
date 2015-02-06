module Record.Preprocessor.ContextAwareParsing where

import Record.Prelude hiding (takeWhile, exp, try, many)
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parsing as Parsing


type Parse = 
  StateT [Context] Parsing.Parse

run :: Parse a -> [Context] -> Parsing.Parse a
run =
  undefined

fetchContext :: Parse (Maybe Context)
fetchContext =
  state $ uncons >>> maybe (Nothing, []) (Just *** id)
