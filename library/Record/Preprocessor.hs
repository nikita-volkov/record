module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parsing as Parsing
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE


process :: String -> String -> Either String String
process =
  undefined

  
type Process =
  ReaderT String (Either String)

parse :: Parsing.Parse a -> String -> Process a
parse p s =
  ReaderT $ \name -> Parsing.run p name s

-- |
-- Detect contexts of all top-level record splices.
contexts :: String -> Process [Context]
contexts =
  parse Parsing.contextASF >=>
  lift . HSE.runParseResult . HSE.reifyContexts HSE.Mode_Module . Rendering.contextASF
      






