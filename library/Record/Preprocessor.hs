module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parsing as Parsing
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE


process :: String -> String -> Either Error String
process =
  undefined

type Error =
  (CursorOffset, String)
  
type Process =
  ReaderT String (Either Error)


parsePlaceholders :: String -> Process [PlaceholderAST]
parsePlaceholders code =
  ReaderT $ \name -> Parsing.run Parsing.placeholderASTs name code

-- |
-- Detect contexts of all top-level record splices.
reifyPlaceholderContexts :: [PlaceholderAST] -> Process [Context]
reifyPlaceholderContexts l =
  case HSE.reifyContexts HSE.Mode_Module $ foldMap Rendering.placeholderASTUsingPlaceholders l of
    HSE.ParseOk a -> return a
    HSE.ParseFailed l m -> lift $ Left (correctOffset $ HSE.srcLocToCursorOffset l, m)
  where
    correctOffset o =
      (<> o) $
      mconcat $
      map (<> CursorOffset 0 (-1)) $
      flip evalStateT mempty $ do
        ast <- lift $ l
        modify $ \o' -> 
          (o' <>) $
          either (error . showString "Unexpected parsing error: " . show) id $
          Parsing.run Parsing.cursorOffsetAtEnd "" $
          Rendering.placeholderAST ast
        o' <- get
        unless (o' < o) mzero
        lift $ case ast of
          PlaceholderAST_InCurlies o'' _ -> [o'']
          _ -> []



