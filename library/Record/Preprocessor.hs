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
  ReaderT $ \name -> Parsing.run (Parsing.total (many Parsing.placeholderAST)) name code

-- |
-- Detect contexts of all top-level record splices.
reifyPlaceholderContexts :: [PlaceholderAST] -> Process [Context]
reifyPlaceholderContexts l =
  case HSE.reifyContexts HSE.Mode_Module $ foldMap Rendering.placeholderASTUsingPlaceholders l of
    HSE.ParseOk a -> return a
    HSE.ParseFailed l m -> lift $ Left (correctOffset $ HSE.srcLocToCursorOffset l, m)
  where
    correctOffset o =
      stringCursorOffset $
      foldMap Rendering.placeholderAST $
      catMaybes $
      flip evalState mempty $ forM l $ \ast -> do
        modify $ (<> ((stringCursorOffset . Rendering.placeholderASTUsingPlaceholders) ast))
        o' <- get
        if o' < o
          then return $ Just ast
          else return $ Nothing
      where
        stringCursorOffset =
          (\(CursorOffset l c) -> CursorOffset (pred l) (pred c)) .
          either (error . showString "Unexpected cursor offset parsing error: " . show) id .
          Parsing.run Parsing.cursorOffsetAtEnd ""
      
