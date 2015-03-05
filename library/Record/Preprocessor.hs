module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parsing as Parsing
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE


process :: String -> String -> Either Error String
process name code =
  flip runReaderT name $ do
    asts <- parseUnleveleds code
    levels <- reifyUnleveledLevels asts
    undefined

type Error =
  (CursorOffset, String)
  
type Process =
  ReaderT String (Either Error)


parseUnleveleds :: String -> Process [Decontexted Unleveled]
parseUnleveleds code =
  ReaderT $ \name -> Parsing.run (Parsing.total (many (Parsing.decontexted Parsing.unleveled))) name code

-- |
-- Detect levels of all top-level record splices.
reifyUnleveledLevels :: [Decontexted Unleveled] -> Process [Level]
reifyUnleveledLevels l =
  case HSE.reifyLevels HSE.Mode_Module $ foldMap (Rendering.decontexted (const "Ѣ")) l of
    HSE.ParseOk a -> return a
    HSE.ParseFailed l m -> lift $ Left (correctOffset $ HSE.srcLocToCursorOffset l, m)
  where
    correctOffset o =
      stringCursorOffset $
      foldMap (Rendering.decontexted Rendering.unleveled) $
      catMaybes $
      flip evalState mempty $ forM l $ \ast -> do
        modify $ (<> ((stringCursorOffset . Rendering.decontexted (const "Ѣ")) ast))
        o' <- get
        if o' < o
          then return $ Just ast
          else return $ Nothing
      where
        stringCursorOffset =
          (\(CursorOffset l c) -> CursorOffset (pred l) (pred c)) .
          either (error . showString "Unexpected cursor offset parsing error: " . show) id .
          Parsing.run Parsing.cursorOffsetAtEnd ""
      
reifyExpLevels :: Exp Unleveled -> Process [Level]
reifyExpLevels =
  \case
    Exp_Record strict (RecordExpBody_Named sections) ->
      fmap concat . mapM reifyUnleveledLevels . catMaybes . map snd $ sections
