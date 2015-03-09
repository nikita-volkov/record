module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parse as Parse
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE


process :: String -> String -> Either Error String
process name code =
  flip runReaderT name $ do
    asts <- parse code
    levels <- reifyLevels Level_Decl asts
    undefined

type Error =
  (CursorOffset, String)
  
type Process =
  ReaderT String (Either Error)


parse :: String -> Process [Decontexted Placeholder]
parse code =
  ReaderT $ \name -> Parse.run (Parse.total (many (Parse.decontexted Parse.placeholder))) name code

-- |
-- Detect levels of all top-level record splices.
reifyLevels :: Level -> [Decontexted Placeholder] -> Process [Level]
reifyLevels level l =
  case HSE.reifyLevels level $ foldMap (Rendering.decontexted (const "Ѣ")) l of
    HSE.ParseOk a -> return a
    HSE.ParseFailed l m -> lift $ Left (correctOffset $ HSE.srcLocToCursorOffset l, m)
  where
    correctOffset o =
      stringCursorOffset $
      foldMap (Rendering.decontexted (Rendering.unleveled . snd)) $
      catMaybes $
      flip evalState mempty $ forM l $ \ast -> do
        modify $ (<> ((stringCursorOffset . Rendering.decontexted (const "Ѣ")) ast))
        o' <- get
        if o' < o
          then return $ Just ast
          else return $ Nothing
      where
        stringCursorOffset =
          either (error . showString "Unexpected cursor offset parsing error: " . show) id .
          Parse.run Parse.cursorOffsetAtEnd ""
      
reifyExpLevels :: Exp Placeholder -> Process [Level]
reifyExpLevels =
  \case
    Exp_Record strict (RecordExpBody_Named sections) ->
      fmap concat . mapM (reifyLevels Level_Exp) . catMaybes . map snd $ sections


