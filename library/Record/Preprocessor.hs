module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parse as Parse
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE
import qualified Record.Preprocessor.Position as Position


process :: String -> String -> Either Error String
process name code =
  flip runReaderT name $ do
    asts <- parse code
    levels <- reifyLevels Level_Decl asts
    undefined

type Error =
  (Position.Position, String)
  
type Process =
  ReaderT String (Either Error)


parse :: String -> Process (HaskellForest Placeholder)
parse code =
  ReaderT $ \name -> Parse.run (Parse.total (many (Parse.haskell Parse.placeholder))) name code

-- |
-- Detect levels of all top-level record splices.
reifyLevels :: Level -> (HaskellForest Placeholder) -> Process [Level]
reifyLevels level l =
  case HSE.reifyLevels level $ foldMap (Rendering.haskell (const "Ѣ")) l of
    HSE.ParseOk a -> return a
    HSE.ParseFailed l m -> lift $ Left (correctOffset $ HSE.srcLocToCursorOffset l, m)
  where
    correctOffset o =
      stringCursorOffset $
      foldMap (Rendering.haskell (Rendering.unleveled . snd)) $
      catMaybes $
      flip evalState Position.zero $ forM l $ \ast -> do
        modify $ (Position.add ((stringCursorOffset . Rendering.haskell (const "Ѣ")) ast))
        o' <- get
        if o' < o
          then return $ Just ast
          else return $ Nothing
      where
        stringCursorOffset =
          either (error . showString "Unexpected cursor offset parsing error: " . show) id .
          Parse.run Parse.cursorOffsetAtEnd ""

reifyExp :: Exp (HaskellForest Placeholder) -> Process (Exp (HaskellForest Extension))
reifyExp =
  traverse (reifyExtension Level_Exp)

reifyExtension :: Level -> HaskellForest Placeholder -> Process (HaskellForest Extension)
reifyExtension =
  undefined
