module Record.Preprocessor where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Record.Preprocessor.Parse as Parse
import qualified Record.Preprocessor.Rendering as Rendering
import qualified Record.Preprocessor.HSE as HSE
import qualified Record.Preprocessor.Position as Position


process :: String -> String -> Either Error String
process name code =
  flip runReaderT name $ 
    parseModule code >>=
    reifyExtensionForest Level_Decl >>=
    return . foldMap (Rendering.haskell Rendering.extension)

type Error =
  (Position.Position, String)
  
type Process =
  ReaderT String (Either Error)


parse :: Parse.Parse a -> String -> Process a
parse p code =
  ReaderT $ \name -> Parse.run p name code

parseModule :: String -> Process (HaskellForest Placeholder)
parseModule =
  parse (Parse.total (many (Parse.haskell Parse.placeholder)))

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
      foldMap (Rendering.haskell (Rendering.unleveledExtension . snd)) $
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
  traverse (reifyExtensionForest Level_Exp)

reifyExtensionForest :: Level -> HaskellForest Placeholder -> Process (HaskellForest Extension)
reifyExtensionForest level forest =
  do
    sublevels <- reifyLevels level forest
    flip evalStateT sublevels $ flip (traverse . traverse) forest $ \placeholder -> StateT $ \case
      head : tail -> liftM (, tail) (reifyExtension head placeholder)
      _ -> error "Unexpected end of sublevels"

reifyExtension :: Level -> Placeholder -> Process Extension
reifyExtension level (position, tree) =
  let 
    code = Rendering.unleveledExtension tree
    in case level of
      Level_Exp -> return . Extension_Exp =<< reifyExp =<< parse Parse.exp code
      Level_Type -> Extension_Type <$> parse Parse.type_ code 
