module Record.Preprocessor.Rendering.MeasuredString where

import Record.Prelude
import qualified Record.Preprocessor.Parse as Parse
import qualified Record.Preprocessor.CursorOffset as CursorOffset


type MeasuredString =
  (CursorOffset.CursorOffset, String)

string :: String -> MeasuredString
string = 
  measure &&& id
  where
    measure =
      either (error . showString "Unexpected cursor offset parsing error: " . show) id .
      Parse.run Parse.cursorOffsetAtEnd ""

space :: CursorOffset.CursorOffset -> MeasuredString
space offset@(rows, columns) =
  (offset, string)
  where
    string = replicate rows '\n' <> replicate columns ' '

