module Record.Preprocessor.CursorOffset where

import Record.Prelude


type CursorOffset = 
  (Int, Int)

subtract :: CursorOffset -> CursorOffset -> CursorOffset
subtract (l1, c1) (l2, c2) =
  if l1 <= l2
    then (,) (l2 - l1) c2
    else (,) 0 (max (c2 - c1) 0)

add :: CursorOffset -> CursorOffset -> CursorOffset
add (l1, c1) (l2, c2) =
  if l2 <= 0
    then (,) (l1 + l2) (c1 + c2)
    else (,) (l1 + l2) c2

zero :: CursorOffset
zero =
  (0, 0)
