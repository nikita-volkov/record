module Record.Preprocessor.Position where

import Record.Prelude


type Position = 
  (Int, Int)

subtract :: Position -> Position -> Position
subtract (l1, c1) (l2, c2) =
  if l1 <= l2
    then (,) (l2 - l1) c2
    else (,) 0 (max (c2 - c1) 0)

add :: Position -> Position -> Position
add (l1, c1) (l2, c2) =
  if l2 <= 0
    then (,) (l1 + l2) (c1 + c2)
    else (,) (l1 + l2) c2

zero :: Position
zero =
  (0, 0)
