module Main where

import BasePrelude
import Record.Types
import Record.Lens
import Record.Preprocessor


main =
  do
    string <- readFile "samples/1.hs"
    print $ process "samples/1.hs" string


