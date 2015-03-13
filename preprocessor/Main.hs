module Main where

import BasePrelude
import Record.Types
import Record.Lens
import Record.Preprocessor


main =
  do
    string <- readFile "samples/1.hs"
    either print putStrLn $ process "samples/1.hs" string


