module Main where

import BasePrelude
import Record


main =
  return ()


type User = 
  [record| (name :: String, birthday :: (year :: Int, month :: Int, day :: Int)) |]
