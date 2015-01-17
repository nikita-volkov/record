module Main where

import BasePrelude
import Record
import Record.Lens


main =
  return ()


type User = 
  [record| {name :: String, birthday :: {year :: Int, month :: Int, day :: Int}} |]

userBirthdayYearLens =
  [lens| birthday.year |]
