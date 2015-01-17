module Main where

import BasePrelude
import Record
import Record.Lens


main =
  return ()


type Person = 
  [record| {name :: String, birthday :: {year :: Int, month :: Int, day :: Int}} |]

getPersonBirthdayYear :: Person -> Int
getPersonBirthdayYear =
  view [lens|birthday.year|]

setPersonBirthdayYear :: Int -> Person -> Person
setPersonBirthdayYear =
  set [lens|birthday.year|]

personBirthdayYearLens :: Lens Person Int
personBirthdayYearLens =
  [lens|birthday.year|]

personBirthdayYearLens' :: Lens Person Int
personBirthdayYearLens' =
  [lens|birthday|] . [lens|year|]

