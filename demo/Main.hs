module Main where

import BasePrelude
import Record
import Record.Lens


main =
  return ()


type User = 
  [record| {name :: String, birthday :: {year :: Int, month :: Int, day :: Int}} |]

getUserBirthdayYear :: User -> Int
getUserBirthdayYear =
  view [lens|birthday.year|]

setUserBirthdayYear :: Int -> User -> User
setUserBirthdayYear =
  set [lens|birthday.year|]

userBirthdayYearLens :: Lens User Int
userBirthdayYearLens =
  [lens|birthday.year|]

userBirthdayYearLens' :: Lens User Int
userBirthdayYearLens' =
  [lens|birthday|] . [lens|year|]

