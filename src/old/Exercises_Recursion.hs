module Exercises_Recursion where

reverseList = go []
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs

reverseWord :: String -> String
reverseWord "" = ""
reverseWord word = ""
