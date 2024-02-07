module Lib
  ( someFunc,
  )
where

import Data.Char (toUpper)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Writer a = (a, String)

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \x ->
  let (y, s1) = m1 x
      (z, s2) = m2 y
   in (z, s1 ++ s2)

return :: a -> Writer a
return x = (x, "")

upCase :: String -> Writer String
upCase s = (map toUpper s, "upCase ")

--toWords :: String -> Writer [String]
--toWords s = (words s, "toWords ")
--dropWhile

isSpace :: Char -> Bool
isSpace char
  | char == ' ' = True
  | otherwise = False

--replaceSpaceByEmpty :: Char -> Char
--replaceSpaceByEmpty char = if (isSpace char) then '' else char

-- Void es un conjunto vacio.
words str = [ isSpace(str !! i) | i <- [0..length str]]

--class Monoidd m where
--    mempty :: m
--    mappend :: m -> m -> m
--
--instance Monoidd String where
--  mempty = ""
--  mappend = (++)

truncate num n = num * (10 ** n)









