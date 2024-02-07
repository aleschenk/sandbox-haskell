module TopOrLocal where

area d = pi * (r * r)
  where r = d / 2

--r = d / 2

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5

