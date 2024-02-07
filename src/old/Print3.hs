module Print3 where

import Web.Scotty

myGreeting :: String
myGreeting = "hello" ++ " world"

hello :: String
hello = "hello"

world :: [Char]
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting = concat [hello, " ", world]

--https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
data Zero
data Succ a
type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ (Succ (Succ (Succ Zero)))

-- https://wiki.haskell.org/Peano_numbers
data Nat = Zero | Succ Nat



