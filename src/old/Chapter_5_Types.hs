module Chapter_5_Types where

-- So, why do we want types? Type systems in logic and mathematics
-- have been designed to impose constraints that enforce correctness
-- ******************************************************************************
--This declaration creates a datatype with the type constructor Bool,
--and we refer to specific types by their type constructors. We use type
--constructors in type signatures, not in the expressions that make up
--our term-level code. The type constructor Bool takes no arguments
--(some type constructors do take arguments). The definition of Bool
--above also creates two data constructors, True and False. Both of
--these values are of type Bool. Any function that accepts values of
--type Bool must allow for the possibility of True or False; you cannot
--specify in the type that it should only accept one specific value. An
--English language formulation of this datatype would be something
--like: “The datatype Bool is represented by the values True or False.”
--data Bool = False | True
dd :: String -> String
dd a = a

-- Typeclass-Constrained Polymorphic Type Variable
abc :: (Num a, Num b) => a -> b -> b
abc a b = b

-- How to read type signature
-- Excercies
-- a) not    -> _ :: Bool -> Bool
-- b) length -> _ :: [a] -> Int
-- c) concat -> _ :: [[a]] -> [a]
-- d) head   -> _ :: [a] -> a
-- e) (<)    -> _ :: Ord a => a -> -a -> Bool
-- Currying
-- data (->) a b
-- f :: a -> b
-- f :: a -> (b -> c)
-- f :: a ->
--anExpresion = if true then false
--aNewList = [ x * 2 | x <- [0 .. 99], x * x > 3]
a =
  let x = 1
   in x + 1

lambda = \x -> 1 + x

aNewList =
  [ 1 + 1
  , 2 + 3
  , if True
      then 1
      else 2
  , let x = 1
     in x + 1
  , lambda (2)
  ]

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = undefined
