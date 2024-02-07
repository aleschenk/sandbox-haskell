module Chapter_9_Lists where

--data [] c  = [] | c : [c]

--(:) :: a -> [a] -> [a]

-- This is a syntact sugar
--[1, 2, 3] ++ 4

--for this
--(1 : 2 : 3 : []) ++ 4 : []

-- and this is the same of above without infix operator
--(:) 1 ((:) 2 ((:) 3 []))

-- UNICODE
--k :: String
--k = ""
--
----asc (♥) a b c = a ♥ (b ♥ c) == (a ♥ b) ♥ ce
--
--d = let x € y = x * y in 2 € 5

eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool False False = [False]
eftBool True False = []
eftBool False True = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = undefined

_eftInt :: Int -> Int -> [Int] -> [Int]
_eftInt num upperBound lst
  | length(lst) == upperBound = lst
  | otherwise = _eftInt (num + 1) upperBound lst

eftInt :: Int -> Int -> [Int]
eftInt a b = _eftInt a b []

eftChar :: Char -> Char -> [Char]
eftChar = undefined

d = let a = 2; d = 4 in a + d

-- -----------------------------------------------
-- 9.7 List comprehensions
-- -----------------------------------------------

lc_1 = [ x^2 | x <- [1..10] ]

lc_2 = [ x^2 | x <- [1..0], rem x 2 == 0]

lc_3 = [ x^y | x <- [1..0], y <- [2,3]]

lc_4 = [ x^2 | x <- [1..0], x == if x > 2 then 3 else 4]

lc_5 = [x^y | x <- [1..10], y <- [2, 3], x^y < 200]

lc_6 = [(x, y) | x <- [1, 2, 3], y <- [6, 7]]

lc_7 = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]

-- -----------------------------------------------
-- 9.8 Spines and nonstrict evaluation
-- -----------------------------------------------

-- -----------------------------------------------
-- 9.9 Transforming lists of values
-- -----------------------------------------------
-- Prelude> map (+1) [1, 2, 3, 4]
-- [2,3,4,5]

-- Exercises: More Bottoms

--1. Will the following expression return a value or be ⊥?
--  take 1 $ map (+1) [undefined, 2, 3]
--  rta: error

--2. Will the following expression return a value?
--  take 1 $ map (+1) [1, undefined, 3]
--  rta: [2] 

--3. Will the following expression return a value?
--  take 2 $ map (+1) [1, undefined, 3]
--  rta: error

--4. What does the following mystery function do? What is its type?
--Describe it (to yourself or a loved one) in standard English and
--then test it out in the REPL to make sure you were correct.
--  itIsMystery xs = map (\x -> elem x "aeiou") xs
--  rta: returns a list which each elements could be True or False depending weather the char is a vowel or not.
--  example: xs = ["dog"], rta = [False, True, False]

--5. What will be the result of the following functions:
--  a) map (^2) [1..10]
--    rta: [1,4,9,16,25,36,49,64,81,100]

--b) map minimum [[1..10], [10..20], [20..30]]
---- n.b. `minimum` is not the same function
---- as the `min` that we used before
--c) map sum [[1..5], [1..5], [1..5]]


-- -----------------------------------------------
-- 9.10 Filtering lists of values
-- -----------------------------------------------