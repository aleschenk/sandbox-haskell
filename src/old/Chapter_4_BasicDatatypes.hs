module Chapter_4_BasicDatatypes where

type Name = String

data Pet = Cat | Dog String

-- TUPLES
first :: (a,b) -> String
first tuple = ""

changeToCat :: (Name -> Pet) -> Pet
changeToCat name = Cat

aDog :: (->) String Pet
aDog a = Dog a

data A b c

--a :: Pet
--a = changeToCat (Dog "Peter")

--ii :: Show a => a -> String

-- Exercises

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = let
           a = fst t1
           b = snd t1
           c = fst t2
           d = snd t2
          in
            ((b, d), (a, c))

x = (+)

df xs = w `x` 1
        where w = length xs

-- -----------------------------------------------
-- 4.3 Anatomy of a data declaration
-- -----------------------------------------------

-- Data declarations are how datatypes are defined.
-- Type Signature (the type level of your code)
-- The whole thing is called a data declaration
-- data Bool = True | False
--     [1]  [2] [3] [4]
-- 1. Type constructor for datatype Bool. This is the name of the type and shows up in type signatures.
-- 2. Data constructor for the value False.
-- 3. Pipe | indicates a sum type or logical disjunction: “or.” So, a Bool value is True or False.
-- 4. Data constructor for the value True
-- ------------------------------------
-- Exercises: Mood Swing
-- ------------------------------------
-- Blah es un Data Constructor
-- Woot es un Data Constructor
-- Show NO es un Data Constructor
-- Given the following datatype, answer the following questions
data Mood
  = Blah
  | Woot
  deriving (Show)

-- 1. What is the type constructor, or name of this type?
-- rta: Mood is the Type Constructor
-- 2. If the function requires a Mood value, what are the values you could possibly use there?
-- rta: The possible values are Blah and Woot
-- verification:
aFunction :: Mood -> String
aFunction mood = ""

fBlah = aFunction Blah

fWoot = aFunction Woot

-- fShow = aFunction Show <-- This line does not compile.
-- 3. We are trying to write a function changeMood to change Chris’s
-- mood instantaneously. It should act like not in that, given one
-- value, it returns the other value of the same type. So far, we’ve
-- written a type signature changeMood :: Mood -> Woot. What’s wrong
-- with that?
-- Woot is not a Type Constructor it is a Data Constructor
--changeMood :: Mood -> Woot
changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

rvrs :: String -> String
rvrs text = awesome ++ " " ++ is ++ " " ++ curry
  where
    text = "Curry is awesome"
    curry = take 5 text
    is = take 2 $ drop 6 text
    awesome = drop 9 text

main :: IO ()
--main = print $ rvrs "Curry is awesome"
main = print (rvrs "Curry is awesome")
