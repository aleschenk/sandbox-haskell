module Chapter_7_Functional_Patterns where

--1. Which (two or more) of the following are equivalent?
mTh x y z = x * y * z

mTh1 x y = \z -> x * y * z

mTh2 x = \y -> \z -> x * y * z

mTh3 = \x -> \y -> \z -> x * y * z

-- rta: all are equivalent

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where
    f = \n -> n + 1

--b) Rewrite the following to use anonymous lambda syntax:
--addFive x y = (if x > y then y else x) + 5
-- rta:
addFive = \x -> \y -> (if x > y then y else x) + 5

--c) Rewrite the following so that it doesn’t use anonymous
--lambda syntax:
--mflip f = \x -> \y -> f y x
-- rta:
mflip f x y = f y x

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

-- User is a sum with two constructors, UnregisteredUser and RegisteredUser.
-- RegisteredUser constructor is a product of two newtypes, Username and AccountNumber.
data User
  = UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "User is no registered"
printUser (RegisteredUser (Username "ale") (AccountNumber 123)) = putStrLn $ "My name is Ale" ++ " " ++ show 123
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) = putStrLn $ name ++ " " ++ show acctNum

printUserAle = printUser (RegisteredUser (Username "ale") (AccountNumber 123))

unpacking =
  let myUser = (Username "Homero")
      myAcc = (AccountNumber 123)
   in printUser $ RegisteredUser myUser myAcc

unpacking2 = printUser $ RegisteredUser myUser myAcc
  where
    myUser = Username "Homer"
    myAcc = AccountNumber 123

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

-- And a product type called Penguin. We haven’t given product
--types much attention yet, but for now you can think of Penguin as a
--type with only one value, Peng, and that value is a sort of box that
--contains a WherePenguinsLive value

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

--antarcticPenguin :: Penguin -> Bool
--antarcticPenguin (Peng Antarctica) = True
--antarcticPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin p = (galapagosPenguin p) || (antarcticPenguin p)

k (x, y) = x

k1 = k ((4 -1), 10)

k2 = k ("three", (1 + 2))

k3 = k (3, True)

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where
    y = xs == reverse xs

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eyyyyy. What's shakin ?"
    False -> putStrLn "psshhhh."
  where
    cool = coolness == "downright frosty yo"

--functionC x y = if (x > y) then  x else y
functionC x y = case x > y of
  True -> x
  _ -> y

--ifEvenAdd2 n = if even n then (n+2) else n
--ifEvenAdd2 n =  case even n of
--  True -> n +2
--  False -> n

ifEvenAdd2 n =
  let x = even n
   in case x of
        True -> n + 2
        False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- 7.6 Higher-order functions
flips :: (a -> b -> c) -> b -> a -> c
flips fab b a = fab a b

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \x y -> f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

--returnBroke :: (((a -> b) -> c) -> d) -> d
--returnBroke _ _ _ d = d

--returnBroke :: (((a -> b) -> c) -> d) -> d
--returnBroke _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = (flip dodgy) 2

--2. dodgy 1 1 -> rta:  11
--3. dodgy 2 2 -> rta:  22
--4. dodgy 1 2 -> rta:  12
--5. dodgy 2 1 -> rta:  12
--6. oneIsOne 1 -> rta:  11
--7. oneIsOne 2 -> rta:  21
--8. oneIsTwo 1 -> rta:  21
--9. oneIsTwo 2 -> rta:  22
--10. oneIsOne 3 -> rta:  31
--11. oneIsTwo 3 -> rta:  23

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (- x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a ^ 2 + b ^ 2 == c ^ 2 = "RIGHT ON"
  | otherwise = "not right"

dogYrs :: Integer -> Integer
dogYrs x
  | let y = 0 in (y + x) > 0 = x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where
    y = x / 100

--3. The following function returns

pal2 xs
  | xs == reverse xs = True
  | otherwise = False

--a) xs written backwards when it’s True
--b) True when xs is a palindrome <-- Right answer
--c) False when xs is a palindrome
--d) False when xs is reversed

--6. The following function returns
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

--a) the value of its argument plus or minus 1
--b) the negation of its argument
--c) an indication of whether its argument is a positive or nega- tive number or zero <-- Right Answer
--d) binary machine language

fcompose :: (b -> c) -> (a -> b) -> (a -> c)
fcompose f g a = f (g (a))

fcompose2 :: (a -> b) -> (b -> c) -> (a -> c)
fcompose2 f g a = g (f (a))

sum2 :: Num a => t a -> a
sum2 a = undefined

negate2 :: a -> a
negate2 = undefined

--a = negate2 . sum2
--
--d = (negate . sum) [1, 2, 3, 4, 5]

-- reverse :: [a] -> [a]
-- take :: Int -> [a] -> [a]

-- . :: (b -> c) (a -> b) -> (a -> c)
-- take 5 . reverse

--    take    .  reverse
-- (b  ->  c) -> (a -> b)
-- [a] -> [a] . [a] -> [a]

--(|>) :: Int -> String
--(|>) x = ""

--(|>) x f = f x

--f . g = \x -> f(g x)

--f . g . h = \x -> f (g (h x ))

--f = negate . sum

f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs








