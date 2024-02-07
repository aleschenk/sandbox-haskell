module Chapter_6_Typeclasses where

data Trivial = Trivial'

-- instance  Eq  Trivial where
--    [1]   [2]   [3]     [4]
-- Trivial' == Trivial' = True
--    [5]   [6]  [7]      [8]

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun

--instance Show DayOfWeek where
--  show dayOfWeek
--    | dayOfWeek == Mon = "Monday"
--    | dayOfWeek == Tue = "Thuesday"
--    | dayOfWeek == Weds = "Wednesday"
--    | dayOfWeek == Thu = "Thursday"
--    | dayOfWeek == Fri = "Friday"
--    | dayOfWeek == Sat = "Saturday"
--    | dayOfWeek == Sun = "Sunday"
--    | otherwise = show "Error"

instance Show DayOfWeek where
  show Mon = "Monday"
  show Mon = "Monday"
  show Tue = "Thuesday"
  show Weds = "Wednesday"
  show Thu = "Thursday"
  show Fri = "Friday"
  show Sat = "Saturday"
  show Sun = "Sunday"
  show _ = "Error"

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date = Date DayOfWeek Int

instance Eq Date where
  (==)
    (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'

instance Show Date where
  show (Date weekday dayOfMonth) = " " ++ (show weekday) ++ " " ++ (show dayOfMonth)

data UserStatus = Connected | NotConnected

instance Eq UserStatus where
  (==) Connected Connected = True
  (==) NotConnected NotConnected = True
  (==) _ _ = False

-- Another way to write the same thing. In this case using the infix notation.
--instance Eq UserStatus where
--  Connected == Connected = True
--  NotConnected == NotConnected = True
--  _ == _ = False

data Identity a = Identity a

--instance Eq (Identity a) where
--  (==) (Identity v) (Identity v') = v == v'

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- --------------------------------
-- EXERCISES: Eq Instances
-- --------------------------------

-- Write the Eq instance for the datatype provided.

-- 1.
data TisAnInteger = TisAn Integer

instance Eq (TisAnInteger) where
  (==) (TisAn v) (TisAn v') = v == v'

-- 2.
data TwoInteger = Two Integer Integer

instance Eq (TwoInteger) where
  (==) (Two v1 v2) (Two v1' v2') = v1 == v1' && v2 == v2'

-- 3.
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAString v) (TisAString v') = v == v'

-- 4.
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v0 v1) (Pair v0' v1') = v0 == v0' && v1 == v1'

-- 5.
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v0 v1) (Tuple v0' v1') = v0 == v0' && v1 == v1'

-- 6.
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne v) (ThisOne v') = v == v'
  (==) (ThatOne v) (ThatOne v') = v == v'

-- 7.
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v') = v == v'
  (==) (Goodbye v) (Goodbye v') = v == v'

-- --------------------------------
-- ORDERING
-- --------------------------------

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

--
-- Exercises: Will They Work?
--

--1. max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
-- Yes, it will work.
-- rta: 5

--2. compare (3 * 4) (3 * 5)
-- Yes, it will work.
-- rta: LT

--3. compare "Julie" True
-- No, it wont work, because the comparision is between different types.

--4. (5 + 3) > (3 + 6)
--  Yes, it will work
-- Rta: False

instance Enum DayOfWeek where
  succ Mon = Tue
  succ Tue = Weds
  succ Weds = Thu
  succ Thu = Fri
  succ Fri = Sat
  succ Sat = Sun
  succ Sun = Mon

class (Show a) => Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: (Numberish a, Numberish b, Numberish c) => a -> b -> c
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfB = toNumber a'
    summed = integerOfA + integerOfB

--sumNumberish :: (Numberish a) => a -> a -> a
--sumNumberish a a' = fromNumber summed
--  where integerOfA = toNumber a
--        integerOfB = toNumber a'
--        summed = integerOfA + integerOfB

--add :: (Num a) => a -> a -> a
--add x y = x + y
--

-- ------------------------------------------------------------
--  6.14 E X E R C I E S
-- ------------------------------------------------------------

--1. The Eq class
--a) includes all types in Haskell
--b) is the same as the Ord class
--c) makes equality tests possible <--
--d) only includes numeric types

--2. The typeclass Ord
--a) allows any two values to be compared <--
--b) is a subclass of Eq
--c) is a superclass of Eq
--d) has no instance for Bool

--3. Suppose the typeclass Ord has an operator >. What is the type of >?
--a) Ord a => a -> a -> Bool <--
--b) Ord a => Int -> Bool
--c) Ord a => a -> Char
--d) Ord a => Char -> [Char]

--4. In x = divMod 16 12
--a) the type of x is Integer
--b) the value of x is undecidable
--c) the type of x is a tuple
--d) x is equal to 12 / 16

--5. The typeclass Integral includes
--a) Int and Integer numbers
--b) integral, real, and fractional numbers
--c) Schrodinger’s cat
--d) only positive numbers

--1. Does the following code typecheck? If not, why not?
--data Person = Person Bool
--printPerson :: Person -> IO ()
--printPerson person = putStrLn (show person) <-- It won't compile. missing "deriving (Show)"

--2. Does the following typecheck? If not, why not?
--data Mood = Blah | Woot deriving Show
--settleDown x = if x == Woot then Blah else x <-- It won't compile. missing could be anything.

data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

--3. If you were able to get settleDown to typecheck:
--a) What values are acceptable inputs to that function? Blah and Woot
--b) What will happen if you try to run settleDown 9? Why? It fails because: No instance for (Num Mood) arising from the literal ‘9’
--c) What will happen if you try to run Blah > Woot? Why? It fails because x is not constrained to Ord.


type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"




data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'


--i :: Num a => a
--i :: a
--i = 1

f :: Float
--f :: Num a => a  <-- Could not deduce (Fractional a) arising from the literal ‘1.0’       from the context: Num a
f = 1.0

--f1 :: Float
f1 :: Fractional a => a
f1 = 1.0

--f2 :: Float
f2 :: RealFrac a => a
f2 = 1.0


--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

--freud1' :: a -> a
freud1' :: Int -> Int
freud1' x = x

myX = 1 :: Int
sigmund :: Int -> Int
--sigmund :: a -> a
sigmund x = myX

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fx a b = fx a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fx _ = fx


newtype Nada = Nada Double deriving (Eq, Show)

-- 2 
-- 'a'
-- "ABC"
-- []
-- (,)
-- \x = x -> x
-- {}