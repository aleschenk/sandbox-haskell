{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter_11_Algebraic_datatypes where

data Role a b = Admin a | User b

myFunc :: Role String b -> String
myFunc role = ""

caller = myFunc (Admin "Joe")

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherJusky :: Num a => HuskyType a
myOtherJusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDodge :: DogueDeBordeaux Int
myDodge = DogueDeBordeaux 10

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

-- -----------------------------------------------
--  Exercises: Dog Types
-- -----------------------------------------------
--1. Is Doggies a type constructor or a data constructor?
--  rta: a Type Constructor

--2. What is the kind of Doggies?
--  rta: * -> *

--3. What is the kind of Doggies String?
--  rta: *

--4. What is the type of Husky 10?
--  rta: Num a => Doggies a

--5. What is the type of Husky (10 :: Integer)?
--  rta: Doggies Integer

--6. What is the type of Mastiff "Scooby Doo"?
--  rta: Mastiff [Char]]

--7. Is DogueDeBordeaux a type constructor or a data constructor?
--  rta: Both because each one shares the same name.

--8. What is the type of DogueDeBordeaux?
--  rta:  doge -> DogueDeBordeaux doge

--9. What is the type of DogueDeBordeaux "doggie!"
--  rta:  DogueDeBordeaux [Char]

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

-- -----------------------------------------------
-- Exercises: Vehicles
-- -----------------------------------------------
myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir

--1. What is the type of myCar?
--  rta: Vehicle

--2. Given the following, define the functions:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _) = False

isPlane :: Vehicle -> Bool
isPlane a = not $ isCar a

areCars :: [Vehicle] -> [Bool]
areCars vehicles = [isCar vehicle | vehicle <- vehicles]

-- 3. Now we’re going to write a function to tell us the manufacturer of a piece of data:
getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer
getManu _ = undefined

-- 4. Given that we’re returning the Manufacturer, what will happen if you use this on Plane data?
-- rta: It will fail.

-- 5. All right. Let’s say you’ve decided to add the size of the plane as
-- an argument to the Plane constructor. Add that to your datatypes
-- in the appropriate places and change your data and functions
-- appropriately.

-- Exercises: Cardinality
--1. data PugType = PugData
--  rta: nullary

--2. For this one, recall that Bool is also defined with the |:
--data Airline
--  = PapuAir
--  | CatapultsR'Us
--  | TakeYourChancesUnited
--  rta: 1

--3. Given what we know about Int8, what’s the cardinality of Int16?
--4. Use the REPL and maxBound and minBound to examine Int and
--Integer. What can you say about the cardinality of those types?
--5. Extra credit (impress your friends!): What’s the connection
--between the 8 in Int8 and that type’s cardinality of 256?

data Example = MakeExample Int deriving (Show)

--tooManyGoats :: Int -> Bool
--tooManyGoats n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

newtype Cows = Cows Int deriving (Eq, Show)

--data Goats = Goats Int deriving (Eq, Show)
--data Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

a = tooManyGoats (Goats 23)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (age, name) = age > 42 && length (name) > 30

instance TooMany (Int, Int) where
  tooMany (n1, n2) = (n1 + n2) > 40

--instance TooMany (Num a, TooMany a) where
--  tooMany (n1, n2) = (n1 + n2) > 40

-- -----------------------------------------------
--  Exercises: Pity the Bool
-- -----------------------------------------------
--1. Given a datatype
data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)

--What is the cardinality of this datatype? Hint: We already know
--Bool’s cardinality. Show your work as demonstrated earlier.

--fst :: a -> b
--fst :: a -> b -> c
--fst :: (->) Integer ((->) a b)
fst :: (a -> b) -> (c -> d)
fst = undefined

data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

--                       b          +              c   
data BookType = FictionBook Fiction | NonFictionBook NonFiction | ASS String Integer deriving Show

type AuthorName = String

-- Author is a Product Type.
--data Author = Author (AuthorName, BookType)

-- In Normal Form
--data Author = Fiction AuthorName | NonFiction AuthorName deriving (Eq, Show)


c :: BookType -> String
c (FictionBook Fiction) = ""
c (NonFictionBook NonFiction) = ""
--c (ASS String Integer) = ""

--d = c A ("", 2)

-- -----------------------------------------------
--  Exercises: How Does Your Garden Grow?
-- -----------------------------------------------
--1. Given the type
data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden = Garden Gardener FlowerType deriving (Show)

--What is the normal form of Garden?
--data Garden' = Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener deriving (Show)

--data Garden' = Gardener Gardenia | Gardener Daisy | Gardener Rose | Gardener Lilac deriving Show


-- (a + b) * (a + c)
-- a * ( b + c)

-- a * (b + c) -> (a * b) + (a * c)

--data DataSum = A | B
--
----                         (a   *  b )  +           ( a * c )
--data DataProduct = Prod1 DataSum String | Prod2 DataSum Integer
--
---- a * (b +c)
--data DataProduct' = DataSum String | DataSum Integer

-- -----------------------------------------------
-- 11.13 Constructing and deconstructing values
-- -----------------------------------------------

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig
type Farmshouse'' = Farmhouse

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)


data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)

-- Alternately
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)



