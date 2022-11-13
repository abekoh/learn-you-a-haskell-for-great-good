import Data.Data (typeOf)
import Data.Map qualified as Map

-- data Shape
--   = Circle Float Float Float
--   | Rectangle Float Float Float Float
--   deriving (Show)

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- data Person = Person String String Int Float String String
--   deriving (Show)

-- data Person = Person
--   { firstName :: String,
--     lastName :: String,
--     age :: Int,
--     height :: Float,
--     phoneNumber :: String,
--     flavor :: String
--   }
--   deriving (Show)

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `dotProd` (Vector l m n) = Vector (i * l) (j * m) (k * n)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` n = Vector (i * n) (j * n) (k * n)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

type IntMap = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) ->
    if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "ZD39I")), (101, (Free, "JAH3I"))]

infixr 5 :-:

data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++

(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

main :: IO ()
main = do
  print $ "hello"
  print $ typeOf (id @Shape)
  print $ area (Circle (Point 10 20) 10)
  print $ area (Rectangle (Point 0 0) (Point 100 100))
  print $ (Circle (Point 10 20) 10)
  print $ map (Circle (Point 10 20)) [4, 5, 6, 6]
  let c = Car {company = "Ford", model = "Mustang", year = 1967}
  print c
  print $ Vector 3 5 8 `vplus` Vector 9 2 8
  print $ Vector 3 5 8 `vmult` 10
  print $ Vector 3 5 8 `dotProd` Vector 9.0 2.0 4.0
  let mysteryDude =
        "Person { firatName =\"Michael\""
          ++ ", lastName =\"Diamond\""
          ++ ", age = 43}"
  -- let m = read mysteryDude :: Person
  -- print m
  print $ succ Monday
  print $ lockerLookup 100 lockers
  print $ lockerLookup 101 lockers
  print $ lockerLookup 102 lockers
  print $ 3 :-: 4 :-: 5 :-: Empty
  let a = 3 :-: 4 :-: 5 :-: Empty
  let b = 6 :-: 7 :-: Empty
  print $ a ^++ b
