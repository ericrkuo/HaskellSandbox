-- Enumeration Types
data DaysOfTheWeek = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

day :: DaysOfTheWeek
day = Monday

weekdays :: [DaysOfTheWeek]
weekdays = [Monday, Tuesday, Wednesday, Thursday, Friday]

isWeekend :: DaysOfTheWeek -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

-- algebraic data types (beyond enumartions)
data FailableDouble = Failure
                    | OK Double
                    | NOTOK Bool
  deriving Show

ex01 = Failure
ex02 = OK 3.4
ex03 = NOTOK False

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv _ 1 = NOTOK False
safeDiv x y = OK (x / y)

-- pattern matching with algebraic data types
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- more complex algebraic data types
-- e.g. data constructor with > 1 argument
--      type constructor same name as data constructor, but they are NOT the same thing
--      data constructor argument has type of an algebraic data type
--      data constructor argument with type of own type constructor
--          - this is actually Recursive data types (covered later)
--          --> try "foo (Cow (Cow (Person "as" "asd" 12)))"

data Person =
    -- first name, last name, age
    Person String String Int
    | DaysAvailable [DaysOfTheWeek]
    | TestScore FailableDouble Bool
    | Cow Person
  deriving Show

foo :: Person -> String
foo (Person f l x)
  | x > 20 = ("Hi, I'm " ++ f ++ " " ++ l ++ " and I am old, I am " ++ (show x) ++ " years old")
  | otherwise = ("Hi, I'm " ++ f ++ " " ++ l)
foo (DaysAvailable []) = "I'm not available"
foo (DaysAvailable [x]) = "I'm only available on " ++ (show x)
foo (DaysAvailable xs) = "I'm available on: " ++ (foldr ((++). ((++) " ") . show) "" xs)
foo (TestScore Failure _) = "I'm a failure"
foo (TestScore (OK 50) _) = "You got 50!"
foo (TestScore (OK d) b) = ((show d) ++ " " ++ (show b))
foo (Cow p) = foo p

data SparseList = Empty
                | OneAndRest Double SparseList
                | SkipAndRest Int SparseList
  deriving (Eq, Show)       

-- L2.5
-- All it wants is to replace explicit 0's in OneAndRest with SkipAndRest 1
slZeroToSkip :: SparseList -> SparseList
slZeroToSkip Empty = Empty
slZeroToSkip (OneAndRest 0.0 rest) = (SkipAndRest 1 (slZeroToSkip rest))
slZeroToSkip (OneAndRest d rest) = (OneAndRest d (slZeroToSkip rest))
slZeroToSkip (SkipAndRest n rest) = (SkipAndRest n (slZeroToSkip rest))

-- L2.6
slCompact :: SparseList -> SparseList
slCompact Empty = Empty
slCompact (OneAndRest d rest) = (OneAndRest d (slCompact rest))
slCompact (SkipAndRest x (SkipAndRest y rest)) = slCompact (SkipAndRest ((max x 1)+(max y 1)) rest)
slCompact (SkipAndRest x rest) = (SkipAndRest (max x 1) (slCompact rest))
