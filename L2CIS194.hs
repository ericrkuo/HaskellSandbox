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

-- pattern matching
pmExample :: Person -> String
-- x@pat
pmExample p@(Person a b c) = "x@pat pattern matching gave: " ++ (show p)
-- nested pattern matching
pmExample (Cow p@(Person a b c)) = "Person " ++ (show p) ++ " likes cows"
-- wild card matching
pmExample _ = "Everything else"


-- case expressions

-- ex04 = case "Hello" of
   --        [] -> 3
      --       ('H':s) -> length s
         --  _ -> 7

-- failureToZero but with case expressions
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                      Failure -> 0
                      (OK d) -> d

data IntList = EmptyEmpty | Cons Int IntList

intListProd  :: IntList -> Int
intListProd  EmptyEmpty = 1
intListProd  (Cons x xs) = x * (intListProd xs)

data Tree = EmptyLeaf
          | Leaf Char
          | Node Tree Int Tree
  deriving Show

{-
          1
      2       3
    A   B        D
-}
tree :: Tree
tree = Node (Node (Leaf 'A') 2 (Leaf 'B')) 1 (Node EmptyLeaf 3 (Leaf 'D'))

inOrder :: Tree -> [String]
inOrder EmptyLeaf = []
inOrder (Leaf c) = [[c]]
inOrder (Node left val right) = (inOrder left) ++ [(show val)] ++ (inOrder right)

preOrder :: Tree -> [String]
preOrder EmptyLeaf = []
preOrder (Leaf c) = [[c]]
preOrder (Node left val right) = [(show val)] ++ (preOrder left) ++ (preOrder right)

postOrder :: Tree -> [String]
postOrder EmptyLeaf = []
postOrder (Leaf c) = [[c]]
postOrder (Node left val right) = (postOrder left) ++ (postOrder right) ++ [(show val)]
