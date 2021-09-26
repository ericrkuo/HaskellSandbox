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

-- L2
-- Q8
slEmpty, slTwoElt, slManyElt :: SparseList
slEmpty = Empty
slTwoElt = SkipAndRest 2 Empty
slManyElt = OneAndRest 2.5 (OneAndRest 1 (SkipAndRest 1 (OneAndRest 3.2 (SkipAndRest 3 (OneAndRest 1 Empty)))))
slManyElt2 = OneAndRest (-2) (OneAndRest 1 (SkipAndRest 0 (OneAndRest 0 (SkipAndRest 3 (OneAndRest (-1) Empty)))))

filterLarger' :: Double -> SparseList -> SparseList
filterLarger' _ Empty = Empty
filterLarger' d (OneAndRest d' sl) | d' > d = OneAndRest d' (filterLarger' d sl)
                                  | otherwise = filterLarger' d sl
filterLarger' d (SkipAndRest n sl) | 0 > d = SkipAndRest n (filterLarger' d sl)
                                  | otherwise = filterLarger' d sl
                                
filterWithinUnit' :: SparseList -> SparseList
filterWithinUnit' Empty = Empty
filterWithinUnit' (OneAndRest d sl) | abs d < 1 = OneAndRest d (filterWithinUnit' sl)
                                   | otherwise = filterWithinUnit' sl
filterWithinUnit' (SkipAndRest n sl) = SkipAndRest n (filterWithinUnit' sl)

slFilter :: (Double -> Bool) -> SparseList -> SparseList
slFilter _ Empty = Empty
slFilter f (OneAndRest d sl)
  | f d = OneAndRest d (slFilter f sl)
  | otherwise = slFilter f sl
slFilter f (SkipAndRest n sl)
  | f 0 = SkipAndRest n (slFilter f sl)
  | otherwise = slFilter f sl

filterLarger :: Double -> SparseList -> SparseList
filterLarger d = slFilter (> d)

filterWithinUnit :: SparseList -> SparseList
filterWithinUnit = slFilter (\n -> abs n < 1)

-- 9
slMap :: (Double -> Double) -> SparseList -> SparseList
slMap _ Empty = Empty
slMap f (OneAndRest d sl) = OneAndRest (f d) (slMap f sl)
slMap f (SkipAndRest n sl) = SkipAndRest (round (f (fromIntegral n))) (slMap f sl)

slSigns :: SparseList -> SparseList
slSigns = slMap signum

-- 12
data ProVal a = CV a String
              | PV a
  deriving (Show, Eq)

getVal :: ProVal a -> a
getVal (CV value _) = value
getVal (PV value) = value

-- 13
displayPro :: ProVal a -> String
-- displayPro (CV _ comment) = ['['] ++ comment ++ [']'] -- equivalent
displayPro (CV _ comment) = "[" ++ comment ++ "]"
displayPro _ = "()"

-- 14
selectHead :: [Int] -> ProVal Int
selectHead [] = (CV 0 "empty")
selectHead (x:_) = (CV x "selected")

-- bind (CV [] "Old") selectHead == CV 0 "[Old] [empty] bound"
-- bind (CV [123,456] "hi") selectHead == CV 123 "[hi] [selected] bound"

-- bind V1
-- WRONG b/c can't use getVal
bind'' :: ProVal a -> (a -> ProVal b) -> ProVal b
bind'' pa@(PV val) f = CV (getVal pb) ((displayPro pa) ++ " " ++ (displayPro pb) ++ " bound")
  where pb = f val
bind'' pa@(CV val cmt) f = CV (getVal pb) ((displayPro pa) ++ " " ++ (displayPro pb) ++ " bound")
  where pb = f val

-- bind V2
-- using pattern matching to where clauses
-- using x@pat to give name x to entire pattern IN the where clause!!
-- WRONG b/c pb@(CV x _) will not match to pb@(PV x)
bind' :: ProVal a -> (a -> ProVal b) -> ProVal b
bind' pa@(PV val) f = CV x (proA ++ " " ++ proB ++ " bound")
  where
    pb@(CV x _) = f val
    proA = displayPro pa
    proB = displayPro pb
bind' pa@(CV val cmt) f = CV x (proA ++ " " ++ proB ++ " bound")
  where
    pb@(CV x _) = f val
    proA = displayPro pa
    proB = displayPro pb

-- using case and where
-- TODO is there a more succinct way to do this?
--
-- >>> bind (CV [123,456] "hi") selectHead
-- CV 123 "[hi] [selected] bound"
--
-- >>> bind (CV [] "Old") selectHead
-- CV 0 "[Old] [empty] bound"
--
-- >>> bind (PV True) (\n -> PV 1)
-- CV 1 "() () bound"
--
-- >>> bind (CV [] "hi") (\n -> PV 1)
-- CV 1 "[hi] () bound"
--
-- >>> bind (PV True) (\n -> CV 123 "x")
-- CV 123 "() [x] bound"
--
-- >>> bind (PV True) (\n -> CV n (show n))
-- CV True "() [True] bound"
bind :: ProVal a -> (a -> ProVal b) -> ProVal b
bind pa@(PV val) f = case pb of 
  (CV x _ ) -> CV x cmt
  (PV y ) -> CV y cmt
  where
    pb = f val
    cmt = ((displayPro pa) ++ " " ++ (displayPro pb) ++ " bound")
bind pa@(CV val _) f = case pb of
  (CV x _ ) -> CV x cmt
  (PV y ) -> CV y cmt
  where
    pb = f val
    cmt = ((displayPro pa) ++ " " ++ (displayPro pb) ++ " bound")

-- data constructor types that are function types in algebraic data types
data Moo = Moo (Int -> String) Bool
         | X ([Int] -> ProVal Int) [Int]

mooTest :: Moo -> String
mooTest (Moo f b) = (f 2) ++ (show b)
mooTest (X f xs) = case pb of
  (CV _ cmt) -> cmt
  (PV y) -> show y
  where pb = f xs

mooEx1 = mooTest (Moo (\n -> show n) False)
mooEx2 = mooTest (X selectHead [1,2,3])
mooEx3 = mooTest (X selectHead [])

-- function in where clause
ff :: String -> String -> String
ff a b = printFancy a
  where
    printFancy x  -- IMPORTANT: needs to be on a new line
      | x == "hello" = x ++ b
      | otherwise = b ++ x ++ b