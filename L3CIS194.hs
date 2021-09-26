-- Recursion patterns

data IntList = Empty | Cons Int IntList
  deriving Show

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

exAdd1 = mapIntList (+ 1) exampleList
exAbs = mapIntList abs exampleList
exSquare = mapIntList (^ 2) exampleList

-- Three ways to pass functions as arguments

-- 1. Declare explicitly
double x = x*2
exDouble1 = mapIntList double exampleList

-- 2. Anonymouse functions
exDouble2 = mapIntList (\n -> 2*n) exampleList

-- 3. Operator sections
exDouble3 = mapIntList (* 2) exampleList

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons x xs)
  | f x = Cons x (filterIntList f xs)
  | otherwise = filterIntList f xs

exFilterPositive = filterIntList (> 0) exampleList
exFilterEven = filterIntList even exampleList

-- takes second argument and last item and applies function
-- and repeats with next penultimate item and result from above
foldrIntList :: (Int -> a -> a) -> a -> IntList -> a
foldrIntList _ base Empty = base
foldrIntList f base (Cons x Empty) = (f x base)
foldrIntList f base (Cons x xs) = f x (foldrIntList f base xs)

exFoldr1 = foldrIntList max 0 exampleList
exFoldr2 = foldrIntList (max . abs) (-100) exampleList
exFoldr3 = foldrIntList (\x y -> (show x) ++ " " ++ y) "hello" exampleList
exFoldr4 = foldrIntList (+) 0 exampleList