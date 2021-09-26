-- Recursion patterns

data IntList = Empty | Cons Int IntList
  deriving (Eq, Show)

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

-- Polymorphism

-- Polymorphic data types
data List t = E | C t (List t)
  deriving Show

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

lst4 :: List String
lst4 = C "hello" (C "hi" (C "foo" (C "boo" E)))

lst5 :: List IntList
lst5 = C exampleList (C Empty (C (Cons 123 Empty) E))

-- Polymorphic functions
filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList f (C x xs)
  | f x = C x (filterList f xs)
  | otherwise = filterList f xs

exFilterList1 = filterList odd lst1
exFilterList2 = filterList (> 'y') lst2
exFilterList3 = filterList (&& True) lst3
exFilterList4 = filterList (\n -> (filterIntList even n) /= Empty) lst5

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

-- can use mapList to go from list of a's to list of b's
-- eg. `mapList toList lst5` converts from List ConsList to List [Int]
exMapList1 = mapList show lst1
exMapList2 = mapList toList lst5

toList Empty = []
toList (Cons x xs) = x : (toList xs)