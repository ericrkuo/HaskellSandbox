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

-- data types with more than one type variables
data Foo a b c = A a
              | B b
              | D (c -> b) a

fooEx1 :: Foo Int String Bool
fooEx1 = D (\n -> show n) 123

fooEx2 :: Foo Bool Char Integer
fooEx2 = B 'A'

fooEx3 :: Foo String b c
fooEx3 = A "hello"

-- Polymorphic functions
filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList f (C t ts)
  | f t = C t (filterList f ts)
  | otherwise = filterList f ts

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

-- Sections or Operator Sections
-- https://www.haskell.org/onlinereport/exps.html#sections

ex01 = map (* 2) [1,2,3] -- (* 2) = (op e) = (\x -> x op e) where op = (*) and e = 2
ex02 = map (2 *) [1,2,3] -- same as above b/c commutative
ex03 = map (2 ^) [1,2,3] -- (\x -> 2 ^ x)
ex04 = map (^ 2) [1,2,3] -- (\x -> x ^ 2)
ex05 = map (2 `subtract`) [1,2,3]
ex06 = map (`subtract` 2) [1,2,3]
-- ex07 = map (- 2) [1,2,3] Invalid, `-` is treated specially in grammar, it is the unary operator https://wiki.haskell.org/Unary_operator

-- Total and partial functions
-- Two ways to write partial functions

-- 1. rewrite with Maybe or change output to indicate possible failure
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- 2. If we are guaranteed some condition, types should reflect that to guarantee
-- Therefore, compiler can enforce those guarantees
data NonEmptyList a = NEL a [a]
  deriving Show

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just (NEL x xs)

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as