{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
-- parametric polymorphism

-- does not work because CALLER gets to choose types, by saying && as an IMPLEMENTOR
-- this is not parametric polymorphism anymore
-- f :: a -> a -> a
-- f x y = x && y

-- ex02 is parametric in the type a
-- AKA works uniformly for any type chosen by caller
ex01 :: a -> a
ex01 a = a

-- TODO how would this work?
ex02 :: a -> b
ex02 a = undefined

ex03 :: a -> b -> a
ex03 a b = a

-- could do any operation with lists, so long as returns list of a's
ex04 :: [a] -> [a]
ex04 [] = []
ex04 [a] = [a]
ex04 (a:as) = as

-- inputs: function that inputs b and outputs c
-- function that inputs a and outputs b
-- a
-- output: c
-- e.g. ex05 show (< 5) 5
--      (Bool -> String) (Int -> Bool) Int -> String
ex05 :: (b -> c) -> (a -> b) -> (a -> c)
ex05 b2c a2b a = b2c (a2b a)

ex06 :: (a -> a) -> a -> a
ex06 f a = f a
-- OR ex06 f a = a

-- Type classes

-- MyEq is a type class with single parameter a
-- any type a that wants to be an INSTANCE of MyEq must define the two functions
-- NOTE: type of (==) is MyEq a => a -> a -> Bool where (MyEq a) is a type constraint meaning that you can use (==) with any type a so long as a is an instance of MyEq
class MyEq a where
  isEqual :: a -> a -> Bool

  isNotEqual :: a -> a -> Bool
  isNotEqual a b = not (isEqual a b) -- default implementation

data Foo = F Int
          | G Char

-- Foo is an instance of Eq
instance Eq Foo where
  (==) :: Foo -> Foo -> Bool
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  (/=) :: Foo -> Foo -> Bool
  foo1 /= foo2 = not (foo1 == foo2)

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

foo1 = F 2
foo2 = F 2 
foo3 = G 'a'

exEq1 = foo1 == foo2
exEq2 = foo2 == foo3

-- Can declare existing types as instance of your own type class

class Printable a where
  printf :: a -> String
  printBang :: a -> String

instance Printable Int where
  printf :: Int -> String
  printf a = show (a)

  printBang a = show (a) ++ "!!!"

exPrintable1 = printf (2::Int)
exPrintable2 = printf ((-1)::Int)
exPrintable3 = printBang (100::Int)
exPrintable4 = printBang ((-1)::Int)
-- printBang (200::Integer) does not work since Integer is not an instance of type Printable

printCombined :: Printable a => a -> a -> String
printCombined p1 p2 = printBang p1 ++ " " ++ printBang p2

-- multi-parameter type classes
-- multple dispatch - implementation of multi depends on BOTH the types of a and b
class Multi a b where
  multi :: a -> b -> String

instance (Multi String Int) where
  multi :: String -> Int -> String
  multi s x = s ++ show x

instance (Multi Int String) where
  multi :: Int -> String -> String
  multi x s = show x ++ s

exmulti1 = multi "asdasd" (2::Int)
exmulti2 = multi (100::Int) "hello"

-- multiFunc (2::Int) "asdas"
-- multiFunc "asdas" (2::Int)
multiFunc :: (Multi a b) => a -> b -> String
multiFunc a b = "Result: " ++ (multi a b)


-- type class with ternary methods
class Ternary a where
  doSomething :: a -> a -> a -> a

instance Ternary Int where
  doSomething a b c = a + b + c

-- above we declared Int to be an instance of Printable and Ternary
-- printableAndTernary (2::Int)
printableAndTernary :: Int -> String
printableAndTernary a = printBang (doSomething a a a)



-- a type class example
class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList a = [a]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

-- replicates list 3 times
instance Listable [Int] where
  toList xs = xs ++ xs ++ xs

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  -- in order toList
  toList :: (Tree Int) -> [Int]
  toList Empty = []
  toList (Node a left right) = (toList left) ++ [a] ++ (toList right)

-- E.g. toList (Node (1::Int)  Empty Empty)

sumL :: Listable a => a -> Int
sumL a = sum (toList a)

-- NOTE cannot be (Listable x, Ord x, Listable y, Ord y)
-- since even tho Ord x and Ord y lets us compare x's and y's, it does not let us compare x and y TOGETHER
foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y


-- example of type class with type constraints
instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y

-- regular triple instance would just be
instance Listable (Char, Char, String) where
  toList (a, b, xs) = []

-- Int and Bool are instances of Listable, and the pair is also Listable by the instance above
exListablePair1 :: ([Int], Bool)
exListablePair1 = ([1,2,3,4], True)

-- Char Char String are not instances of Listable, but the tuple of them is an instance of Listable
exListablePair2 = ('c', 'a', "asdasd")

-- can call toList on the above examples