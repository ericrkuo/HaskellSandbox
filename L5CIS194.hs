
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