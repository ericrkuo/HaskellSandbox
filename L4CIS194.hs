-- anonymous functions (lambda abstractions)
greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs

anonymousFuncWithMoreThanOneArgument = (\x y z -> [x,2*y,3*z]) 5 6 3

-- function composition
f :: b -> c
f = undefined

g :: a -> b
g = undefined

-- :t (f.g) gives (a -> c) which is equivalent to (\x -> f ( g x))

-- currying and partial application

{-
Can think of as
- takes in one argument of type a and outputs function (b -> c -> d)
- (b -> c ->d) takes in one argument of type b and outputs function (c -> d)
- (c -> d) takes in on argument of type b and gives back value of type d
EQUIVALENTLY this can be expressed as
  foo :: a -> (b -> (c -> d))

THEREFORE function arrows are RIGHT ASSOCIATIVE

function application is LEFT ASSOCIATIVE
e.g. foo True 1 'a' is equivalent to (((foo True) 1) 'a') where (foo True) returns a function that takes in one argument of type b and gives back a function (c -> d)
-}
foo :: a -> b -> c -> d
foo = undefined

-- wholemeal programming
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

-- pipeline of three functions
-- map and filter are partially applied
--      e.g. filter (> 3) returns [a] -> [a] (takes on input [a] and produces [a])
-- point free style, defining function without reference to arguments
--    saying what a function is rather than what it does
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (> 3)
