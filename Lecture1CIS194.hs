-- Declarations and Variables

x :: Int
x=3  -- x is defined to be 3
-- x=4 compile error "Multiple declarations of x"

y :: Int
y=y+1 -- y is defined to be y+1, this will infinitely loop

-- Basic Types
-- Machine sized Integers (2^29) but can be architecture specific
i :: Int
i = -78

-- Arbitrary-precision integers (limited by amount of memory on machine)
n :: Integer
n = 1234567890987654321987340982334987349872349874534

a :: Double
a = 1.2323123123123123123123123123

b :: Float
b = 1.1111111111111111111111  -- Float loses precision

c,d :: Bool
c = False
d = True

e :: Char
e = 'a'

f :: String
f = "aslkdjaslkjd"

-- Arithmetic
ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1
ex05 = mod 19 3
ex06 = 19 `mod` 3
ex07 = 7 ^ 222
ex08 = (-3) * (-7)

-- badArith1 = i + n (this gives an error because Haskell does not do implicty conversion)
-- need to explicitly convert
badArith1Fixed = i + fromIntegral n  -- changes n to Int
badArith1Fixed1 = fromIntegral i + n -- changes i to Integer

-- badArith2 = i/i (error b/c / is for floating-point division)
badArith2Fixed = div i i
badArith2Fixed1 = i `div` i

-- Boolean Logic
ex11 = True && False
ex12 = not (False || True)
ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"

-- Basic Functions

-- pattern matching with cases
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

fib:: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- guards
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

tmp :: Integer -> String
tmp n
  | n `mod` 2 == 0 = "EVEN"
  | n `mod` 2 == 1 && n > 10 = "ODD and > 10"
  | otherwise = "ODD and <= 10"

foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

-- Pairs
sumTogether :: (Integer, Integer) -> Integer
sumTogether (x,y) = x+y

-- Multiple args
test :: Char -> Char -> Char -> Char -> String
test a b c d = [a,b,c,d]

-- Constructing Lists
ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []
ex21 = [2,3,4] == 2 : 3 : 4 : []

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

fibSeq :: Integer -> [Integer]
fibSeq 0 = [0]
fibSeq n = (fib n) : fibSeq (n - 1)

bad = error "aslkdjasld"

orLB :: [Bool] -> [Bool] -> Bool
orLB [] [] = False
orLB (x:_) [] = x
orLB [] (x:_) = x
orLB (x:xs) (y:ys)
    | x == True = True
    | y == True = True
    | otherwise = False

-- example of matching case, but guard fails, what happens?
-- in this case "testtest 0" will enter the next case
testtest :: Integer -> String
testtest 0
  | False = "You shall not eneter"
testtest n = "Hi"

-- in this case, get an error about non-exhaustive patterns
testtest2 :: Integer -> String
testtest2 0
  | False = "You shall not eneter"

-- L1.14
unrle :: [(Int, Char)] -> [Char]
unrle [] = ""
unrle ((n,_):rest)
  | n <= 0 = unrle rest
unrle ((n,c) : rest) = c : unrle ((n-1,c):rest)

-- L1.15
go :: [Int] -> Int
go [] = 0       -- empty list
go (x:[]) = 0   -- single element list
go (x:y:[]) = x-y
-- otherwise for (x:y:zs) take max(x-y, go (y:zs))
go (x:y:zs) = maximum ((x-y):(go (y:zs)):[])

-- explicit pattern match with values
foofoo :: Int -> Int
foofoo 2 = 2
foofoo (-1) = (-1)
foofoo n = n+1

-- expressions that pass type check, but causes error when evaluate
bad1 = undefined
bad2 = head []
bad3 = 123 / undefined

-- exaclty one true (lecture2.lhs)
-- brute force
oneTrue :: Bool -> Bool -> Bool -> Bool
oneTrue x y z = (x && not y && not z) || (not x && y && not z) || (not x && not y && z)

-- pattern matching
oneTrue' :: Bool -> Bool -> Bool -> Bool
oneTrue' True False False = True
oneTrue' False True False = True
oneTrue' False False True = True
oneTrue' _ _ _ = False

-- guards
oneTrue'' :: Bool -> Bool -> Bool -> Bool
oneTrue'' x y z
  | x && not y && not z = True
  | not x && y && not z = True
  | not x && not y && z = True
  | otherwise = False