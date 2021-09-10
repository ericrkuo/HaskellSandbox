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