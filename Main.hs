module Main where
import Data.Text.Titlecase

main :: IO ()
main = putStrLn (titlecase "hello, haskell!!")

x :: Int
x=3
-- x=4 compile error "Multiple declarations of x"

y :: Int
y=y+1

