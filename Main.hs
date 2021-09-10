module Main where
import Data.Text.Titlecase

main :: IO ()
main = putStrLn (titlecase "hello, haskell!!")
