-- IO type
main = putStrLn "Hello, Haskell!"

-- Combining IO
main1 = putStrLn "Hi"
    >> putStrLn "Hello"
    >> putStrLn "Hey"

main2 :: IO ()
main2 = putStrLn "Please enter a number: "
  >> readLn
  >>= (\n -> putStrLn (show (n+1)))
  