module Main (main) where

import Primes
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a number to check if it's prime: "
  num <- getLine
  let result = isPrime (read num :: Int) 
  case result of
    Just True  -> putStrLn "It is prime!"
    Just False -> putStrLn "It's not prime."
    Nothing    -> putStrLn "Sorry, this number is not a valid candidate for primality testing."
