--L34-6
module Main where

--L34-10
import qualified Palindrome as P

isPalindrome :: String -> Bool
isPalindrome text = text == reverse text

main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  --L34-11
  -- let response = if isPalindrome text
  let response = if P.isPalindrome text
                 then "it is!"
                 else "it's not!"
  print response
