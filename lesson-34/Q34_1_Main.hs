--Q34-1
module Main where

import qualified Q34_1_Palindrome as P

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- TIO.getLine
  --L34-11
  -- let response = if isPalindrome text
  let response = if P.isPalindrome text
                 then "it is!"
                 else "it's not!"
  print response
