--QC34-3
module Main where

import Palindrome

main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  --L34-11
  -- let response = if isPalindrome text
  let response = if isPalindrome text
                 then "it is!"
                 else "it's not!"
  print response

--ghc -o Main.exe Main2.hs
