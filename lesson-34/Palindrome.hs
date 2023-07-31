--L34-7, QC34-2
module Palindrome(isPalindrome) where
--QC34-2
-- module Palindrome(isPalindrome, preprocess) where

import Data.Char (toLower, isSpace, isPunctuation)

-- ホワイトスペースをすべて除去
stripWhiteSpace :: String -> String
stripWhiteSpace = filter (not . isSpace)

-- 句読点をすべて除去
stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

-- 文字列全体を小文字に変換
toLowercase :: String -> String
toLowercase = map toLower

preprocess :: String -> String
preprocess = stripWhiteSpace . stripPunctuation . toLowercase

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where cleanText = preprocess text
