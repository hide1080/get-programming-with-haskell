module Q34_1_Palindrome
  ( isPalindrome
  )
where

import Data.Char
  ( toLower
  , isSpace
  , isPunctuation
  )
import qualified Data.Text as T

-- ホワイトスペースをすべて除去
stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

-- 句読点をすべて除去
stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

-- 文字列全体を小文字に変換
toLowercase :: T.Text -> T.Text
toLowercase = T.toLower

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowercase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
