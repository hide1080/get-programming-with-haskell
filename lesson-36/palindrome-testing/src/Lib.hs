module Lib
    ( isPalindrome
    , preprocess
    ) where
--QC36-1
--全ての関数をエクスポートする
-- module Lib where

import Data.Char
    ( isSpace
    , isPunctuation
    )
import Data.Text as T

-- preprocess :: T.Text -> T.Text
-- preprocess = T.filter (not . isPunctuation)

-- isPalindrome :: T.Text -> Bool
-- isPalindrome text = cleanText == T.reverse cleanText
--     where cleanText = preprocess text

--Q36-1

-- ホワイトスペースをすべて除去
stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

-- 句読点をすべて除去
stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
