module Palindrome
    ( isPalindrome
    ) where

import Lib
    ( stripWhiteSpace
    , stripPunctuation
    )
import qualified Data.Text as T


preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
