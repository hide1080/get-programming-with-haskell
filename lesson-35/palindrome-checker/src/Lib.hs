module Lib
    ( stripWhiteSpace
    , stripPunctuation
    ) where

import Data.Char
    ( isSpace
    , isPunctuation
    )
import qualified Data.Text as T

-- ホワイトスペースをすべて除去
stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

-- 句読点をすべて除去
stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)
