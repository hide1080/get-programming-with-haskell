{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

--L23-2
myWord1 :: String
myWord1 = "dog"
myWord2 :: T.Text
myWord2 = "dog"

--L23-4
aWord :: T.Text
aWord = "Cheese"

main :: IO()
main = do
  print aWord
