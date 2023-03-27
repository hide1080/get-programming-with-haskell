{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

--L23-1
firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

--QC23-1
fourthWord :: T.Text
fourthWord = T.pack thirdWord

--L23-2
myWord1 :: String
myWord1 = "dog"
--同じ数値リテラルを別の型で使用してもコンパイルエラーにならないのに
--文字列リテラルをData.Textとして使おうとするとエラーになってしまう
--Couldn't match type ‘[Char]’ with ‘T.Text’
-- myWord2 :: T.Text
-- myWord2 = "dog"
--これは言語拡張 OverloadedStringsを使うと解決できる。text.hsを参照

--L23-3
myNum1 :: Int
myNum1 = 3
myNum2 :: Integer
myNum2 = 3
myNum3 :: Double
myNum3 = 3


--L23-5
sampleImput :: T.Text
sampleImput = "this\nis\ninput"

--L23-6
someText :: T.Text
someText = "Some\ntext for\t you"

--L23-7
breakText :: T.Text
breakText = "simple"
exampleText :: T.Text
exampleText = "This is simple to do"

ws :: T.Text
ws = " "

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]
combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

--QC23-3
linesForText :: T.Text -> [T.Text]
linesForText = T.splitOn "\n"
unlinesForText :: [T.Text] -> T.Text
unlinesForText = T.intercalate "\n"
