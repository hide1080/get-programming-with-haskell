{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

--Data.Text.Lazyのlinesで変換できるので以下は不要。toIntsを参照
-- linesForText :: T.Text -> [T.Text]
-- linesForText = T.splitOn "\n"

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main::IO ()
main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  print (sum numbers)
