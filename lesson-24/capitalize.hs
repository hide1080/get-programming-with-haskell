--Q24-2
--指定されたファイルの内容を読み取って大文字で書き換える

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let capitalized = T.toUpper input
  TIO.writeFile fileName capitalized
