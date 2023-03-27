--Q24-1
--コマンドラインの第1引数に指定されたファイルを
--第2引数に指定されたファイルにコピーする簡単な例

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  let srcFileName = head args
  let dstFileName = head (tail args)
  input <- TIO.readFile srcFileName
  TIO.writeFile dstFileName input
