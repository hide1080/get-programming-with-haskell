--Q25-1
import System.Environment
import System.IO
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do
  args <- getArgs
  let source = head args
  input <- B.readFile source
  putStrLn "Bytes:"
  print (B.length input)
  putStrLn "Characters:"
  print ((T.length . TE.decodeUtf8) input)
