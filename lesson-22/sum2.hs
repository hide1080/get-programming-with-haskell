import System.Environment (getArgs)
import Control.Monad (replicateM)

--L22-5, L22-6, L22-7
main :: IO ()
main = do
  args <- getArgs
  let linesToRead =
        if not (null args)
        then read (head args)
        else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)
