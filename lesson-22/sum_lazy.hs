import Debug.Trace (trace)
-- import Data.List.Split

--L22-8
-- main::IO ()
-- main = do
--   userInput <- getContents
--   mapM_ print userInput

--L22-9
-- sampleData :: [Char]
-- sampleData = ['6','2','\n','2','1','\n']

--L22-10
-- myLines :: String -> [String]
-- myLines = splitOn "\n"

--L22-11
toInts :: String -> [Int]
toInts = map read . lines

--L22-12
main::IO ()
main = do
  userInput <- getContents
  let input = trace ("userInput:" ++ show userInput) userInput
  let numbers = toInts input
  print (sum numbers)
