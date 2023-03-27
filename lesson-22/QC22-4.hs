import Debug.Trace (trace)

toInts :: String -> [Int]
toInts = map read . lines

toSqrs :: [Int] -> [Int]
toSqrs = map (^2)

--L22-12
main::IO () --for GHC compiled executable
main = do
-- mainSumSquares::IO ()
-- mainSumSquares = do
  userInput <- getContents
  let input = trace ("userInput:" ++ show userInput) userInput
  let numbers = toInts input
  let squares = toSqrs numbers
  print (sum squares)
