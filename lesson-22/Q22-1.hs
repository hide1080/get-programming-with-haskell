sampleInput :: [String]
sampleInput = ["21", "+" ,"123"]

calc :: [String] -> Int
calc (v1 : "+" : v2 : rest) = read v1 + read v2
calc (v1 : "*" : v2 : rest) = read v1 * read v2

main :: IO ()
main = do
  userInput <- getContents
  let values = lines userInput
  print (calc values)
