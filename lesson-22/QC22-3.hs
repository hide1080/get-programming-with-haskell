main :: IO ()
main = do
  userInput <- getContents
  putStrLn (reverse userInput)
