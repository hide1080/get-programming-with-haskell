import System.Random

--QC25-3
randomChar :: IO Char
randomChar = do
  randomInt <- randomRIO (0, 255)
  return (toEnum randomInt)

main :: IO ()
main = do
  char <- randomChar
  print char
