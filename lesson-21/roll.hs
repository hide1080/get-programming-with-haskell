import System.Random

--QC21-2
minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
  dieRoll <- randomRIO (minDie, maxDie)
  putStrLn (show dieRoll)
