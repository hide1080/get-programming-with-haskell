--L28-7
minOfThree :: (Ord a) => a -> a -> a-> a
minOfThree v1 v2 v3 = min v1 (min v2 v3)

--L28-8
readInt :: IO Int
readInt = read <$> getLine

--L28-9
minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

--L28-10
main :: IO ()
main = do
  putStrLn "Enter three numbers"
  minInt <- minOfInts
  putStrLn (show minInt ++ " is the smallest")

--QC28-4
qc28_4 = minOfThree <$> Just 10 <*> Just 3 <*> Just 6
