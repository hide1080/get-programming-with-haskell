--Q32-2

monthEnds :: [Int]
monthEnds =  [31,28,31,30,31,30,31,31,30,31,30,31]

datesByDoComprehension :: [Int] -> [Int]
datesByDoComprehension ends = do
  end <- ends
-- long version
  -- date <- [1 .. end]
  -- return date
-- short version
  [1 .. end]

datesByPure :: [Int] -> [Int]
-- long version
-- datesByPure ends = ends >>= (\ end -> [1 .. end] >>= (\ date -> return date))
-- short version
datesByPure ends = ends >>= (\ end -> [1 .. end])
