--Q32-1

monthEnds :: [Int]
monthEnds =  [31,28,31,30,31,30,31,31,30,31,30,31]

datesByListComprehension :: [Int] -> [Int]
datesByListComprehension ends = [date | end <- ends, date <- [1 .. end]]
