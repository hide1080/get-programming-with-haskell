-- import Data.List
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
-- import Data.Semigroup
-- import Data.Maybe

--L20-2
file1 :: [(Int,Double)]
file1 = [(1, 200.1),(2, 199.5),(3, 199.4),(4, 198.9),(5, 199.0),(6, 200.2),(9, 200.3),(10, 201.2),(12, 202.9)]
file2 :: [(Int,Double)]
file2 = [(11, 201.6),(12, 201.5),(13, 201.5),(14, 203.5),(15, 204.9),(16, 207.1),(18, 210.5),(20, 208.8)]
file3 :: [(Int,Double)]
file3 = [(10, 201.2),(11, 201.6),(12, 201.5),(13, 201.5),(14, 203.5),(17, 210.5),(24, 215.1),(25, 218.7)]
file4 :: [(Int,Double)]
file4 = [(26, 219.8),(27, 220.5),(28, 223.8),(29, 222.8),(30, 223.8),(31, 221.7),(32, 222.3),(33, 220.8),(34, 219.4),(35, 220.1),(36, 220.6)]

--L20-3
data TS a = TS [Int] [Maybe a]

--L20-4
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    extendedValues = map (`Map.lookup` timeValueMap) completeTimes

--L20-5
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where
    splitPairs = unzip tvPairs
    times = fst splitPairs
    values = snd splitPairs

--L20-6
showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

--L20-7
instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

--L20-8
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

--L20-9
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (k, (Just v)) = Map.insert k v myMap

--L20-10
combineTS :: TS a -> TS a -> TS a
combineTS (TS [][]) ts2 = ts2
combineTS ts1 (TS [][]) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [(minimum bothTimes) .. (maximum bothTimes)]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    combinedValues = map (`Map.lookup` updatedMap) completeTimes

--L20-11
instance Semigroup (TS a) where
  (<>) = combineTS

--L20-12
instance Monoid (TS a) where
  mempty = TS [][]

--L20-13
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

--L20-14
mean :: Real a => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

--L20-15
meanTS :: Real a => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values)
  =
    if all (== Nothing) values
    then Nothing
    else Just avg
  where
     justVals = filter isJust values
     cleanVals = map (\ (Just x) -> x) justVals
     avg = mean cleanVals

--L20-16
type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = newFunc
  where
    newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, v) = (i, v)
    newFunc (i, v) (_, Nothing) = (i, v)
    newFunc (i1, Just v1) (i2, Just v2) =
      if f v1 v2 == v1
      then (i1, Just v1)
      else (i2, Just v2)

--L20-17
compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS f (TS [] []) = Nothing
compareTS f (TS ts vs)
  =
    if all (== Nothing) vs
    then Nothing
    else Just best
  where
    pairs = zip ts vs
    best = foldl (makeTSCompare f) (0, Nothing) pairs

--L20-18
minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

--L20-19
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a

--L20-20
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

--L20-21
diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS ts vs) = TS ts (Nothing : diffVals)
  where
    shiftVals = tail vs
    diffVals = zipWith diffPair shiftVals vs

--L20-22
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vs
  =
    if Nothing `elem` vs
    then Nothing
    else Just avg
  where
    avg = mean (map fromJust vs)

--L20-23
movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vs n
  =
    if length nextVals == n
    then meanMaybe nextVals : movingAvg restVals n
    else []
  where
    nextVals = take n vs
    restVals = tail vs

--L20-24
maTS :: Real a => TS a -> Int -> TS Double
maTS (TS [] []) n = TS [] []
maTS (TS ts vs) n
  =
    TS ts smoothedVals
  where
    ma = movingAvg vs n
    nothings = replicate (n `div` 2) Nothing
    smoothedVals = mconcat [nothings, ma, nothings]
