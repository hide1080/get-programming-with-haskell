module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

main :: IO ()
main = print "Lesson 42"

--L42-4
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 9) [(3, True)]

--L42-5
oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 10) $ zip [1 .. 10] $ repeat True

--L42-6
-- beansInBuckets :: UArray Int Int
-- beansInBuckets = array (0, 3) []

--L42-7
updateBiB :: UArray Int Int
updateBiB = beansInBuckets // [(1, 5), (3, 6)]

--L42-8, L42-9 中間状態を作ってしまうので避けた方が良い
-- listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
-- listToSTUArray vals = do
--   let end = length vals - 1
--   myArray <- newArray (0, end) 0
--   forM_ [0 .. end] $ \ i -> do
--     let val = vals !! i
--     writeArray myArray i val
--   return myArray

--L42-10, L42-11 最終的にlistToSTUArrayを内部に実装した
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \ i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

--L42-12
-- myData :: UArray Int Int
-- myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

--L42-13
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray           -- UArrayをSTUArrayとして解凍
  let end = (snd . bounds) myArray  -- 配列の最終要素はタプルboundsのsecond
  forM_ [1 .. end] $ \ i -> do
    forM_ [0 .. (end - i)] $ \ j -> do
      val <- readArray stArray j    -- readArrayを使うとSTUArrayから値を見れる
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do          --when関数により条件が満たされた場合にのみdoを実行
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray



--QC42-1
qcArray :: UArray Int Bool
qcArray = array (0, 4) [(1, True), (2, True)]

--QC42-2
beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) [(0, 0), (0, 0), (0, 0), (0, 0)]

--QC42-3
update3TimesBiB :: UArray Int Int
update3TimesBiB = accum (*) updateBiB $ zip [0 .. 3] $ repeat 3

--QC42-4
myData :: UArray Int Int
myData = listToUArray [7, 9, 6, 4, 3, 8, 10, 2]



--Q42-1
crossOver :: (UArray Int Int, UArray Int Int) -> Int -> UArray Int Int
crossOver (a, b) n = runSTUArray $ do
  sta <- thaw a
  let end = (snd . bounds) a
  forM_ [n .. end] $ \ i -> do
    writeArray sta i $ b ! i
  return sta

crossOverTest :: UArray Int Int
crossOverTest = crossOver (listToUArray [1,1,1,1,1], listToUArray [0,0,0,0,0]) 3

-- ghci> crossOverTest 
-- array (0,4) [(0,1),(1,1),(2,1),(3,0),(4,0)]

--Q42-2
replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [0 .. end] $ \ i -> do
    val <- readArray stArray i
    let isZero = val == 0
    when isZero $ do
      writeArray stArray i (-1)
  return stArray

replaceZerosTest :: UArray Int Int
replaceZerosTest = replaceZeros (listToUArray [0, -1, 2, 0, 0])

-- ghci> replaceZerosTest
-- array (0,4) [(0,-1),(1,-1),(2,2),(3,-1),(4,-1)]