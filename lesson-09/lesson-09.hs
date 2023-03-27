import Data.Char
import Distribution.Simple.Program.HcPkg (list)

--9-1
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

add3ByAll xs = myMap (\ a -> a + 3) xs

mul3ByAll xs = myMap (\ a -> a * 3) xs

addAnA [] = []
addAnA (x:xs) = ("a " ++ x ) : addAnA xs

squareAll [] = []
squareAll (x:xs) = ((^2) x) : squareAll xs

--9-3
myFilter p [] = []
myFilter p (x:xs) =
  if p x
  then x : myFilter p xs
  else myFilter p xs

--QC9-1
remove p [] = []
remove p (x:xs) =
  if p x
  then remove p xs
  else x : remove p xs

--QC9-2
-- myProduct [] = 0
myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y : x

myReverse xs = foldl rcons [] xs

myReverse2 xs = myFoldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

--QC9-3
-- myFoldlは終了条件ではない手順で終了しない。

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs

--9.6
--Q9-1
myElem a list = len > 0
  where len = length (filter (== a) list)

--Q9-2
myIsPalindrome text = trimed == reverse trimed
  where lowered = map toLower text
        trimed = filter (/= ' ') lowered

--Q9-3
myHarmonic n = foldl (\ x y -> x + 1 / y) 0 (take n [1 ..])
  -- where list = take n [1 ..] -- これは遅延評価にならない
--  where list = [1 .. n] -- これも遅延評価にならない
-- ghci> myHarmonic 100000000
-- *** Exception: stack overflow

harmonic n = sum (take n seriesValues)
  where seriesPair = zip (cycle [1.0]) [1.0, 2.0 ..]
        seriesValues = map (\ pair -> (fst pair) / (snd pair)) seriesPair
