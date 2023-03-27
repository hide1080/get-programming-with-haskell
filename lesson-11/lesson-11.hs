--11.1 Haskellの型
x :: Int  -- 型シグネチャ
x = 2     -- 変数定義

--11.2 関数の型
double :: Int -> Int  -- 型シグネチャ
double n = n * 2      -- 関数定義

half :: Int -> Double
half n = fromIntegral n / 2 -- IntのnをDoubleに変換

--QC11-1
-- halve :: Int -> Int --NG
halve :: Integer -> Integer
halve n = n `div` 2

anotherNumber :: Int
anotherNumber = read "6"

--QC11-2
printDouble :: Int -> String
printDouble n = show (double n)

readAsInt = read "6" :: Int
readAsDouble = read "6" :: Double

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

--QC11-3
-- makeAdd3 :: String -> (Int, String, String)
-- makeAdd2 :: String -> String -> (Int, String, String)
-- makeAdd1 :: Int -> String -> String -> (Int, String, String)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
  then f n
  else n

simple :: a -> a
simple x = x

--QC11-4
myMap :: (a -> a) -> [a] -> [a]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
--myMapRet = myMap show [1,2,3,4]
--(a -> a)は引数と同じ型を返す必要がある。
--myMap show [1,2,3,4]のような呼び出しでは第一引数の関数はその引数と戻り値が別の型になる。

--Q11-1
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p list = filter p list
--filterの型シグネチャは、第二引数の型と戻り値の型を変えることができない点がmapとは異なる。

--Q11-2
myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

myHead :: [a] -> a
myHead (x:xs) = x
--空のリストで呼び出されたときに空のリストを返すheadを作成することは不可能。
--型シグネチャはリストの要素の型を返すと宣言しているため。

--Q11-3
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
