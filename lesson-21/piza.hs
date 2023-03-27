import qualified Data.Map as Map

--L21-4
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

--L21-5
type Pizza = (Double, Double)

--L21-6
costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

--L21-7
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2
  =
    if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

--L21-8
describePizza :: Pizza -> String
describePizza (size, cost)
  =
    "The "
    ++ show size
    ++ " pizza "
    ++ "is cheaper at "
    ++ show costSqInch
    ++ " per square inch"
  where
    costSqInch = costPerInch (size, cost)

--L21-9
main :: IO ()
main = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

--Monadのチラ見：Maybeでのdo表記（MaybeもIOもMonadのメンバ）
--Maybeのコンテキストは、それらの値が存在しない可能性があること
--IOのコンテキストは、現実世界との接点であること、そしてデータの振る舞いがHaskellの純粋な部分と異なる可能性があること

--L21-10~12ではL21-9のIOをMaybeにしてみる

--L21-10
costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

--L21-11
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

--L21-12
maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  size3 <- Map.lookup 3 sizeData
  cost3 <- Map.lookup 3 costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let pizza3 = (size3, cost3)
  let betterPizza = comparePizzas pizza1 pizza2
  let bestPizza = comparePizzas betterPizza pizza3
  --returnは、do表記のコンテキストに戻す
  --この場合、StringがMaybe Stringとなって返される
  return (describePizza bestPizza)
