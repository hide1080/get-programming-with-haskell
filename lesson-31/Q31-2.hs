--Q31-2
import qualified Data.Map as Map

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2
  =
    if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

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

listMain :: [String]
listMain
  =
    do
      size1 <- [10, 12, 17]
      cost1 <- [12.0, 15.0, 20.0]
      size2 <- [10, 11, 18]
      cost2 <- [13.0, 14.0, 21.0]
      let pizza1 = (size1, cost1)
      let pizza2 = (size2, cost2)
      let betterPizza = comparePizzas pizza1 pizza2
      return (describePizza betterPizza)
