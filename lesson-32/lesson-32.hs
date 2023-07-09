import Control.Monad
import Data.Char

--L32-?1
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (2 ^) [1 .. n]

--L32-2
powersOfTwoAndThree_ :: Int -> [(Int, Int)]
powersOfTwoAndThree_ n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

--L32-?4
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = [ (powersOfTwo, powersOfThree)
    | value <- [1 .. n]
    , let powersOfTwo = 2 ^ value
    , let powersOfThree = 3 ^ value
  ]

--L32-?2
allEvenOdds_ :: Int -> [(Int, Int)]
allEvenOdds_ n = do
  evenValue <- [2, 4 .. n]
  oddValue <- [1, 3 .. n]
  return (evenValue, oddValue)

--L32-?5
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = [ (evenValue, oddValue)
    | evenValue <- [2, 4 .. n]
    , oddValue <- [1, 3 .. n]
  ]

--Q32-1
int1To10AndSquare :: [(Int, Int)]
int1To10AndSquare = do
  n <- [1 .. 10]
  let square = n * n
  return (n, square)

--L32-?3
evensGuard_ :: Int -> [Int]
evensGuard_ n = do
  value <- [1 .. n]
  guard(even value)
  return value

--L32-?6
evensGuard :: Int -> [Int]
evensGuard n = [ value | value <- [1 .. n] , even value ]

--Q32-2
filter :: (a -> Bool) -> [a] -> [a]
filter p list = do
  value <- list
  guard(p value)
  return value

--L32-3
evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n ^ 2
  guard(even nSquared)
  return nSquared

--Q32-3
qc32_3 :: [String]
qc32_3 = [ "Mr." ++ cap
    | color <- ["brown", "blue", "pink", "orange"]
    , let cap = (\ (x:xs) ->  toUpper x:xs) color
  ]
