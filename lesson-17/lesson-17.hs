--L17-1
import Data.List(sort)
import Data.Char(isUpper)

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

allUpper :: String -> Bool
allUpper = myAll isUpper

--QC17-1
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

anyUpper :: String -> Bool
anyUpper = myAny isUpper

--L17-2
instance Semigroup Integer where
  (<>) x y = x + y

--QC17-2
--IntをSemigroupのインスタンスにするために(/)を使用することは不可能。
--理由はIntの除算結果はDoubleになりIntにならないからルルールに違反するため。

--L17-3
data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  deriving (Show, Eq)

--L17-4 Semigroup型クラスの結合律を破っているダメな例
-- instance Semigroup Color where
--   (<>) Red Blue = Purple
--   (<>) Blue Red = Purple
--   (<>) Yellow Blue = Green
--   (<>) Blue Yellow = Green
--   (<>) Yellow Red = Orange
--   (<>) Red Yellow = Orange
--   (<>) a b =
--     if a == b
--     then a
--     else Brown

--L17-5 Semigroup型クラスの結合律を満たした例
instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b  | a == b = a
            | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
            | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
            | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
            | otherwise = Brown

--QC17-3
--Integerに対するSemigroupの実装は結合律をサポートする

--L17-6
-- class Semigroup a => Monoid a where
--   identity :: a

--L17-7
-- class Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a

--QC17-4
--Integerに対するmappendまたは<>を+ではなく*で実装した場合のmemptyの値は何か。
--mempty（単位元）の値は、1

--L17-8
type Events = [String]
type Probs = [Double]

--L17-9
data PTable = PTable Events Probs

--L17-10
createPTable :: Events -> Probs -> PTable
createPTable events probs =
  PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

--L17-11
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

--L17-12
instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

--L17-13
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

--L17-14
combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = (\ x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

--L17-15
instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

--L17-16
instance Monoid PTable where
  mempty = PTable [] []
  -- mappend = (<>) -- Semigroupから継承済み

--L17-17
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]
spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]
