--QC29-1
concatMaybeStr :: Maybe String -> Maybe String -> Maybe String
concatMaybeStr v1 v2 = (++) <$> v1 <*> v2

--QC29-2
helloWorldIO :: IO String
helloWorldIO = pure "Hello, World!"

--QC29-3
-- 書籍抜粋
-- 問題
-- pure (+) <*> (1,2) <*> (3,4) = (1+2,1+4,2+3,2+4) = (3,5,5,6)になるようにしたいとしましょう。
-- この方法がうまくいかないのはなぜでしょうか。
-- 解答
-- 「この方法がうまくいかないのは、(3,5,5,6)が(1,2)や(3,4)とはまったく異なる型だからです。
-- 最初の型は(a,b,c,d)、他の2つの型は(a,b)です。」

--L29-4
doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

--L29-5
boxPrize :: [Int]
boxPrize = [500, 20000]

--L29-6
-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize
totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

--QC29-4
boxRate :: [Int]
boxRate = [10, 50]
ratedPrize :: [Int]
ratedPrize = pure (*) <*> doorPrize <*> boxRate

--L29-7
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2..n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

--L29-8
testNames :: [String]
testNames = [
      "John Smith"
    , "Robert'); DROP TABLE Students;--"
    , "Christina NULL"
    , "Randall Munroe"
    , "hide1"
  ]

--L29-9
testIds :: [Int]
testIds = [
      1337
    , 0123
    , 999999
  ]

--L29-10
testScores :: [Int]
testScores = [
      0
    , 100000
    , -99999
  ]

data User = User {
    name ::     String
  , gamerId ::  Int
  , score ::    Int
} deriving Show

--L29-11
testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

--QC29-5
-- testNamesに名前を一つ追加すると45個になる