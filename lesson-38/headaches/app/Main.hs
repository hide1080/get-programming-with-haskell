module Main (main) where

import Data.Char

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  print (displayResult result)

--L38-1
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

--L38-2
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = [] --QC38-1
myTakePM n (x:xs) = x : myTakePM (n - 1) xs

--L38-3
myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x

--QC38-2
maximumError = maximum [] :: [Int]
succError = succ maxBound :: Int
sumError = sum [1..]

--L38-4
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

--L38-5
myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs <*> myTakeSafer (n - 1) (Just (tail xs))

--L38-?
primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

-- isPrime :: Int -> Maybe Bool
-- isPrime n
--   | n < 2 = Nothing
--   | n > maxN = Nothing
--   | otherwise = Just (n `elem` primes)

--L38-6
eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x : _) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

--QC38-3
qc38_3 = (+) <$> eitherHead intExample <*> eitherHead (tail intExample)

--L38-7
-- isPrime :: Int -> Either String Bool
-- isPrime n
--   | n < 2 = Left "Numbers less than 2 are not candidates for primes"
--   | n > maxN = Left "Value exceeds limits of prime checker"
--   | otherwise = Right (n `elem` primes)

--L38-8
data PrimeError = TooLarge | InvalidValue

--L38-9
instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

--L38-10
isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

--L38-11
displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's Prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

--Q38-1
isDigits :: String -> Bool
isDigits = all isDigit

addStrInts :: String -> String -> Either String Int
addStrInts l r
  | isDigits l && isDigits r = Right (read l + read r)
  | not (isDigits l || isDigits r) = Left "Both arguments are invalid"
  | not (all isDigit l) = Left "The first argument is invalid"
  | not (all isDigit r) = Left "The second argument is invalid"

--Q38-2
succSafer :: (Enum a, Bounded a, Eq a) => a -> Maybe a
succSafer n
  =
    if n /= maxBound
    then Just (succ n)
    else Nothing

tailSafer :: [a] -> [a]
tailSafer [] = []
tailSafer (x : xs) = xs

lastSafer :: [a] -> Either String a
lastSafer [] = Left "empty list"
lastSafer xs = lastSafer' 100 xs
  where
    lastSafer' :: Int -> [a] -> Either String a
    lastSafer' 0 _ = Left "List limit exceeded"
    lastSafer' _ [x] = Right x
    lastSafer' n (x : xs) = lastSafer' (n - 1) xs
