import qualified Data.Map as Map

--Tips
maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM mPair = mPair >>= (\ (x, y) -> return(max x y))

--L31-1
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName =
  askForName
  >> getLine
  >>= (\ name -> return (nameStatement name))
  >>= putStrLn

--L31-2
helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)

--L31-4
echo_ :: IO ()
echo_ = getLine >>= putStrLn

--QC31-1
echo :: IO ()
echo = do
  line <- getLine
  putStrLn line

--L31-5
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

--L31-6
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

--L31-7
data Candidate = Candidate {
    candidateId ::  Int
  , codeReview ::   Grade
  , cultureFit ::   Grade
  , education ::    Degree
} deriving Show

--L31-8
viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

--QC31-2, L31-12
candidate1 :: Candidate
candidate1 = Candidate {
    candidateId = 1
  , codeReview =  A
  , cultureFit =  A
  , education =   BA
}

candidate2 :: Candidate
candidate2 = Candidate {
    candidateId = 2
  , codeReview =  C
  , cultureFit =  A
  , education =   PhD
}

candidate3 :: Candidate
candidate3 = Candidate {
    candidateId = 3
  , codeReview =  A
  , cultureFit =  B
  , education =   MS
}

resultOfCandidate1 = viable candidate1 -- False
resultOfCandidate2 = viable candidate2 -- True

--L31-9
readInt :: IO Int
readInt = getLine >>= (return . read)

-- readGrade :: IO Grade
-- readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

--QC31-3
readGrade :: IO Grade
readGrade = do
  strGrade <- getLine
  return (read strGrade)

--L31-10
readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return Candidate {
        candidateId = cId
      , codeReview =  codeGrade
      , cultureFit =  cultureGrade
      , education =   degree
    }

--L31-11
assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement =
        if passed
        then "passed"
        else "failed"
  return statement

--L31-13
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

--L31-14
assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement =
        if passed
        then "passed"
        else "failed"
  return statement

--QC31-4
maybeToString :: Maybe String -> String
maybeToString (Just s) = s
maybeToString Nothing = "error id not found"

--L31-15
candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

--L31-16
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
        then "passed"
        else "failed"
  return statement

--L31-17
assessCandidates :: [Candidate] -> [String]
assessCandidates [] = []--TODO

--QC31-5
--assessCandidateList は空のリストに対処するか。
--対処する。空のリストを与えると空のリストが返る

--L31-18
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
        then "passed"
        else "failed"
  return statement
