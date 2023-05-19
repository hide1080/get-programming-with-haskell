--L28-11
data User = User {
    name ::     String
  , gamerId ::  Int
  , score ::    Int
} deriving Show

--L28-12
serverUsername :: Maybe String
serverUsername =  Just "Sue"
serverGamerId ::  Maybe Int
serverGamerId =   Just 1337
serverScore ::    Maybe Int
serverScore =     Just 9001

--L28-13
readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter a username, gamerId and score"
  let username =  getLine -- username :: IO String
  let gamerId =   readInt -- gamerId  :: IO Int
  let score =     readInt -- score    :: IO Int
  user <- User <$> username <*> gamerId <*> score
  print user

--QC28-5
--結果全体がNothingになる
