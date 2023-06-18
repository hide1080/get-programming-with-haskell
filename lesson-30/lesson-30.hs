import qualified Data.Map as Map

--L30-1
main :: IO ()
main = do
  putStrLn "Remember do-notation!"
  putStrLn "It makes things easy!"

--L30-2
type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [
      (1, "nYarlathoTep")
    , (2, "KINGinYELLOW")
    , (3, "dagon1997")
    , (4, "rcarter1919")
    , (5, "xCTHULHUx")
    , (6, "yogSOThoth")
  ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [
      ("nYarlathoTep", 2000)
    , ("KINGinYELLOW", 15000)
    , ("dagon1997", 300)
    , ("rcarter1919", 12)
    , ("xCTHULHUx", 50000)
    , ("yogSOThoth", 150000)
  ]

--L30-3
-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- ?

--L30-4
lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

--L30-5 FunctorまたはApplicativeを使わない解決策
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

--L30-6
-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId gamerId = altLookupCredits (lookupUserName gamerId)

--QC30-1
creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id
-- 結果が入れ子のコンテキストになる。
-- Mybe (Maybe PlayerCredits)

--QC30-2
-- IOのコンテキストでは、Maybeのコンテキストのように値を取り出す方法がない。

--L30-7
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

--L30-8
type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1),(1002,2),(1003,3),(1004,4),(1005,5),(1006,6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

--L30-9
creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id =
  lookupGamerId id
  >>= lookupUserName
  >>= lookupCredits

--QC30-3
readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

readIntToPrintDouble :: IO ()
readIntToPrintDouble = readInt >>= printDouble

--L30-12
askForName :: IO ()
askForName = putStrLn "What is your name?"

--L30-13
nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

--QC30-4
plus2AndReturn :: Num a => (a -> IO a)
plus2AndReturn = (\ n -> return ((+2) n))

--L30-14
helloName :: IO ()
helloName =
  askForName
  >> getLine
  >>= (\ name -> return (nameStatement name))
  >>= putStrLn
