--L19-1
-- data Maybe a
--   = Nothing
--   | Just a

-- Lesson 19でもLesson 18に引き続きOrganを使用する

import qualified Data.Map as Map
import Data.Maybe
import Data.List

data Organ  = Heart
            | Brain
            | Kidney
            | Spleen
            deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24, 27]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map count allOrgans
  where
    count organ = (length . filter (== organ)) values

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)

--QC19-1
-- この例のNoghingの型を調べてみる --> Maybe Organ
-- ghci> :t Map.lookup 6 organCatalog 
-- Map.lookup 6 organCatalog :: Maybe Organ
-- ただのNothingの型を調べてみる --> Maybe a
-- ghci> :t Nothing 
-- Nothing :: Maybe a

--L19-2
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

--L19-3
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents id = Map.lookup id catalog

--L19-4
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

--L19-5
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (== Just organ) available)

--L19-6
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething _       = True

--L19-7
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

justTheOrgans2 :: [Maybe Organ]
justTheOrgans2 = filter isJust availableOrgans

--L19-8
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan _ = ""

--L19-9
organList :: [String]
organList = map showOrgan justTheOrgans

--L19-?
cleanList :: String
cleanList = intercalate "," organList

--QC19-2
numOrZero :: Maybe Int -> Int
numOrZero Nothing   = 0
numOrZero (Just n)  = n

--L19-10
data Container
  = Vat Organ
  | Cooler Organ
  | Bag Organ

instance Show Container where
  show (Vat organ)    = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ)    = show organ ++ " in a bag"

data Location
  = Lab
  | Kitchen
  | Bathroom
  deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

--L19-11
process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container)
  = show container
  ++ " in the "
  ++ show location

--L19-12 organがMaybe Organを返すのでコンパイルできない
-- processRequest :: Int -> Map.Map Int Organ -> String
-- processRequest id catalog = report (process organ)
--   where
--     organ = Map.lookup id catalog

--L19-12
processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

--L19-14
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where
    organ = Map.lookup id catalog

--QC19-3
report_QC19_3 :: Maybe (Location, Container) -> String
report_QC19_3 Nothing = "container not found"
report_QC19_3 (Just (loc, con))
  = show con
  ++ " in the "
  ++ show loc
