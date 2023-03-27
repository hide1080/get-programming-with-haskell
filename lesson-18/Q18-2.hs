import qualified Data.Map as Map

data Organ  = Heart
            | Brain
            | Kidney
            | Spleen
            deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney, Heart]

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
