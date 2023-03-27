import qualified Data.Map as Map

data Organ  = Heart
            | Brain
            | Kidney
            | Spleen
            deriving (Show, Eq, Ord, Enum)

maybeOrgans :: [Maybe Organ]
maybeOrgans
  = [
      Just Brain,
      Just Heart,
      Nothing,
      Just Brain,
      Nothing,
      Nothing,
      Just Kidney
    ]

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers maybeOrgans = length (filter (== Nothing) maybeOrgans)

emptyCount :: Int
emptyCount = emptyDrawers maybeOrgans
