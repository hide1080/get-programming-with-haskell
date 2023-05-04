--Q27-3
import qualified Data.Map as Map

data RobotPart = RobotPart {
    name        :: String
  , description :: String
  , cost       :: Double
  , count       :: Int
}

leftArm :: RobotPart
leftArm = RobotPart {
    name        = "left arm"
  , description = "left arm for face punching!"
  , cost        = 1000.00
  , count      = 3
}
rightArm :: RobotPart
rightArm = RobotPart {
    name        = "right arm"
  , description = "right arm for kind hand gestures"
  , cost        = 1025.00
  , count      = 5
}
robotHead :: RobotPart
robotHead = RobotPart {
    name        = "robot head"
  , description = "this head looks mad"
  , cost        = 5092.25
  , count      = 2
}

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "Item not found"
printCost (Just d) = print d

main :: IO ()
main = do
  putStrLn "enter a number"
  partNo <- getLine
  let part = Map.lookup (read partNo) partsDB
  printCost (cost <$> part)
