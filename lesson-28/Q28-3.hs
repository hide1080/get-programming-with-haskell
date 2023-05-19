--Q28-3
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

leftLeg :: RobotPart
leftLeg = RobotPart {
    name        = "left leg"
  , description = "left leg for kicking back!"
  , cost        = 1300.00
  , count      = 6
}

rightLeg :: RobotPart
rightLeg = RobotPart {
    name        = "right leg"
  , description = "right leg for kicking back!"
  , cost        = 1350.00
  , count      = 7
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
    keys = [1, 2, 3, 4, 5]
    vals = [leftArm, rightArm, robotHead, leftLeg, rightLeg]
    keyVals = zip keys vals

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "Missing item."
printCost (Just v) = print v

main :: IO ()
main = do
  putStrLn "Enter a part number 1:"
  partNo1 <- getLine
  putStrLn "Enter a part number 2:"
  partNo2 <- getLine
  let part1 = Map.lookup (read partNo1) partsDB
  let part2 = Map.lookup (read partNo2) partsDB
  let cheapest = min <$> (cost <$> part1) <*> (cost <$> part2)
  printCost cheapest
