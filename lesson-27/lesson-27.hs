import qualified Data.Map as Map

--L27-1
successfulRequest :: Maybe Int
successfulRequest = Just 6
failedRequest :: Maybe Int
failedRequest = Nothing

--L27-2
incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

--QC27-1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just s) = Just (reverse s)
reverseMaybe _ = Nothing

--L27-3
-- instance Functor Maybe where
--   fmap func (Just n) = Just (func n)
--   fmap func Nothing = Nothing

--L27-4
successStr :: Maybe String
successStr = show <$> successfulRequest
failStr :: Maybe String
failStr = show <$> failedRequest

--QC27-2
reverseMaybe2 :: Maybe String -> Maybe String
reverseMaybe2 = fmap reverse
-- reverseMaybe2 = (<$>) reverse

--L27-5
data RobotPart = RobotPart {
    name        :: String
  , description :: String
  , cost       :: Double
  , count       :: Int
}

--L27-6
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

--L27-7
type Html = String
renderHtml :: RobotPart -> Html
renderHtml part
  =
    mconcat [
        "<h2>", partName, "</h2>"
      , "<p><h3>desc</h3>", partDesc, "</p>"
      , "<p><h3>cost</h3>", partCost, "</p>"
      , "<p><h3>count</h3>", partCount, "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

--L27-8
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

--L27-9
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

--L27-10
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

--L27-11
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

--L27-12
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

--L27-13 L27-12のコードは以下のコードと同じ。Listの<$>は単なるmapであるため
allPartsHtml_ :: [Html]
allPartsHtml_ = map renderHtml allParts

--QC27-3 L27-11のコードと同じ
allParts_ :: [RobotPart]
allParts_ = snd <$> Map.toList partsDB

--L27-14
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

--L27-15
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

--L27-16
htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO
