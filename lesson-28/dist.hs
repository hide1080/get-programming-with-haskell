import qualified Data.Map as Map

--L28-1
type LatLon = (Double, Double)

locationDB :: Map.Map String LatLon
locationDB = Map.fromList [
      ("Arkham",    (42.6054, -70.7829))
    , ("Innsmouth", (42.8250, -70.8150))
    , ("Carcosa",   (29.9714, -90.7694))
    , ("New York",  (40.7776, -73.9691))
  ]

--L28-2
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLonToRads :: LatLon -> (Double, Double)
latLonToRads (lat, lon) = (rlat, rlon)
  where
    rlat = toRadians lat
    rlon = toRadians lon

haversine :: LatLon -> LatLon -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlon1) = latLonToRads coords1
    (rlat2, rlon2) = latLonToRads coords2
    dlat = rlat2 - rlat1
    dlon = rlon2 - rlon1
    a = sin (dlat / 2) ^ 2 + cos rlat1 * cos rlat2 * sin (dlon / 2) ^ 2
    c = 2 * atan2  (sqrt a) (sqrt (1 - a))
    earthRadius = 3961.0

--L28-3
printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

--L28-4
haversineMaybe :: Maybe LatLon -> Maybe LatLon -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just v1) (Just v2) = Just (haversine v1 v2)

--QC28-1
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just n1) (Just n2) = Just (n1 + n2)
addMaybe _ _ = Nothing

--QC28-2 部分適用関数を作成する
newYork :: LatLon
newYork = (101.0, 102.0)
distanceFromNY = haversine newYork

--L28-5
maybeInc :: Maybe (Integer -> Integer)
maybeInc = (+) <$> Just 1

--QC28-3
val1 = Just 10
val2 = Just 5
qc28_3_mul = (*) <$> val1 <*> val2
qc28_3_div = div <$> val1 <*> val2
qc28_3_mod = mod <$> val1 <*> val2

--L28-6
main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance
