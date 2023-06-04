--Q28-2
type LatLon = (Double, Double)

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

haversineIO :: IO LatLon -> IO LatLon -> IO Double
haversineIO v1 v2 = haversine <$> v1 <*> v2

haversineMaybe :: Maybe LatLon -> Maybe LatLon -> Maybe Double
haversineMaybe v1 v2 = haversine <$> v1 <*> v2
