--Q29-3
purchasedBeers :: [Int]
purchasedBeers = [6, 12]

remainingBeers :: [Int]
remainingBeers = (\ num -> num - 4) <$> purchasedBeers

tonightFriends :: [Int]
tonightFriends = [2, 3]

totalParticipants :: [Int]
totalParticipants = (+ 2) <$> tonightFriends

expectedBeers :: [Int]
expectedBeers = [3, 4]

totalBeers :: [Int]
totalBeers = pure (*) <*> totalParticipants <*> expectedBeers

beersToPurchase :: [Int]
beersToPurchase = pure (-) <*> totalBeers <*> remainingBeers
