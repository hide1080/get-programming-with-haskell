--Q29-1
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func fa = pure func <*> fa
