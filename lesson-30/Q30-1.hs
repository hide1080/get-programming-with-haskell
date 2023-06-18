--Q30-1
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func ma = ma >>= (\ a -> return (func a))
