--Q30-2
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf ma = mf >>= (\ f -> ma >>= (\ a -> return (f a)))
