--Q27-1
newtype Box a = Box a deriving Show
instance Functor Box where
  fmap f (Box x) = Box (f x)
  -- (<$>) = fmap

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap (replicate n)
