--Q27-2
newtype Box a = Box a deriving Show
instance Functor Box where
  fmap f (Box x) = Box (f x)
  -- (<$>) = fmap

myBox :: Box Int
myBox = Box 1

wrap :: Int -> Box Int
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

wrapped :: Box (Box Int)
wrapped = fmap wrap myBox
