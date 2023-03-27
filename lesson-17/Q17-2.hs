newtype Events = Events [String]
newtype Probs = Probs [Double]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
  where combiner x y = mconcat [x, "-", y]

instance Semigroup Events where
  (<>) =  combineEvents

instance Monoid Events where
  mempty = Events []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mempty = Probs []
