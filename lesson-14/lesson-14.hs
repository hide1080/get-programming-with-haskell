-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

--QC14-1
-- instance Show SixSidedDie where
--   show S1 = "I"
--   show S2 = "II"
--   show S3 = "III"
--   show S4 = "IV"
--   show S5 = "V"
--   show S6 = "VI"

--L14-3
instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

--L14-4
-- show :: SixSidedDie -> String
-- show S1 = "one"
-- show S2 = "two"
-- show S3 = "three"
-- show S4 = "four"
-- show S5 = "five"
-- show S6 = "six"

--L14-5
-- data TwoSidedDie = One | Two
-- show :: TwoSidedDie -> String
-- show One = "one"
-- show Two = "two"

--L14-6
-- instance Eq SixSidedDie where
--   (==) S6 S6 = True
--   (==) S5 S5 = True
--   (==) S4 S4 = True
--   (==) S3 S3 = True
--   (==) S2 S2 = True
--   (==) S1 S1 = True
--   (==) _ _ = False

--QC14-2
-- Minimal complete definition

-- properFraction

-- Methods

-- properFraction :: Integral b => a -> (b, a)

--L14-7
-- instance Ord SixSidedDie where
--   compare S6 S6 = EQ
--   compare S6 _ = GT
--   compare _ S6 = LT
--   compare S5 S5 = EQ
--   compare S5 _ = GT
--   compare _ S5 = LT
--QC14-3
  -- compare S4 S4 = EQ
  -- compare S4 _ = GT
  -- compare _ S4 = LT

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

--L14-12
-- • The constructor of a newtype must have exactly one field but ‘Name’ has two
-- newtype Name = Name (String, String) (String, String) deriving (Show, Eq)
-- • A newtype must have exactly one constructor, but ‘Name’ has two
-- newtype Name = Name (String, String) | Nama deriving (Show, Eq)

newtype Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [
    Name ("Emil", "Cioran"),
    Name ("Eugene", "Thacker"),
    Name ("Friedrich", "Hietzsche")
  ]

--Q14-1
data Number = One | Two | Three deriving Enum

instance Eq Number where
  (==) n1 n2 = fromEnum n1 == fromEnum n2

instance Ord Number where
  compare n1 n2 = compare (fromEnum n1) (fromEnum n2)

--Q14-2
data FiveSidedDie = First | Second | Third | Fourth | Fifth deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
