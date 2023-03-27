import qualified Data.Map as Map

newtype Box a = Box a deriving Show

--L18-1
wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

--QC18-1
--wrap (Box 'a')の型は、Box (Box Char)

--L18-2
data Triple a = Triple a a a deriving Show

--L18-3
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

--L18-4
type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

--L18-5
type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

--L18-6
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

--L18-7
toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

--L18-8
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

--QC18-2
--transformは、変換前後の型変数が同じでなければならない
--mapは、変換前後の型変数が同じでなくてもよい

--L18-9
data List a = Empty
            | Cons a (List a)
            deriving Show
--L18-10
builtinEx1 :: [Int]
builtinEx1 = 1 : 2: 3 : []

ourListEx1 :: List Int
ourListEx1 =  Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c' : 'a' : 't' : []

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

--L18-11
ourMap :: (a -> b) -> List a -> List b
ourMap f Empty = Empty
ourMap f (Cons h t) = Cons (f h) (ourMap f t)

--L18-12 タプルの定義
-- data (,) a b = (,) a b

--L18-13
itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

--L18-14
itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

--QC18-3
--コンパイルエラーになる。
--error: ? No instance for (Fractional Int) arising from the literal ‘12.4’

--QC18-4
--(,,)のカインドは、* -> * -> * -> *
-- ghci> :kind (,,)  
-- (,,) :: * -> * -> * -> *

--L18-15
data Organ  = Heart
            | Brain
            | Kidney
            | Spleen
            deriving (Show, Eq)

--L18-16
organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

--L18-17
ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

--L18-18
pairs = [(2, Heart), (7, Heart), (13, Brain), (14, Spleen), (21, Spleen), (24, Kidney)]

--L18-19
organPairs :: [(Int, Organ)]
organPairs = zip ids organs

--L18-20
organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs
