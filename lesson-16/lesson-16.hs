import Prelude hiding (pi)
--L16-1
-- struct author_name {
--   char* first_name;
--   char* last_name;
-- };

-- struct book {
--   author_name author;
--   char* isbn;
--   char* title;
--   int year_published;
--   double price;
-- };

--L16-2
-- data AuthorName = AuthorName String String
-- data Book = Book AuthorName String String Int Double

--L16-3
-- data Book = Book {
--   author :: AuthorName,
--   isbn :: String,
--   title :: String,
--   year :: Int,
--   price :: Double
-- }

--QC16-1
data AuthorName = AuthorName {
  firstName :: String,
  lastName :: String
}

--QC16-2
-- ex. Java
-- class Car {}
-- class Spoiler {}
-- class SportsCar extends Car {
--   Spoiler rear;
-- }
-- ex. Haskell
-- data SportsCar = SportsCar Car Spoiler

--L16-9
type FirstName = String
type LastName = String
type MiddleName = String
-- data Name = Name FirstName LastName
--           | NameWithMiddle FirstName MiddleName LastName

--L16-10
-- data Creator = AuthorCreator Author | ArtistCreator Artist

--L16-11
-- data Author = Author Name

--L16-12
-- data Artist = Person Name | Band String

--L16-13
-- data Name = Name FirstName LastName
--           | NameWithMiddle FirstName MiddleName LastName
--           | TwoInitialsWithLast Char Char LastName

--L16-14
hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

--L16-15
-- data Name = Name FirstName LastName
--           | NameWithMiddle FirstName MiddleName LastName
--           | TwoInitialsWithLast Char Char LastName
--           | FirstNameWithTwoInits FirstName Char Char

--L16-16
data Book = Book {
  author    :: Creator,
  isbn      :: String,
  bookTitle :: String,
  bookYear  :: Int,
  bookPrice :: Double
}

--L16-17
data VinylRecord = VinylRecord {
  artist    :: Creator,
  recordTitle :: String,
  recordYear  :: Int,
  recordPrice :: Double
}

--L16-18
-- data StoreItem = BookItem Book | RecordItem VinylRecord

--L16-19
data CollectibleToy = CollectibleToy {
  name        :: String,
  description :: String,
  toyPrice    :: Double
}

--L16-20
-- data StoreItem = BookItem Book
--                | RecordItem VinylRecord
--                | ToyItem CollectibleToy

--L16-1
-- price :: StoreItem -> Double
-- price (BookItem v) = bookPrice v
-- price (RecordItem v) = recordPrice v
-- price (ToyItem v) = toyPrice v

--QC16-3
data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

data Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char deriving (Show)

madeBy :: StoreItem -> String
madeBy (BookItem v) = show (author v)
madeBy (RecordItem v) = show (artist v)
madeBy _ = "unknown"

momotarou :: StoreItem
momotarou = BookItem Book {
  author    = hpLovecraft,
  isbn      = "AB0123456789XY",
  bookTitle = "momotarou",
  bookYear  = 2022,
  bookPrice = 1000
}

momotarouMadeBy :: String
momotarouMadeBy = madeBy momotarou

--Q16-1
data Pamphlet = Pamphlet {
  title         :: String,
  pamphLetDesc  :: String,
  contact       :: String
}

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet

aMoviePamphlet :: StoreItem
aMoviePamphlet = PamphletItem Pamphlet {
  title         = "a movie",
  pamphLetDesc  = "This is a movie.",
  contact       = "1-2-3 Shinjuku, Tokyo, Japan"
}

price :: StoreItem -> Double
price (BookItem v) = bookPrice v
price (RecordItem v) = recordPrice v
price (ToyItem v) = toyPrice v
price (PamphletItem _) = 0.0

--Q16-2
pi :: Double
pi = 3.14
data Circle = Circle {
  radius :: Double
}

data Square = Square {
  side :: Double
}

data Rectangle = Rectangle {
  width   :: Double,
  height  :: Double
}

data Shape  = ShapeCircle Circle
            | ShapeSquare Square
            | ShapeRectangle Rectangle

circumference :: Shape -> Double
circumference (ShapeCircle v) = radius v * 2 * pi
circumference (ShapeSquare v) = side v * 4
circumference (ShapeRectangle v) = width v * 2 + height v * 2

area :: Shape -> Double
area (ShapeCircle v) = radius v ^ 2 * pi
area (ShapeSquare v) = side v ^ 2
area (ShapeRectangle v) = width v * height v

aCircle :: Shape
aCircle = ShapeCircle Circle {
  radius = 5
}

aSquare :: Shape
aSquare = ShapeSquare Square {
  side = 4
}

aRectangle :: Shape
aRectangle = ShapeRectangle Rectangle {
  width = 5,
  height = 4
}
