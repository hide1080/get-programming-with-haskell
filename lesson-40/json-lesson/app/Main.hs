module Main (main) where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

--L40-23
main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

--L40-3, 4
data Book = Book {
    title   :: T.Text
  , author  :: T.Text
  , year    :: Int
} deriving (Show, Generic)

--L40-5
instance FromJSON Book
instance ToJSON Book

--L40-6
myBook :: Book
myBook = Book {
    author  = "hide1"
  , title   = "Studying Haskell"
  , year    = 2023
}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

--L40-7
rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"hide1\",\"title\":\"Studying Haskell 2\",\"year\":2024}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

--L40-8
wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"hide2\",\"title\":\"Studying Haskell\",\"year\":2023}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

--L40-9
sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\":123}"

--L40-10, 11
data ErrorMessage = ErrorMessage {
    message :: T.Text
  , errorCode :: Int -- すでに関数として存在するのでerrorという名前は使用できない
} deriving (Show)

--L40-12
instance FromJSON ErrorMessage where
  parseJSON (Object v)
    = ErrorMessage <$> v .: "message" <*> v .: "error"

--L40-13
exampleMessage :: Maybe T.Text
exampleMessage = Just "Opps"
exampleError :: Maybe Int
exampleError = Just 123
exampleErrorMessage :: Maybe ErrorMessage
exampleErrorMessage = ErrorMessage <$> exampleMessage <*> exampleError

--L40-14
sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

--L40-15
instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object ["message" .= message, "error" .= errorCode]

--L40-16
anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

--L40-18
data NOAAResult = NOAAResult {
    uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Double
  , resultid :: T.Text
} deriving Show

--L40-19
instance FromJSON NOAAResult where
  parseJSON (Object v)
    = NOAAResult
      <$> v .: "uid"
      <*> v .: "mindate"
      <*> v .: "maxdate"
      <*> v .: "name"
      <*> v .: "datacoverage"
      <*> v .: "id"

-- Q40-1
instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name datacoverage resultid) =
    object [
        "uid" .= uid
      , "mindate" .= mindate
      , "maxdate" .= maxdate
      , "name" .= name
      , "datacoverage" .= datacoverage
      , "resultid" .= resultid
    ]

--L40-20
data Resultset = Resultset {
    offset :: Int
  , count :: Int
  , limit :: Int
} deriving (Show, Generic)

instance FromJSON Resultset
instance ToJSON Resultset -- Q40-1

--L40-21
newtype Metadata = Metadata {
    resultset :: Resultset
} deriving (Show, Generic)

instance FromJSON Metadata
instance ToJSON Metadata -- Q40-1

--L40-22
data NOAAResponse = NOAAResponse {
    metadata :: Metadata
  , results :: [NOAAResult]
} deriving (Show, Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse -- Q40-1

--L40-23
printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  forM_ results $ \ result -> do
    let dataName = name result
    print dataName

--QC40-1
-- 変換に失敗しないから

--QC40-2
data Name = Name {
    firstName :: T.Text
  , lastName  :: T.Text
} deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name

myName :: Name
myName = Name {
    firstName  = "hide1"
  , lastName    = "080"
}

myNameJSON :: BC.ByteString
myNameJSON = encode myName

rawMyNameJSON :: BC.ByteString
rawMyNameJSON = "{\"firstName\":\"hide1\",\"lastName\":\"080\"}"

myNameFromJSON :: Maybe Name
myNameFromJSON = decode rawMyNameJSON

--QC40-3
data NonGenericName = NonGenericName {
    firstName_ :: T.Text
  , lastName_  :: T.Text
} deriving (Show)

instance FromJSON NonGenericName where
  parseJSON (Object v) =
    NonGenericName <$> v .: "firstName_" <*> v .: "lastName_"

--QC40-4
instance ToJSON NonGenericName where
  toJSON (NonGenericName fn ln) =
    object ["firstName" .= fn, "lastName" .= ln]

-- Q40-2
data IntList = EmptyList | Cons Int IntList
  deriving (Show, Generic)

instance ToJSON IntList
instance FromJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 $ Cons 3 EmptyList