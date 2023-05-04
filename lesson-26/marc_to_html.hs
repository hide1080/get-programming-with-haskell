--L26-1
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Maybe
import Control.Monad (when)
import Data.ByteString.Builder (byteString)
--Data.ByteString.Char8をインポートしていない。
--理由は、Unicodeデータを操作するときにUnicodeテキストとASCIIテキストを取り違えたくないから

--L26-2, L26-4
type Author = T.Text
type Title = T.Text
type Html = T.Text

--L26-3
data Book = Book {
  author :: Author,
  title  :: Title
} deriving Show

--L26-6
book1 :: Book
book1 = Book {
  -- title = "The Conspiracy Against the Human Race",
  -- author = "Ligotti, Thomas"
  title = "走れメロス",
  author = "太宰治"
}
book2 :: Book
book2 = Book {
  -- title = "A Short History of Decay",
  -- author = "Cioran, Emil"
  title = "羅生門",
  author = "芥川龍之介"
}
book3 :: Book
book3 = Book {
  -- title = "The Tears of Eros",
  -- author = "Bataille, Georges"
  title = "吾輩は猫である",
  author = "夏目漱石"
}
myBooks :: [Book]
myBooks = [book1, book2, book3]

--L26-5
bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

--L26-7
booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat [
    "<html>\n",
    "<head><title>books</title>",
    "<meta charset='utf-8'/>",
    "</head>\n",
    "<body>\n",
    booksHtml,
    "\n</body>\n",
    "</html>"
  ]
  where booksHtml = (mconcat . (map bookToHtml)) books

--MARCレコードの構造
--Leader      レコードの先頭24バイト。ベースレコードが始まるアドレスなどの情報を持つ
--Directory   Leaderの直後に配置。ベースレコードにどのようなフィールドがあるかなどの情報を持つ
--Base Record Directoryの次に配置

--L26-9, L26-15, L26-19, L26-24
type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString
type FieldText = T.Text

--L26-21
data FieldMetadata = FieldMetadata {
  tag :: T.Text,      --3バイト
  fieldLength :: Int, --レコードの長さ、4バイト
  fieldStart :: Int   --レコードの開始位置、5バイト
} deriving Show

--L26-10, L26-19
leaderLength :: Int
leaderLength = 24
dirEntryLength :: Int
dirEntryLength = 12

--L26-11
getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

--L26-12
rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . TE.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

--L26-13
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream

--L26-14
allRecords :: B.ByteString -> [MarcRecordRaw] 
allRecords marcStream
  =
    if marcStream == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

--L26-16
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where remainder = B.drop 12 leader

--L26-17
getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength - 1)

--L26-18
getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record
  =
    B.take directoryLength dirAndBaserecord
  where
    directoryLength = getDirectoryLength record
    dirAndBaserecord = B.drop leaderLength record

--L26-20
splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory
  =
    if directory == B.empty
    then []
    else nextEntry : splitDirectory restEntries
  where
    (nextEntry, restEntries) = B.splitAt dirEntryLength directory

--L26-22
makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where
    (theTag, rest) = B.splitAt 3 entry
    textTag = TE.decodeUtf8 theTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

--L26-23
getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

--L26-25
getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetaData = TE.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetaData) baseRecord
    byteStringValue = B.take (fieldLength fieldMetaData) baseAtEntry

--L26-26
fieldDelimiter :: Char
fieldDelimiter = toEnum 31

--L26-27
titleTag :: T.Text
titleTag = "245"
titleSubfield :: Char
titleSubfield = 'a'
authorTag :: T.Text
authorTag = "100"
authorSubfield :: Char
authorSubfield = 'a'

--L26-28
lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record
  =
    if null results
    then Nothing
    else Just (head results)
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

--L26-29
lookupSubfield
  :: Maybe FieldMetadata
  -> Char
  -> MarcRecordRaw
  -> Maybe T.Text
lookupSubfield Nothing subfield record
  = Nothing
lookupSubfield (Just fieldMetaData) subfield record
  =
    if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record fieldMetaData
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) subfields

--L26-30
lookupValue
  :: T.Text -> Char -> MarcRecordRaw ->Maybe T.Text
lookupValue
  aTag subfield record
  = lookupSubfield entryMetadata subfield record
  where
    entryMetadata = lookupFieldMetadata aTag record

--L26-31
lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield
lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

--L26-32
marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

--L26-33
pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs
  =
    map (\ (title, author) ->
      Book {
        title = fromJust title,
        author = fromJust author
      }
    ) justPairs
  where
    justPairs
      =
        filter (\ (title, author) ->
          isJust title && isJust author
        ) pairs

--L26-34
processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

--L26-8
-- main :: IO ()
-- main = do
--     let html = booksToHtml myBooks
--     TIO.putStrLn html
--     TIO.writeFile "books.i.html" html

-- main :: IO ()
-- main
--   = do
--     marcData <- B.readFile "sample.mrc"
--     let marcRecords = allRecords marcData
--     print (length marcRecords)

main :: IO ()
main
  = do
    marcData <- B.readFile "sample.mrc"
    let processed = processRecords 50 marcData
    TIO.writeFile "books.i.html" processed
    -- print processed
