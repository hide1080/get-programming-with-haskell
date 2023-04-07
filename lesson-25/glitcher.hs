import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad

--L25-4
intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 255

--L25-5
intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

--L25-6
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes =
  mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

--L25-7
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

--L25-9
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

--L25-10
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

--L25-3,11
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   imageFile <- BC.readFile fileName
--   -- glitched <- randomReplaceByte imageFile
--   glitched <- randomSortSection imageFile
--   -- let glitched = imageFile --QC25-2
--   let glitchedFileName = mconcat ["glitched_", fileName]
--   BC.writeFile glitchedFileName glitched
--   print "all done"

--L25-12
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   imageFile <- BC.readFile fileName
--   glitched1 <- randomReplaceByte imageFile
--   glitched2 <- randomSortSection glitched1
--   glitched3 <- randomReplaceByte glitched2
--   glitched4 <- randomSortSection glitched3
--   glitched5 <- randomReplaceByte glitched4
--   let glitchedFileName = mconcat ["glitched_", fileName]
--   BC.writeFile glitchedFileName glitched5
--   print "all done"

--L25-13
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   imageFile <- BC.readFile fileName
--   glitched <- foldM (\ bytes f -> f bytes)
--     imageFile
--     [
--       randomReplaceByte,
--       randomSortSection,
--       randomReplaceByte,
--       randomSortSection,
--       randomReplaceByte
--     ]
--   let glitchedFileName = mconcat ["glitched_", fileName]
--   BC.writeFile glitchedFileName glitched
--   print "all done"

--QC25-4
glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [
    randomReplaceByte,
    randomSortSection,
    randomeReverseBytes,
    randomReplaceByte,
    randomSortSection,
    randomeReverseBytes,
    randomReplaceByte
  ]

--Q25-2
reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse target

randomeReverseBytes :: BC.ByteString -> IO BC.ByteString
randomeReverseBytes bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, (bytesLength - sectionSize))
  return (reverseSection start sectionSize bytes)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- foldM (\ bytes f -> f bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"
