import System.IO

--L24-2
-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   hClose helloFile
--   putStrLn "done!"

--L24-3
-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   firstLine <- hGetLine helloFile
--   putStrLn firstLine
--   secondLine <- hGetLine helloFile
--   goodbyeFile <- openFile "goodbye.txt" WriteMode
--   hPutStrLn goodbyeFile secondLine
--   hClose helloFile
--   hClose goodbyeFile
--   putStrLn "done!"

--L24-4
main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine <- hIsEOF helloFile
  firstLine <-
    if not hasLine
    then hGetLine helloFile
    else return "empty"
  putStrLn "done!"
