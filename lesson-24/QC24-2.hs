import System.IO

--QC24-2
main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  hasSecondLine <- hIsEOF helloFile
  secondLine <-
    if not hasSecondLine
    then hGetLine helloFile
    else return ""
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStr goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"
