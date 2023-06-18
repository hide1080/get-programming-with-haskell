-- --L30-10
-- echo :: IO ()
-- echo = getLine >>= putStrLn

-- main :: IO ()
-- main = echo

--L30-11
echoVerbose :: IO ()
echoVerbose =
  putStrLn "Enter a String an we'll echo it!"
  >> getLine >>= putStrLn

main :: IO ()
main = echoVerbose
