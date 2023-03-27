import System.Environment

--L22-1, L22-2, L22-3, L22-4
main :: IO ()
main = do
  args <- getArgs
  -- map putStrLn args  --コンパイルできない。Couldn't match type ‘[]’ with ‘IO’
  -- mapM putStrLn args --まだできない。Couldn't match type ‘[()]’ with ‘()’
  mapM_ putStrLn args
