--L31-3
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

main_ :: IO ()
main_ = getLine >>=
  (\ name ->
    (\ statement ->
    putStrLn statement) (helloPerson name))
