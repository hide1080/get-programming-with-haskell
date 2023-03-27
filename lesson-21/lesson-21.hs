--L21-1
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  let name = getLine
  name2 <- name
  let statement = helloPerson name2
  putStrLn statement

--QC21-1
--ユーザーの入力を取得する行は、name <- getLine
--想定される入力の型は、IO String

--QC21-2
--mainの最後の行がgetLineでもOk ⇒ 誤り
--mainの型は、IO ()である
--一方、getLineの型は、IO Stringなのでうまくいかない

--QC21-3
--getLineの型は、IO Stringである
--StringをとるhelloPersonへの適用は不可能
--helloPerson :: String -> String
