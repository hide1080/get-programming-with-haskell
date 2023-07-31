--L34-5
module Main where

head :: Monoid a => [a] -> a
head (x : xs) = x
head [] = mempty

example :: [[Int]]
example = []

-- このコードをGHCIでテストするには
-- モジュール名をMain以外にするか
-- 以下のダミーコードを追加する必要がある。

main :: IO ()
main = return ()
