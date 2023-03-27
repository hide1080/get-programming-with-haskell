--QC7-1
-- 日常的に行っている再帰プロセスの例
-- プログラミング、アプリケーション、システムの設計

--7.3

myGCD a b =
  if remainder == 0
  then b
  else myGCD b remainder
  where
    remainder = a `mod` b

--QC7-2
-- myGCD関数において、a>b なのか、 a<b なのかは問題にならない

sayAmountV1 n =
  case n of
    1 -> "one"
    2 -> "two"
    _ -> "a bunch"

sayAmountV2 1 = "one"
sayAmountV2 2 = "two"
sayAmountV2 n = "a bunch "

isEmpty [] = True
isEmpty _ = False

myHead (x:xs) = x
myHead [] = error "No head for empty list"

--QC7-3
myTail (x:xs) = xs

--Q7-1
myTail_Q7_1 (x:xs) = xs
myTail_Q7_1 [] = []

--Q7-2
myGCD_Q7_2 a 0 = a
myGCD_Q7_2 a b = myGCD_Q7_2 b (a `mod` b)
