myDrop n aList =
  reverse (take (length aList - n) (reverse aList))

--QC8-1
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs

finiteCycle (first:rest) = first:rest ++ [first]

myCycle (first:rest) = first:myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n =
  if even n
  then 1 + collatz (n `div` 2)
  else 1 + collatz (n * 3 + 1)

--Q8-1
myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]

--Q8-2
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y n = fastFib (x + y) x (n - 1)

myFib n = fastFib 1 1 n

fnByAll f [] = []
fnByAll f (x:xs) = f x : fnByAll f xs
