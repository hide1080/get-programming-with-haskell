fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
  putStrLn "Calculate Fibonacci numbers."
  putStr "Please enter a number: "
  numStr <- getLine
  let num = read numStr
  print (fib num)
