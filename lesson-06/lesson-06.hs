takeLast n aList =
  reverse (take n (reverse aList))

ones n =
  take n (cycle [1])

assignToGroups n aList =
  zip groups aList
  where groups = cycle [1..n]

--Q6-1
repeat_Q6_1 v = cycle [v]

--Q6-2
subseq start end lst =
  drop start (take end lst)

--Q6-3
inFirstHalf el lst =
  elem el (take (length lst `div` 2) lst)

isPalindrome word = word == reverse word
