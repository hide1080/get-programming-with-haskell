a = 10
add4 z =
  (\y ->
    (\x -> y + x + z + a) 1
  ) 2