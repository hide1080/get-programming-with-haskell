counter x =
  let x = x + 1
  in
    let x = x + 1
    in
      x

counterLambda =
  (
    \x ->
      (
        \x -> x + 1
      ) x + 1
  )

counterLambdaAnswer x =
  (\x -> x + 1)
  ((\x -> x + 1) ((\x -> x) x))