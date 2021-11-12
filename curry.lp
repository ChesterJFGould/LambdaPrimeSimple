fun add : Int -> Int -> Int
  | a b = a + b

val add10 : Int -> Int = add 10

val main : Int = add10 20 + add 10 20
