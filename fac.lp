fun fac : Int -> Int
  | n = if n < 1
        then 1
        else n * fac (n - 1)

val main : Int = fac 10
