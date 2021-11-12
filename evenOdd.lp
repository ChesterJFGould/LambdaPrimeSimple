fun even? : Int -> Bool
  | n = if n == 0
        then True
        else odd? (n - 1)
and odd? : Int -> Bool
  | n = if n == 0
        then false
        else even? (n - 1)

fun boolToInt : Bool -> Int
  | n = if n then 1 else 0

val main : Int = boolToInt  (even? 10)
