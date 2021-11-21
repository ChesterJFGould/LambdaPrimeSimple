fun app10 : (Int -> Int) -> Int
  | f = f 10

def main : Int = app10 λ x : Int. x