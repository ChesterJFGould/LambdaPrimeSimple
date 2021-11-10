fun add : Int -> Int -> Int
  | a b = a + b

def add10 : Int -> Int = add 10

def main : Int = add10 20 + add 10 20
