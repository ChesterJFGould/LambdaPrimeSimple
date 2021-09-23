[ letrec
  [ fac : Int -> Int
  | n = if n == 0
        then 1
        else n * fac (n - 1)
  ]
]

[ main : Int = fac 10 ]
