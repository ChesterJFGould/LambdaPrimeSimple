[ zero : Int = 0 ]

[ zero? : Int -> Bool
| n = n == zero
]

[ letrec
  [ even? : Int -> Bool
  | n = if zero? n
        then true
        else odd? (n - 1)
  ]
  [ odd? : Int -> Bool
  | n = if zero? n
        then false
        else even? (n - 1)
  ]
]

[ boolToInt : Bool -> Int
| b = if b
      then 1
      else 0
]

[ main : Int
= boolToInt (even? 110)
]
