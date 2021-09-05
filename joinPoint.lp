[ intId : Int -> Int
| n = n
]

[ main : Int
= let x = 10
  in intId (if x == x then 1 else 0)
]
