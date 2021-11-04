[ a : Int = 10 ]

[ b : Int = 20 ]

[ foo : Int = a + b ]

[ addFoo : Int -> Int
| n = n + foo
]

[ main : Int
= addFoo foo
]
