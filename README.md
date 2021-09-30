# Description
Lambda Prime Simple is a small functional language designed as a tool for me to learn about compiler design and programming language implementation.
As such everything is subject to change including the syntax (which I don't like but still remains the best I can think of).

# Design
The compiler is designed around a Continuation Passing Style (CPS) intermediate language.
This allows for a very simple design as all statements are tail expressions and so things such as saving registers across calls just do not need to be implemented.
However this form does come with some drawbacks such as the saving of all live variables when a conditional isn't in tail position in the original source.
In the future I hope to exploit to strengths of CPS (e.g. beta reduction is always sound even when computing the arguments has side-effects) to offset these inefficiencies and even potentially eliminate them altogether.

# Current Syntax
A non recursive value definition looks like the following.

```
[ ten : Int = 10 ]

[ twenty : Int
= ten + ten
]
```

A non recursive function definition is similar.

```
[ double : Int -> Int
| n = n * 2
]

[ add : Int -> Int -> Int
| a b = a + b
]
```

Recursive definitions may only contain immediate function definitions.

```
[ letrec
  [ fac : Int -> Int
  | n = if n == 0
        then 1
        else n * fac (n - 1)
  ]
]

[ letrec
  [ even? : Int -> Bool
  | n = if n == 0
        then true
        else odd? (n - 1)
  ]
  
  [ odd? : Int -> Bool
  | n = if n == 0
        then false
        else even? (n - 1)
  ]
]
```

Non-recursive definitions may only reference previous definitions while recursive definitions may also reference other definitions in the same block (as shown above).
Therefore the following results in an undefined variable error.

```
[ add10 : Int -> Int
| n = n + ten
]

[ ten : Int = 10 ]
```

Finally the program must contain a definition for `main` which must also be of type `Int`.
Running the program consists of calculating the value of main and then printing it out.

```
[ letrec
  [ fib : Int -> Int
  | n = if n == 1
        then 0
        else if n == 2
        then 1
        else fib (n - 1) + fib (n - 2)
  ]
]

[ main : Int = fib 10 ]
```

# Things I will probably implement
These are things I want to implement, some of them would be significant enough to warrant a new language (polymorphism comes to mind).
+ A better register allocator, the current one is a bit sketchy.
	- My current idea for this is to first try to do allocation *without* spilling via graph colouring with access to all registers except for the heap register. Then if this fails (i.e. it needs to spill some registers) we perform allocation with spilling via a second chance binpacking algorithm as this both allocates the registers and generates the spilling code in a single pass.
+ An optimizer at the `CPS` stage, this will mostly perform beta reduction (function inlining) but will probably also perform eta-reduction and dead variable elimination (although the latter might be better performed at `CPSAsm`, I'm not quite sure yet).
+ An optimizer at the `Lambda` stage, this will probably perform local optimizations such as constant folding.
+ A generational garbage collector based on "CONS should not CONS its arguments, part II: Cheney on the M.T.A.".
+ Support for multi-argument functions beginning at the `Lambda` language. Then using this for the worker/wrapper optimization as layed out in "Types are Calling Conventions" and callee saved registers as shown in "Calling with Continuations".
+ User defined data types and pattern matching in the ML tradition.
+ Polymorphism through a type system based on System-F (as is traditional). The hardest part of this might actually just be the type checker as we could just assume all values are representable as a machine word and then just perform operations upon those.
+ First class continuations.
	- This might be done through exposing an implicit `return` variable in every function (think like the implicit `this` variable in objects) which is bound to the continuation of that function. This could then be used to implement `call/cc` as a derived form as the following (although it might be actually useless, it serves to demonstrate the idea).

```
[ call/cc : (Cont a -> a) -> a
| f = f return
]
```

+ A module system, although I am still thinking about the best way to do this.
	- One idea that struck me is the way in which sml/nj implements structures by just encoding them as records. It might be interesting to just take this to its logical conclusion and just make modules first class values with an actual type. This seems like it might not be too hard to implement (although I could be wrong and it could be very hard, there isn't a lot of literature on implementing module systems).

# Sources
Papers and books which helped me learn how to do all this or will probably be the source for future improvements.
+ [CPSC 411 Book](https://github.com/cpsc411/cpsc411-book)
+ [Compiling with Continuations](https://www.scinapse.io/papers/1580664042)
+ [Compiling with Continuations Continued](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf)
+ [CONS should not CONS its arguments, part II: Cheney on the M.T.A.](https://www.scinapse.io/papers/1990351075)
+ [Types are Calling Conventions](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/tacc-hs09.pdf)
+ [No-brainer CPS conversion](https://www.scinapse.io/papers/2751258746)
+ [Quality and speed in linear-scan register allocation](https://www.scinapse.io/papers/2141582038)
