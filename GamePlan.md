# Changes
+ Globals are just compiled as normal variables.
	+ Mostly simplifies compilation.
		+ We just need to add a mutually recursive tuple form in the cps closure stage.
			+ This is trivial and always sound.
			+ Desirable anyways as we could eventually expose it at the top level.
	+ Simplifies garbage collection, we only need to collect the arguments.
	+ Gives us mutually recursive local functions for free.
	+ Forces our handling of local variables to be efficient, which is needed for the module system.
+ Potentially use second-chance binpacking for register allocation.
	+ Should probably just use graph colouring at first so we can get the globals thing working.
	+ Maybe use the method in Compiling with Continuations?
		+ Would need to add the offset thing they use.
	+ Confusion before was not treating explicitly used registers as normal alocs which happen to be assigned to themselves.

# Pipeline
```
Text - Parse into AST > AST
     - Check program is correct > TAST
     - Lower into simple form > Lambda
     - Lower predicates > Pred
     - Perform CPS transform > CPS
     - Perform closure transform > CPSClosure
     - Lower into pseudo-assembly > CPSAsm
     - Calculate free-in for each op > CPSFree
     - Assign registers > CPSRegisters
     - Lower into symbolic assembly > S64
     - Lower into textual assembly > x64
     - Nasm into binary > Binary
```

# TODO
+ Use GADTs to prove that LetRec only contains lambdas
	+ Provide an empty type for each constructor (`data Lambda`) and then parameterize the `Expr` type over that
+ Maybe eliminate `Body` types in AST and TAST
+ It's conceivable that we could move the translation from AST to TAST and therefore all the checking into the parser.
	+ This way we could have really nice and unified error message not only for parse errors, but for type, undefined, etc. errors.
	+ It could probably also be done in such a way that it doesn't really complicate things at all.
