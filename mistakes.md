# Mistakes and improvements to be made
+ The way TypeCheck handles errors is not great, maybe we should add a third layer of monad transformers for the maybe.
+ Globals.Lower should prove that alocs in NumOp and RelOp are always lowered into alocs instead of just assuming it.
+ The way tuples are lowered is messy
+ The cps transform should use the method outlined in "No-Brainer CPS Conversion" <- Maybe, I'm not entirely convinced its actually a better transform and not just merging an inliner with the cps transform
+ Look for types which can be pulled out into `Compiler.Types`
+ The il types should bear proof that letrecs have to be lambdas?
+ I don't know about the syntax...
+ In the Global -> CPS lowering stage maybe we shouldn't have typed alocs in the environment as we guarentee that things won't magically change type so we can just use the type of the replaced aloc.
