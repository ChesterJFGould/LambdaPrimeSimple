# Pipeline
```
Text - Parse into AST > AST
     - Check program is correct > AST
     - Lower into simple form > Lambda
     - Lower predicates > Pred
     - Split globals into tuples and funcs > Globals
     - Perform CPS transform > CPS
     - Perform closure transform > CPSClosure
     - Lower into pseudo-assembly > CPSAsm
     - Calculate free-in for each op > CPSFree
     - Assign registers > CPSRegisters
     - Lower into symbol assembly > S64
     - Lower into real assembly > x64
     - Nasm into binary > Binary
```
