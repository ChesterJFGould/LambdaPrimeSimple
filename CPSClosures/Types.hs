module CPSClosures.Types where

import Compiler.Types

data Program = Program [Def] Body
             deriving Show

data Def = Func Label Aloc Aloc Aloc Body -- Env, Cont, Arg
         | Cont Label Aloc Aloc Body -- Env, Arg
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = CallFunc APlace APlace APlace APlace -- Func, Env, Cont, Arg
          | CallCont APlace APlace APlace -- Cont, Env, Arg
          | Let Aloc Value Expr
          | LetGlobalTuple Label [APlace] Expr
          | If Pred Expr Expr
          deriving Show

data Pred = RelOp RelOp Aloc Aloc
          deriving Show

data Value = Int Integer
           | Bool Bool
           | Tuple [APlace]
           | TupleRef APlace Int
           | NumOp NumOp Aloc Aloc
           deriving Show
