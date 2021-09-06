module CPS.Types where

import Compiler.Types

data Program = Program Expr
             deriving Show

data Expr = CallFunc APlace APlace APlace
          | CallCont APlace APlace
          | Let Aloc Value Expr
          | LetCont Aloc Cont Expr
          | LetFunc Aloc Func Expr
          | LetGlobalTuple Label [APlace] Expr
          | LetGlobalFunc Label Func Expr
          | LetGlobalFuncs [Label] [Func] Expr
          | If Pred Expr Expr
          deriving Show

data Func = Func Aloc Aloc Expr
          deriving Show

data Cont = Cont Aloc Expr
          deriving Show

data Pred = RelOp RelOp Aloc Aloc
          deriving Show

data Value = Int Integer
           | Bool Bool
           | VLabel Label
           | TupleRef APlace Int
           | NumOp NumOp Aloc Aloc
           deriving Show
