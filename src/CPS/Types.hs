module CPS.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          | TCont Type

type Tagged a = (Type, a)

data Program = Program Expr

data Expr = CallFunc TAloc TAloc TAloc
          | CallCont TAloc TAloc
          | Let TAloc Value Expr
          | LetCont TAloc Cont Expr
          | LetFunc TAloc Func Expr
          | LetRecFuncs [TAloc] [Func] Expr
          | If Pred Expr Expr

data Cont = Cont TAloc Expr

data Func = Func TAloc TAloc Expr

data Pred = RelOp RelOp TAloc TAloc

data Value = Int Integer
           | Bool Bool
           | NumOp NumOp TAloc TAloc

type TAloc = Tagged Aloc
