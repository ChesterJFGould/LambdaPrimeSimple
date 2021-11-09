module CPSClosure.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          | TCont Type
          | TTuple [Type]

type Tagged a = (Type, a)

data Program = Program [Def] Expr

data Def = Func Label TAloc TAloc TAloc Expr
         | Cont Label TAloc TAloc Expr

data Expr = CallFunc TAPlace TAPlace TAPlace TAPlace
          | CallCont TAPlace TAPlace TAPlace
          | Let TAloc Value Expr
          | LetRecTuples TAloc [TAPlace] Expr
          | If Pred Expr Expr

data Pred = RelOp RelOp TAloc TAloc

data Value = Int Integer
           | Bool Bool
           | NumOp NumOp TAloc TAloc
           | Tuple [TAPlace]
           | TupleRef TAloc Int

type TAloc = Tagged Aloc

type TAPlace = Tagged APlace
