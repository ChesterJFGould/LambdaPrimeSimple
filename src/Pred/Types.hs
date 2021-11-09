module Pred.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type

type Tagged a = (Type, a)

data Program = Program TExpr

type TExpr = Tagged Expr

data Expr = Value TValue
          | NumOp NumOp
          | Apply TExpr TExpr
          | Lambda TAloc TExpr
          | Let TAloc TExpr TExpr
          | LetRec [TAloc] [TExpr] TExpr
          | If TPred TExpr TExpr

type TPred = Tagged Pred

data Pred = RelOp RelOp TExpr TExpr

type TValue = Tagged Value

data Value = Int Integer
           | Bool Bool
           | VAloc TAloc

type TAloc = Tagged Aloc
