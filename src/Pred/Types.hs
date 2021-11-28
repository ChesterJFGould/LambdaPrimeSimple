module Pred.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type

type Tagged a = (Type, a)

data Program = Program TExpr

type TExpr = Tagged Expr

data Expr = Atom TAtom
          | NumOp NumOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TAloc TExpr
          | Let TAloc TExpr TExpr
          | LetRec [TAloc] [TExpr] TExpr
          | If Pred TExpr TExpr

data Pred = RelOp RelOp TExpr TExpr

type TAtom = Tagged Atom

data Atom = Int Integer
          | Bool Bool
          | AtAloc TAloc

type TAloc = Tagged Aloc
