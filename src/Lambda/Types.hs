module Lambda.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          deriving Show

type Tagged a = (Type, a)

data Program = Program TExpr
             deriving Show

type TExpr = Tagged Expr

data Expr = Atom TAtom
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TAloc TExpr
          | Let TAloc TExpr TExpr
          | LetRec [TAloc] [TExpr] TExpr
          | If TExpr TExpr TExpr
          deriving Show

type TAtom = Tagged Atom

data Atom = Int Integer
          | Bool Bool
          | AtAloc TAloc
          deriving Show

type TAloc = Tagged Aloc
