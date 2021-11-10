module TAST.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type

type Tagged a = (FileLocation, Type, a)

data Program = Program TProgramDef

type TProgramDef = Tagged ProgramDef

data ProgramDef = Main
                | LetValue Value TProgramDef
                | LetFuncs [Func] TProgramDef

data Func = Func TVar [TVar] TBody

data Value = Value TVar TBody

type TBody = Tagged Body

data Body = Body TExpr

type TExpr = Tagged Expr

data Expr = Atom TAtom
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TVar TExpr
          | Let TVar TExpr TExpr
          | If TExpr TExpr TExpr

type TAtom = Tagged Atom

data Atom = Int Integer
          | Bool Bool
          | VVar TVar

type TVar = Tagged Var

data Var = Var String
