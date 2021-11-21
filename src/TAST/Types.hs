module TAST.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          deriving (Eq, Show)

type Tagged a = (FileLocation, Type, a)

data Program = Program TProgramDef
             deriving Show

type TProgramDef = Tagged ProgramDef

data ProgramDef = Main
                | LetValue TValue TProgramDef
                | LetFuncs [TFunc] TProgramDef
                deriving Show

type TFunc = Tagged Func

data Func = Func TVar [TVar] TExpr
          deriving Show

type TValue = Tagged Value

data Value = Value TVar TExpr
           deriving Show

type TExpr = Tagged Expr

data Expr = Atom TAtom
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TVar TExpr
          | Let TVar TExpr TExpr
          | If TExpr TExpr TExpr
          deriving Show

type TAtom = Tagged Atom

data Atom = Int Integer
          | Bool Bool
          | AtVar TVar
          deriving Show

type TVar = Tagged Var
