module AST.Types where

import Compiler.Types

data CheckError = WithPos FileLocation String
                | WithoutPos String

type Tagged a = (FileLocation, a)

type TTypeAnn = Tagged TypeAnn

data TypeAnn = TAInt
             | TABool
             | TAFunc TTypeAnn TTypeAnn
             deriving Show

data Program = Program TProgramDef
             deriving Show

type TProgramDef = Tagged ProgramDef

data ProgramDef = Main
                | LetValue TValue TProgramDef
                | LetFuncs [TFunc] TProgramDef
                deriving Show

type TValue = Tagged Value

data Value = Value TVar TTypeAnn TExpr
           deriving Show

type TFunc = Tagged Func

data Func = Func TVar TTypeAnn [TVar] TExpr
          deriving Show

type TExpr = Tagged Expr

data Expr = Atom TAtom
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TVar TTypeAnn TExpr
          | Let TVar TExpr TExpr
          | If TExpr TExpr TExpr
          deriving Show

type TAtom = Tagged Atom

data Atom = Int Integer
          | Bool Bool
          | AtVar TVar
          deriving Show

type TVar = Tagged Var
