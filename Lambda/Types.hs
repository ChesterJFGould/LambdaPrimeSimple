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

data Expr = Value TValue
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda Aloc TExpr
          | Let Aloc TExpr TExpr
          | LetGlobal Label TExpr TExpr
          | LetGlobals [Label] [TExpr] TExpr
          | If TExpr TExpr TExpr
          deriving Show

type TValue = Tagged Value

data Value = Int Integer
           | Bool Bool
           | Place APlace
           deriving Show
