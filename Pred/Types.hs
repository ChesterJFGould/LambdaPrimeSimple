module Pred.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          deriving Show

type Tagged val = (Type, val)

data Program = Program TExpr
             deriving Show

type TExpr = Tagged Expr

data Expr = Value TValue
          | NumOp NumOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TAloc TExpr
          | Let TAloc TExpr TExpr
          | LetGlobal TLabel TExpr TExpr
          | LetGlobals [TLabel] [TExpr] TExpr
          | If Pred TExpr TExpr
          deriving Show

data Pred = RelOp RelOp TExpr TExpr
          deriving Show

type TValue = Tagged Value

data Value = Int Integer
           | Bool Bool
           | Place APlace
           deriving Show

type TAloc = Tagged Aloc

type TLabel = Tagged Label
