module Globals.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          | TTuple [Type]
          deriving Show

type Tagged val = (Type, val)

data Program = Program TExpr
             deriving Show

type TExpr = Tagged Expr

data Expr = Value TValue
          | NumOp NumOp TExpr TExpr
          | Apply TExpr TExpr
          | TupleRef TExpr Int
          | Let TAloc TExpr TExpr
          | LetFunc TAloc Func TExpr
          | LetGlobalTuple TLabel [TExpr] TExpr
          | LetGlobalFunc TLabel Func TExpr
          | LetGlobalFuncs [TLabel] [Func] TExpr
          | If Pred TExpr TExpr
          deriving Show

data Func = Func TAloc TExpr
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
