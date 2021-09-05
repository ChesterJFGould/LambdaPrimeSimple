module Globals.Types where

import Compiler.Types

data Program = Program Expr
             deriving Show

data Expr = Value Value
          | NumOp NumOp Expr Expr
          | Apply Expr Expr
          | TupleRef Expr Int
          | Let Aloc Expr Expr
          | LetFunc Aloc Func Expr
          | LetGlobalTuple Label [Expr] Expr
          | LetGlobalFunc Label Func Expr
          | LetGlobalFuncs [Label] [Func] Expr
          | If Pred Expr Expr
          deriving Show

data Func = Func Aloc Expr
          deriving Show

data Pred = RelOp RelOp Expr Expr
          deriving Show

data Value = Int Integer
           | Bool Bool
           | Place APlace
           deriving Show
