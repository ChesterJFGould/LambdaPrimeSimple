module Pred.Types where

import Compiler.Types

data Program = Program Expr
             deriving Show

data Expr = Value Value
          | NumOp NumOp Expr Expr
          | Apply Expr Expr
          | Lambda Aloc Expr
          | Let Aloc Expr Expr
          | LetGlobal Label Expr Expr
          | LetGlobals [Label] [Expr] Expr
          | If Pred Expr Expr
          deriving Show

data Pred = RelOp RelOp Expr Expr
          deriving Show

data Value = Int Integer
           | Bool Bool
           | Place APlace
           deriving Show
