module Lambda.Types where

import Compiler.Types

data Program = Program Expr
             deriving Show

data Expr = Value Value
          | BinOp BinOp Expr Expr
          | Apply Expr Expr
          | Lambda Aloc Expr
          | Let Aloc Expr Expr
          | LetGlobal Label Expr Expr
          | LetGlobals [Label] [Expr] Expr
          | If Expr Expr Expr
          deriving Show

data Value = Int Integer
           | Bool Bool
           | Place APlace
           deriving Show
