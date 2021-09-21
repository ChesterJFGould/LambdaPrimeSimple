module TAST.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          deriving Eq

type Tagged val = (FileLocation, Type, val)

data Program = Program TProgramDef

type TProgramDef = Tagged ProgramDef

data ProgramDef = Main
                | LetDef Def TProgramDef
                | LetRecDefs [Def] TProgramDef

data Def = Def TVar [TVar] TBody

type TBody = Tagged Body

data Body = Body TExpr

type TExpr = Tagged Expr

data Expr = Value TValue
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TVar TExpr
          | Let TVar TExpr TExpr
          | If TExpr TExpr TExpr

type TValue = Tagged Value

data Value = Int Integer
           | Bool Bool
           | VVar TVar

type TVar = Tagged Var

data Var = Var String
         deriving (Eq, Ord, Show)
