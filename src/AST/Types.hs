module AST.Types where

import Compiler.Types

type Tagged a = (FileLocation, a)

type TTypeAnn = Tagged TypeAnn

data TypeAnn = TAInt
             | TABool
             | TAFunc TTypeAnn TTypeAnn

data Program = Program ProgramDef

type TProgramDef = Tagged ProgramDef

data ProgramDef = Main
                | LetDef TDef TProgramDef
                | LetRecDefs [TDef] TProgramDef

type TDef = Tagged Def

data Def = Def Var TypeAnn [Var] TBody

type TBody = Tagged Body

data Body = Body Expr

type TExpr = Tagged Expr

data Expr = Value TValue
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TVar TypeAnn TExpr
          | Let TVar TExpr TExpr
          | If TExpr TExpr TExpr

type TValue = Tagged Value

data Value = Int Integer
           | Bool Bool
           | VVar TVar

type TVar = Tagged Var

data Var = Var String
