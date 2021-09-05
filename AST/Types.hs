module AST.Types where

import Compiler.Types

data FileLocation = FileLocation { file :: String
                                 , line :: Int
                                 , column :: Int
                                 }

instance Show FileLocation where
         show (FileLocation _ line column) = unwords [ "(" ++ show line, ":", show column ++ ")" ]

data CheckError = WithPos FileLocation String
                | WithoutPos String

type Tagged val = (FileLocation, val)

type TTypeAnn = Tagged TypeAnn

data TypeAnn = TAInt
             | TABool
             | TAFunc TTypeAnn TTypeAnn
             deriving Show

data Program = Program TProgramDef
             deriving Show

type TProgramDef = Tagged ProgramDef

data ProgramDef = Main
                | LetDef TDef TProgramDef
                | LetRecDefs [TDef] TProgramDef
                deriving Show

type TDef = Tagged Def

data Def = Def TVar TTypeAnn [TVar] TBody
         deriving Show

type TBody = Tagged Body

data Body = Body TExpr
          deriving Show

type TExpr = Tagged Expr

data Expr = Value TValue
          | BinOp BinOp TExpr TExpr
          | Apply TExpr TExpr
          | Lambda TVar TTypeAnn TExpr
          | Let TVar TExpr TExpr
          | If TExpr TExpr TExpr
          deriving Show

type TValue = Tagged Value

data Value = Int Integer
           | Bool Bool
           | VVar TVar
           deriving Show

type TVar = Tagged Var

data Var = Var String
         deriving (Eq, Ord, Show)
