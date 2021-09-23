module CPS.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          | TCont Type
          | TTuple [Type]
          deriving Show

type Tagged val = (Type, val)

data Program = Program Expr
             deriving Show

data Expr = CallFunc TAPlace TAPlace TAPlace
          | CallCont TAPlace TAPlace
          | Let TAloc TValue Expr
          | LetCont TAloc Cont Expr
          | LetFunc TAloc Func Expr
          | LetGlobalTuple TLabel [TAPlace] Expr
          | LetGlobalFunc TLabel Func Expr
          | LetGlobalFuncs [TLabel] [Func] Expr
          | If Pred Expr Expr
          deriving Show

data Func = Func TAloc TAloc Expr
          deriving Show

data Cont = Cont TAloc Expr
          deriving Show

data Pred = RelOp RelOp TAloc TAloc
          deriving Show

type TValue = Tagged Value

data Value = Int Integer
           | Bool Bool
           | VLabel TLabel
           | TupleRef TAPlace Int
           | NumOp NumOp TAloc TAloc
           deriving Show

type TAPlace = Tagged APlace

type TAloc = Tagged Aloc

type TLabel = Tagged Label
