module CPS.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          | TCont Type
          deriving Show

type Tagged a = (Type, a)

data Program = Program Expr
             deriving Show

data Expr = CallFunc TAloc TAloc TAloc
          | CallCont TAloc TAloc
          | Let TAloc Atom Expr
          | LetCont TAloc Cont Expr
          | LetRecFuncs [TAloc] [Func] Expr
          | If Pred Expr Expr
          deriving Show

data Cont = Cont TAloc Expr
          deriving Show

data Func = Func TAloc TAloc Expr
          deriving Show

data Pred = RelOp RelOp TAloc TAloc
          deriving Show

data Atom = Int Integer
          | Bool Bool
          | AtLabel TLabel
          | NumOp NumOp TAloc TAloc
          deriving Show

type TAloc = Tagged Aloc

type TLabel = Tagged Label
