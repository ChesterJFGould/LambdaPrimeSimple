module CPSClosures.Types where

import Compiler.Types

data Type = TInt
          | TBool
          | TFunc Type Type
          | TCont Type
          | TTuple [Type]
          | TClosure Type
          deriving (Eq, Ord, Show)

type Tagged val = (Type, val)

data Program = Program [Def] Body
             deriving Show

data Def = Func TLabel TAloc TAloc TAloc Body -- Env, Cont, Arg
         | Cont TLabel TAloc TAloc Body -- Env, Arg
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = CallFunc TAPlace TAPlace TAPlace TAPlace -- Func, Env, Cont, Arg
          | CallCont TAPlace TAPlace TAPlace -- Cont, Env, Arg
          | Let TAloc Value Expr
          | LetGlobalTuple TLabel [TAPlace] Expr
          | If Pred Expr Expr
          deriving Show

data Pred = RelOp RelOp TAloc TAloc
          deriving Show

data Value = Int Integer
           | Bool Bool
           | VLabel TLabel
           | Tuple [TAPlace]
           | TupleRef TAPlace Int
           | NumOp NumOp TAloc TAloc
           deriving Show

type TAloc = Tagged Aloc

type TLabel = Tagged Label

type TAPlace = Tagged APlace
