module CPSAsm.Types where

import Compiler.Types

data Program = Program [GlobalTuple] [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body [Reg] Expr
          deriving Show

data Expr = Set Mloc MTriv Expr
          | NumOp NumOp Mloc Mloc Expr
          | MRef Mloc MPlace Int Expr
          | MSet MPlace Int MPlace Expr
          | If Pred Expr Expr
          | Jump MPlace [Reg]
          deriving Show

data Pred = RelOp RelOp Mloc Mloc
          deriving Show
