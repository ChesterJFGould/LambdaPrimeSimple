module CPSRegisters.Types where

import Compiler.Types

data Program = Program [GlobalTuple] [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Expr
          deriving Show

data Expr = Set Reg RTriv Expr
          | NumOp NumOp Reg Reg Expr
          | MRef Reg RPlace Int Expr
          | MSet RPlace Int RPlace Expr
          | If Pred Expr Expr
          | Jump RPlace
          deriving Show

data Pred = RelOp RelOp Reg Reg
          deriving Show
