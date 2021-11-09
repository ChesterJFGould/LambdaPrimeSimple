module CPSRegisters.Types where

import Compiler.Types

data Program = Program [Block] Expr

data Block = Block Label Expr

data Expr = Set Reg RTriv Expr
          | NumOp NumOp Reg Reg Expr
          | MRef Reg Reg Int Expr
          | MSet Reg Int RPlace Expr
          | If Pred Expr Expr
          | Jump RPlace

data Pred = RelOp RelOp Reg Reg
