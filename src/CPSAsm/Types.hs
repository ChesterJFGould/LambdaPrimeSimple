module CPSAsm.Types where

import Compiler.Types

data Program = Program [Block] Expr

data Block = Block Label Expr

data Expr = Set Mloc MTriv Expr
          | NumOp NumOp Mloc Mloc Expr
          | MRef Mloc Mloc Int Expr
          | MSet Mloc Int MPlace Expr
          | If Pred Expr Expr
          | Jump MPlace [Reg]

data Pred = RelOp RelOp Mloc Mloc
