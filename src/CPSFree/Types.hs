module CPSFree.Types where

import Compiler.Types

import qualified Data.Set.Ordered as O

type Free = O.OSet Mloc

type Tagged a = (Free, a)

data Program = Program [Block] TExpr

data Block = Block Label TExpr

type TExpr = Tagged Expr

data Expr = Set Mloc MTriv TExpr
          | NumOp NumOp Mloc Mloc TExpr
          | MRef Mloc Mloc Int TExpr
          | MSet Mloc Int MPlace TExpr
          | If Pred Expr TExpr
          | Jump MPlace [Reg]

data Pred = RelOp RelOp Mloc Mloc
